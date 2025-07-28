{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use ?~" #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module WebUI (runWebUI, ExperimentProgress (..)) where

import Codec.Archive.Zip (Archive (..), Entry, toEntry)
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Concurrent.STM (STM, TChan, TMVar, atomically, newTMVar, readTChan, takeTMVar, tryPutTMVar)
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execStateT, modify)
import Data.Binary (encode)
import Data.Binary.Builder qualified as Binary
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Lazy.Char8 qualified as LB
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Function ((&))
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Experiments (
  Experiment (..),
  ExperimentId (..),
  ExperimentResult (..),
  ExperimentSequenceId (..),
 )
import GHC.Generics (Generic)
import Meta
import Network.HTTP.Types (status200)
import Network.Wai (StreamingBody)
import Network.Wai.Middleware.Gzip (def, gzip)
import Optics (At (at), Lens', makeFieldLabelsNoPrefix, non, use, view, (%), (%?), (%~), (.~), (^.), (^?), _Just)
import Optics.State.Operators ((%=), (.=))
import Text.Blaze.Html.Renderer.Pretty qualified as H
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx (hxExt, hxGet, hxPushUrl, hxSwap, hxTarget, hxTrigger)
import Text.Blaze.Htmx.ServerSentEvents (sseConnect)
import Text.Printf (printf)
import Util (diffTimeHMSFormat, foreverThread, modifyMVarPure_, mwhen, whenJust, whenJustM, whenM)
import Web.Scotty (
  ActionM,
  Parsable (..),
  addHeader,
  finish,
  get,
  header,
  html,
  middleware,
  next,
  notFound,
  pathParam,
  queryParamMaybe,
  queryParams,
  raw,
  redirect,
  scotty,
  setHeader,
  status,
  stream,
  text,
 )

data ExperimentProgress
  = ExperimentStarted ExperimentSequenceId Experiment
  | ExperimentCompleted ExperimentSequenceId ExperimentResult
  | ExperimentSequenceCompleted ExperimentSequenceId
  deriving (Show)

data ExperimentSequenceInfo = ExperimentSequenceInfo
  { sequenceId :: ExperimentSequenceId
  , experiments :: Map ExperimentId ExperimentInfo
  , isRunning :: Bool
  }
  deriving (Generic, Show)

instance Eq ExperimentSequenceInfo where
  l == r = l.sequenceId == r.sequenceId && (Map.keys l.experiments == Map.keys r.experiments)

data ExperimentInfo = ExperimentInfo
  { experiment :: Experiment
  , result :: Maybe ExperimentResult
  }
  deriving (Generic, Show)

data WebUIState = WebUIState
  { sequences :: Map ExperimentSequenceId ExperimentSequenceInfo
  , experimentsSem :: Semaphore
  , totalRunCount :: Int
  , totalRunCountSem :: Semaphore
  , liveUpdates :: Bool
  , autoPrune :: Bool
  , runTime :: NominalDiffTime
  }
  deriving (Generic)

newWebUIState :: IO WebUIState
newWebUIState = do
  experimentsSem <- atomically newSemaphore
  totalRunCountSem <- atomically newSemaphore
  return
    WebUIState
      { sequences = Map.empty
      , experimentsSem
      , totalRunCount = 0
      , totalRunCountSem
      , liveUpdates = True
      , autoPrune = True
      , runTime = 0
      }

at2 :: ExperimentSequenceId -> ExperimentId -> Lens' (Map ExperimentSequenceId ExperimentSequenceInfo) (Maybe ExperimentInfo)
at2 sequenceId experimentId =
  at sequenceId
    % non
      ( ExperimentSequenceInfo
          { sequenceId
          , experiments = Map.empty
          , isRunning = True
          }
      )
    % #experiments
    % at experimentId

isHtmxRequest :: ActionM Bool
isHtmxRequest = (Just "true" ==) <$> header "HX-Request"

handleProgress :: MVar WebUIState -> ExperimentProgress -> IO ()
handleProgress stateVar progress = do
  state <- readMVar stateVar

  modifyMVar_ stateVar . execStateT $ do
    case progress of
      ExperimentStarted sequenceId experiment -> do
        #sequences % at2 sequenceId experiment.experimentId .= Just (ExperimentInfo experiment Nothing)
        liftIO . atomically $ signalSemaphore state.experimentsSem
      ExperimentCompleted sequenceId result -> do
        -- FIXME: Report error if experiment does not exist
        -- FIXME: Report error if experiment still has active runs
        mExperiment <- use (#sequences % at2 sequenceId result.experimentId)
        whenJust mExperiment $ \runningExperiment -> do
          let completedExperiment =
                runningExperiment
                  & #result .~ Just result
                  & #experiment % #extraInfos %~ Map.union result.extraInfos
          #sequences % at2 sequenceId result.experimentId .= Just completedExperiment
          liftIO . atomically $ signalSemaphore state.experimentsSem
          liftIO . atomically $ signalSemaphore state.totalRunCountSem
          #totalRunCount %= (+ 1)
      ExperimentSequenceCompleted sequenceId -> do
        #sequences % at sequenceId % _Just % #isRunning .= False
        liftIO . atomically $ signalSemaphore state.experimentsSem
    when state.autoPrune $
      modify pruneUninterestingSequences

scottyServer :: MVar WebUIState -> IO ()
scottyServer stateVar = scotty 8888 $ do
  middleware (gzip def)

  get "/resources/style.css" $ do
    addHeader "Content-Type" "text/css"
    raw $
      LB.fromStrict $(embedFile =<< makeRelativeToProject "resources/style.css")

  get "/resources/htmx.min.js.gz" $ do
    addHeader "Content-Type" "application/javascript"
    addHeader "Content-Encoding" "gzip"
    raw $
      LB.fromStrict $(embedFile =<< makeRelativeToProject "resources/htmx.min.js.gz")

  get "/resources/sse.js.gz" $ do
    addHeader "Content-Type" "application/javascript"
    addHeader "Content-Encoding" "gzip"
    raw $
      LB.fromStrict $(embedFile =<< makeRelativeToProject "resources/sse.js.gz")

  get "/experiments/:sequenceId/:experimentId/report.org" $ do
    ExperimentSequenceIdParam sequenceId <- pathParam "sequenceId"
    ExperimentIdParam experimentId <- pathParam "experimentId"
    state <- liftIO (readMVar stateVar)

    case state.sequences ^. at2 sequenceId experimentId of
      (Just experimentInfo) -> text (LT.fromStrict $ experimentReportOrgMode experimentInfo)
      _ -> next

  get "/experiments/:sequenceId/:experimentId/report.zip" $ do
    ExperimentSequenceIdParam sequenceId <- pathParam "sequenceId"
    ExperimentIdParam experimentId <- pathParam "experimentId"
    state <- liftIO (readMVar stateVar)

    case experimentReportZip =<< state.sequences ^. at2 sequenceId experimentId of
      Just archive -> do
        setHeader "Content-Type" "application/zip"
        raw (encode archive)
      _ -> next

  get "/experiments/:sequenceId/:experimentId" $ do
    ExperimentSequenceIdParam sequenceId <- pathParam "sequenceId"
    ExperimentIdParam experimentId <- pathParam "experimentId"
    state <- liftIO (readMVar stateVar)
    htmx <- isHtmxRequest

    case state.sequences ^. at2 sequenceId experimentId of
      Just experimentInfo
        | htmx -> blazeHtml (experimentInfoBlock experimentInfo)
        | otherwise -> blazeHtml (experimentInfoPage state experimentInfo)
      Nothing -> next

  get "/events" $ do
    state <- liftIO (readMVar stateVar)
    status status200
    setHeader "Content-Type" "text/event-stream"
    stream . eventStreamFromIO . atomically $ do
      waitForSemaphore state.experimentsSem
      pure [EventStreamEvent{event = "experiment-list", data_ = "update"}]

  get "/experiments" $ do
    whenM (queryParamExists "toggle-updates") $
      liftIO (modifyMVarPure_ stateVar (toggle #liveUpdates))

    whenM (queryParamExists "prune-uninteresting") $
      liftIO (modifyMVarPure_ stateVar pruneUninterestingSequences)

    whenM (queryParamExists "toggle-auto-prune") $ do
      -- Trigger a prune as well, otherwise the user has to wait for the next
      -- update
      liftIO (modifyMVarPure_ stateVar pruneUninterestingSequences)
      liftIO (modifyMVarPure_ stateVar (toggle #autoPrune))

    whenJustM (queryParamMaybe "delete-sequence") $ \(ExperimentSequenceIdParam sequenceId) -> do
      liftIO (modifyMVarPure_ stateVar (deleteSequence sequenceId))

    whenJustM (queryParamMaybe "delete-experiment-sequence") $ \(ExperimentSequenceIdParam sequenceId) ->
      whenJustM (queryParamMaybe "delete-experiment") $ \(ExperimentIdParam experimentId) ->
        liftIO (modifyMVarPure_ stateVar (deleteExperiment sequenceId experimentId))

    state <- liftIO (readMVar stateVar)
    blazeHtml (experimentList state)

  get "/" $ do
    state <- liftIO (readMVar stateVar)
    blazeHtml (indexPage state)

  -- This lets you just hit F5 on the browser that was looking at the
  -- application before restarting and get the new instance that has just
  -- started
  notFound $
    isHtmxRequest >>= \case
      False -> redirect "/"
      True -> finish

deleteExperiment :: ExperimentSequenceId -> ExperimentId -> WebUIState -> WebUIState
deleteExperiment sequenceId experimentId =
  #sequences % at2 sequenceId experimentId .~ Nothing

deleteSequence :: ExperimentSequenceId -> WebUIState -> WebUIState
deleteSequence sequenceId =
  #sequences %~ Map.delete sequenceId

toggle :: Lens' s Bool -> s -> s
toggle = (%~ not)

pruneUninterestingSequences :: WebUIState -> WebUIState
pruneUninterestingSequences =
  #sequences
    %~ Map.filter
      ( \ExperimentSequenceInfo{isRunning, experiments} ->
          isRunning || any isInteresting experiments
      )
    . Map.map (#experiments %~ Map.filter isInteresting)

queryParamExists :: T.Text -> ActionM Bool
queryParamExists p = isJust . find ((== p) . fst) <$> queryParams

runWebUI :: TChan ExperimentProgress -> IO ()
runWebUI progressChan = do
  stateVar <- newMVar =<< newWebUIState

  foreverThread "UI Handler" $ do
    progress <- atomically (readTChan progressChan)
    handleProgress stateVar progress

  startTime <- getCurrentTime
  foreverThread "UI timer" $ do
    currentTime <- getCurrentTime
    modifyMVarPure_ stateVar $
      #runTime .~ diffUTCTime currentTime startTime
    threadDelay 1_000_000 -- 1s
  scottyServer stateVar

htmlBase :: Html -> Html
htmlBase content = H.docTypeHtml $ do
  H.head $ do
    H.title "Equifuzz"
    H.link H.! A.rel "stylesheet" H.! A.href "/resources/style.css"
    H.script H.! A.src "/resources/htmx.min.js.gz" $ ""
    H.script H.! A.src "/resources/sse.js.gz" $ ""
  H.body H.! hxExt "sse" H.! sseConnect "/events" $ do
    H.main content
    H.footer $ do
      H.span ("Version " <> H.string versionName)
      H.span "© Michalis Pardalos 2023"

indexPage :: WebUIState -> Html
indexPage state =
  htmlBase $ do
    experimentList state
    H.div H.! A.id "run-info" $ pure ()

experimentInfoPage :: WebUIState -> ExperimentInfo -> Html
experimentInfoPage state info =
  htmlBase $ do
    experimentList state
    experimentInfoBlock info

experimentInfoBlock :: ExperimentInfo -> Html
experimentInfoBlock info = H.div H.! A.id "run-info" H.! A.class_ "long" $ do
  infoBoxNoTitle $ do
    table
      [ ["UUID", H.toHtml (show info.experiment.experimentId.uuid)]
      , ["Expected Result", if info.experiment.expectedResult then "Equivalent" else "Non-equivalent"]
      , case info.result of
          Just result ->
            [ "Actual Result"
            , case result.proofFound of
                Just True -> "Equivalent"
                Just False -> "Non-equivalent"
                Nothing -> "Inconclusive"
            ]
          Nothing -> []
      ]
    when (isJust info.result) $
      H.a H.! A.href [i|./#{info ^. #experiment % #experimentId % #uuid}/report.org|] $
        "Rendered (org-mode)"

  infoBox "SystemC" (H.pre $ H.text ("\n" <> info.experiment.scDesign))

  infoBox "Verilog" (H.pre $ H.text ("\n" <> info.experiment.verilogDesign))

  whenJust (info ^? #result %? #counterExample % _Just) $ \counterExample ->
    infoBox
      "Counter-example"
      (H.pre $ H.text ("\n" <> counterExample))

  whenJust info.result $ \result ->
    infoBox
      "Full output"
      (H.pre $ H.text ("\n" <> result.fullOutput))

  forM_ (Map.toList info.experiment.extraInfos) $ \(label, content) ->
    infoBox (H.text label) (H.pre $ H.text ("\n" <> content))

experimentList :: WebUIState -> Html
experimentList state = H.div
  H.! A.id "experiment-list-area"
  H.! mwhen
    state.liveUpdates
    (hxTrigger "sse:experiment-list" <> hxGet "/experiments" <> hxSwap "outerHTML")
  $ do
    let (running, notRunning) = Map.partition (view #isRunning) state.sequences
    let (interesting, justRunning) = Map.partition (any (\e -> isInteresting e && not (isRunning e)) . view #experiments) running
    H.div H.! A.class_ "flex-max-available overflow-scroll" $ do
      mapM_ sequenceBox interesting
      mapM_ sequenceBox justRunning
      H.hr
      mapM_ sequenceBox notRunning

    H.div $ do
      table
        [ ["Currently running", H.toHtml (show (length running))]
        , ["Total runs", H.toHtml (show state.totalRunCount)]
        , ["Running time", H.toHtml (diffTimeHMSFormat state.runTime)]
        ,
          [ "Amortised experiment time"
          , H.toHtml @String (printf "%.1fs" amortisedExperimentTime)
          ]
        , ["Live Updates", toggleUpdatesButton]
        , ["Auto prune", autoPruneButton]
        ]
        H.! A.class_ "flex-constant"
      pruneUninterestingButton
 where
  sequenceBox ExperimentSequenceInfo{..} =
    let
      deleteButton =
        H.button
          H.! hxGet [i|/experiments?delete-sequence=#{sequenceId ^. #uuid}|]
          H.! hxTarget "closest #experiment-list-area"
          H.! hxSwap "outerHTML"
          $ "❌"
      experimentDeleteButton experimentId =
        H.button
          H.! hxGet [i|/experiments?delete-experiment=#{experimentId ^. #uuid}&delete-experiment-sequence=#{sequenceId ^. #uuid}|]
          H.! hxTarget "closest #experiment-list-area"
          H.! hxSwap "outerHTML"
          $ "❌"
      title = do
        unless isRunning deleteButton
        H.text (UUID.toText sequenceId.uuid)
     in
      infoBox title . H.ul $ do
        forM_ experiments $ \ExperimentInfo{..} ->
          H.li $ do
            case result of
              Just _ -> experimentDeleteButton experiment.experimentId
              Nothing -> H.text "(Running)"
            experimentLink sequenceId experiment.experimentId $ do
              H.text ("Size " <> T.pack (show experiment.size))

  amortisedExperimentTime :: Float
  amortisedExperimentTime
    | state.totalRunCount == 0 = 0
    | otherwise =
        realToFrac
          (nominalDiffTimeToSeconds state.runTime / fromIntegral state.totalRunCount)

  toggleUpdatesButton =
    H.button
      H.! hxGet "/experiments?toggle-updates=1"
      H.! hxTarget "closest #experiment-list-area"
      H.! hxSwap "outerHTML"
      $ if state.liveUpdates
        then "ON"
        else "OFF"

  autoPruneButton =
    H.button
      H.! hxGet "/experiments?toggle-auto-prune=1"
      H.! hxTarget "closest #experiment-list-area"
      H.! hxSwap "outerHTML"
      $ if state.autoPrune
        then "ON"
        else "OFF"

  pruneUninterestingButton =
    H.button
      H.! hxGet "/experiments?prune-uninteresting=1"
      H.! hxTarget "closest #experiment-list-area"
      H.! hxSwap "outerHTML"
      $ "Prune uninteresting"

  experimentLink :: ExperimentSequenceId -> ExperimentId -> Html -> Html
  experimentLink sequenceId experimentId inner =
    H.a
      H.! hxTarget "#run-info"
      H.! hxSwap "outerHTML"
      H.! hxPushUrl "true"
      H.! hxGet [i|/experiments/#{sequenceId ^. #uuid}/#{experimentId ^. #uuid}|]
      H.! A.href [i|/experiments/#{sequenceId ^. #uuid}/#{experimentId ^. #uuid}|]
      $ H.toHtml inner

table :: [[Html]] -> Html
table rows = H.table $
  forM_ rows $ \row ->
    H.tr $
      forM_ row $ \content ->
        H.td content

infoBox :: Html -> Html -> Html
infoBox title body = do
  H.div H.! A.class_ "info-box" $ do
    H.h2 H.! A.class_ "info-box-title" $
      title
    H.div H.! A.class_ "info-box-content" $
      body

infoBoxNoTitle :: Html -> Html
infoBoxNoTitle body = do
  H.div H.! A.class_ "info-box" $ do
    H.div H.! A.class_ "info-box-content" $
      body

blazeHtml :: Html -> ActionM ()
blazeHtml = html . LT.pack . H.renderHtml

newtype UUIDParam = UUIDParam UUID
newtype ExperimentSequenceIdParam = ExperimentSequenceIdParam ExperimentSequenceId
  deriving (Parsable) via UUIDParam
newtype ExperimentIdParam = ExperimentIdParam ExperimentId
  deriving (Parsable) via UUIDParam

instance Parsable UUIDParam where
  parseParam txt = case UUID.fromText (LT.toStrict txt) of
    Just uuid -> Right (UUIDParam uuid)
    Nothing -> Left ("'" <> txt <> "' is not a valid UUID")

experimentReportOrgMode :: ExperimentInfo -> Text
experimentReportOrgMode (ExperimentInfo _ Nothing) = "Not yet completed"
experimentReportOrgMode (ExperimentInfo experiment (Just result)) =
  [__i|
    * Bug report
    - Expected: =#{expectedResult}=
    - Got:      =#{actualResult}=

    ** Files
    #{files}

    ** Output
    \#+begin_example
    #{fullOutput}
    \#+end_example
    |]
 where
  props =
    experiment.extraInfos <> result.extraInfos

  expectedResult :: Text
  expectedResult = if experiment.expectedResult then "equivalent" else "non-equivalent"

  actualResult :: Text
  actualResult = case result.proofFound of
    Just True -> "equivalent"
    Just False -> "non-equivalent"
    Nothing -> "failed"

  files =
    [("cpp", "spec.cpp"), ("verilog", "impl.sv"), ("tcl", "compare.tcl")]
      & map (uncurry formatFile)
      & T.intercalate "\n\n"

  formatFile :: Text -> Text -> Text
  formatFile orgOpts name = case props Map.!? name of
    Nothing -> ""
    Just txt ->
      [__i|
          =#{name}=
          \#+begin_src #{orgOpts}
          #{txt}
          \#+end_src
          |]

  fullOutput :: Text
  fullOutput = result.fullOutput

experimentReportZip :: ExperimentInfo -> Maybe Archive
experimentReportZip (ExperimentInfo _ Nothing) = Nothing
experimentReportZip info@(ExperimentInfo experiment (Just result)) =
  Just
    ( Archive
        { zEntries =
            mapMaybe fileEntry ["spec.cpp", "impl.sv", "compare.tcl"]
              <> [ toEntry "description.org" 0 (LB.fromStrict . TE.encodeUtf8 $ description)
                 , toEntry "log.txt" 0 (LB.fromStrict . TE.encodeUtf8 $ fullOutput)
                 ]
        , zSignature = Nothing
        , zComment = ""
        }
    )
 where
  props :: Map Text Text =
    experiment.extraInfos <> result.extraInfos

  description = experimentReportOrgMode info

  fileEntry :: Text -> Maybe Entry
  fileEntry name = do
    contents <- props Map.!? name
    return (toEntry (T.unpack name) 0 (LB.fromStrict $ TE.encodeUtf8 contents))

  fullOutput :: Text
  fullOutput = result.fullOutput

--------------------------- Event Streams -----------------------------------------

type EventStream = StreamingBody

data EventStreamEvent = EventStreamEvent
  { event :: LB.ByteString
  , data_ :: LB.ByteString
  }

eventStreamEventToBuilder :: EventStreamEvent -> Binary.Builder
eventStreamEventToBuilder EventStreamEvent{event, data_} =
  Binary.fromLazyByteString eventBS
 where
  eventValue :: LB.ByteString
  eventValue = eventEncode "event: " event

  eventData :: LB.ByteString
  eventData = eventEncode "data: " data_

  eventEncode :: LB.ByteString -> LB.ByteString -> LB.ByteString
  eventEncode label bs = LB.unlines [label <> line | line <- LB.lines bs]

  eventBS = eventValue <> eventData <> "\n"

eventStreamFromIO :: IO [EventStreamEvent] -> EventStream
eventStreamFromIO produceEvent send flush = do
  forever $ do
    flush
    events <- produceEvent
    forM_ events $ \event ->
      send (eventStreamEventToBuilder event)

newtype Semaphore = Semaphore (TMVar ())

newSemaphore :: STM Semaphore
newSemaphore = Semaphore <$> newTMVar ()

waitForSemaphore :: Semaphore -> STM ()
waitForSemaphore (Semaphore mvar) = takeTMVar mvar

signalSemaphore :: Semaphore -> STM ()
signalSemaphore (Semaphore mvar) = void $ tryPutTMVar mvar ()

isInteresting :: ExperimentInfo -> Bool
isInteresting info = case info.result of
  Just result -> result.proofFound /= Just info.experiment.expectedResult
  Nothing -> True -- Still running, so interesting by default

isRunning :: ExperimentInfo -> Bool
isRunning info = isNothing info.result

makeFieldLabelsNoPrefix ''WebUIState
makeFieldLabelsNoPrefix ''ExperimentSequenceInfo
makeFieldLabelsNoPrefix ''ExperimentInfo
