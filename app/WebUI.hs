{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Use ?~" #-}

module WebUI (runWebUI) where

import Control.Applicative (asum)
import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar, threadDelay)
import Control.Concurrent.STM (STM, TChan, TMVar, atomically, newTMVar, readTChan, takeTMVar, tryPutTMVar)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execStateT)
import Data.Binary.Builder qualified as Binary
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Lazy.Char8 qualified as LB
import Data.Coerce (coerce)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Function ((&))
import Data.List (find, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text.Lazy qualified as LT
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Experiments
  ( DesignSource (..),
    Experiment (..),
    ExperimentId (..),
    ExperimentProgress (..),
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
import Safe (headMay, tailSafe)
import Text.Blaze.Html.Renderer.Pretty qualified as H
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx (hxExt, hxGet, hxPushUrl, hxSwap, hxTarget, hxTrigger)
import Text.Blaze.Htmx.ServerSentEvents (sseConnect)
import Util (diffTimeHMSFormat, foreverThread, modifyMVarPure_, mwhen, whenJust, whenM)
import Web.Scotty (ActionM, Parsable (..), addHeader, get, header, html, middleware, next, param, params, raw, scotty, setHeader, status, stream)

data ExperimentSequenceInfo = ExperimentSequenceInfo
  { sequenceId :: ExperimentSequenceId,
    experiments :: Map ExperimentId ExperimentInfo
  }
  deriving (Generic, Eq, Show)

data ExperimentInfo = ExperimentInfo
  { experiment :: Experiment,
    result :: Maybe ExperimentResult
  }
  deriving (Generic, Eq, Show)

data WebUIState = WebUIState
  { runningExperiments :: Map ExperimentSequenceId ExperimentSequenceInfo,
    interestingExperiments :: Map ExperimentSequenceId ExperimentSequenceInfo,
    uninterestingExperiments :: Map ExperimentSequenceId ExperimentSequenceInfo,
    experimentsSem :: Semaphore,
    totalRunCount :: Int,
    totalRunCountSem :: Semaphore,
    liveUpdates :: Bool,
    runTime :: NominalDiffTime
  }
  deriving (Generic)

newWebUIState :: IO WebUIState
newWebUIState = do
  experimentsSem <- atomically newSemaphore
  totalRunCountSem <- atomically newSemaphore
  return
    WebUIState
      { interestingExperiments = Map.empty,
        runningExperiments = Map.empty,
        uninterestingExperiments = Map.empty,
        experimentsSem,
        totalRunCount = 0,
        totalRunCountSem,
        liveUpdates = True,
        runTime = 0
      }

at2 :: ExperimentSequenceId -> ExperimentId -> Lens' (Map ExperimentSequenceId ExperimentSequenceInfo) (Maybe ExperimentInfo)
at2 sequenceId experimentId =
  at sequenceId % non (ExperimentSequenceInfo sequenceId Map.empty) % #experiments % at experimentId

handleProgress :: MVar WebUIState -> ExperimentProgress -> IO ()
handleProgress stateVar progress = do
  state <- readMVar stateVar

  modifyMVar_ stateVar . execStateT $ case progress of
    ExperimentStarted sequenceId experiment -> do
      #runningExperiments % at2 sequenceId experiment.experimentId .= Just (ExperimentInfo experiment Nothing)
      liftIO . atomically $ signalSemaphore state.experimentsSem
    ExperimentCompleted sequenceId result -> do
      -- FIXME: Report error if experiment does not exist
      -- FIXME: Report error if experiment still has active runs
      mExperiment <- use (#runningExperiments % at2 sequenceId result.experimentId)
      whenJust mExperiment $ \runningExperiment -> do
        let completedExperiment = runningExperiment {result = Just result}
        if isInteresting completedExperiment
          then #interestingExperiments % at2 sequenceId result.experimentId .= Just completedExperiment
          else #uninterestingExperiments % at2 sequenceId result.experimentId .= Just completedExperiment
        #runningExperiments % at2 sequenceId result.experimentId .= Nothing
        liftIO . atomically $ signalSemaphore state.experimentsSem
        liftIO . atomically $ signalSemaphore state.totalRunCountSem
        #totalRunCount %= (+ 1)
    ExperimentSequenceCompleted sequenceId -> do
      #runningExperiments % at sequenceId .= Nothing
      liftIO . atomically $ signalSemaphore state.experimentsSem

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

  get "/experiments/:sequenceId/:experimentId" $ do
    sequenceId <- coerce @UUIDParam @ExperimentSequenceId <$> param "sequenceId"
    experimentId <- coerce @UUIDParam @ExperimentId <$> param "experimentId"
    state <- liftIO (readMVar stateVar)
    isHtmxRequest <- (Just "true" ==) <$> header "HX-Request"

    let mExperimentInfo :: Maybe ExperimentInfo =
          asum . map (view (at2 sequenceId experimentId)) $
            [ state.runningExperiments,
              state.interestingExperiments,
              state.uninterestingExperiments
            ]

    case mExperimentInfo of
      (Just experimentInfo)
        | isHtmxRequest -> blazeHtml (experimentInfoBlock experimentInfo)
        | otherwise -> blazeHtml (experimentInfoPage state experimentInfo)
      _ -> next

  get "/events" $ do
    state <- liftIO (readMVar stateVar)
    status status200
    setHeader "Content-Type" "text/event-stream"
    stream . eventStreamFromIO . atomically $ do
      waitForSemaphore state.experimentsSem
      pure [EventStreamEvent {event = "experiment-list", data_ = "update"}]

  get "/experiments" $ do
    whenM (paramExists "toggle-updates") $
      liftIO (modifyMVarPure_ stateVar toggleLiveUpdates)

    whenM (paramExists "prune-uninteresting") $
      liftIO (modifyMVarPure_ stateVar pruneUninterestingSequences)

    state <- liftIO (readMVar stateVar)
    blazeHtml (experimentList state)

  get "/" $ do
    state <- liftIO (readMVar stateVar)
    blazeHtml (indexPage state)

toggleLiveUpdates :: WebUIState -> WebUIState
toggleLiveUpdates = #liveUpdates %~ not

pruneUninterestingSequences :: WebUIState -> WebUIState
pruneUninterestingSequences state =
  state
    & #uninterestingExperiments
      %~ Map.filter
        ( \sequenceInfo ->
            sequenceInfo.sequenceId `Map.member` state.runningExperiments
              || sequenceInfo.sequenceId `Map.member` state.interestingExperiments
        )

paramExists :: LT.Text -> ActionM Bool
paramExists p = isJust . find ((== p) . fst) <$> params

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
    H.header "Equifuzz"
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
  infoBoxNoTitle . table $
    [ ["UUID", H.toHtml (show info.experiment.experimentId.uuid)],
      ["Expected Result", if info.experiment.expectedResult then "Equivalent" else "Non-equivalent"],
      ["Comparison Value", H.text info.experiment.comparisonValue],
      case info.result of
        Just result ->
          [ "Actual Result",
            case result.proofFound of
              Just True -> "Equivalent"
              Just False -> "Non-equivalent"
              Nothing -> "Inconclusive"
          ]
        _ -> []
    ]

  infoBox "Description" (H.pre $ H.text ("\n" <> info.experiment.longDescription))

  infoBox "Design" (H.pre $ H.text ("\n" <> info.experiment.design.source))

  whenJust (info ^? #result %? #counterExample % _Just) $ \counterExample ->
    infoBox
      "Counter-example"
      (H.pre $ H.text ("\n" <> counterExample))

  whenJust info.result $ \result ->
    infoBox
      "Full output"
      (H.pre $ H.text ("\n" <> result.fullOutput))

experimentList :: WebUIState -> Html
experimentList state = H.div
  H.! A.id "experiment-list-area"
  H.! mwhen
    state.liveUpdates
    (hxTrigger "sse:experiment-list" <> hxGet "/experiments" <> hxSwap "outerHTML")
  $ do
    H.div H.! A.id "experiment-list" $ do
      experimentSubList "Running" state.runningExperiments
      experimentSubList "Interesting" state.interestingExperiments
      experimentSubList "Uninteresting" state.uninterestingExperiments
    table
      [ ["Total runs", H.toHtml (show state.totalRunCount)],
        ["Running time", H.toHtml (diffTimeHMSFormat state.runTime)],
        ["Live Updates", toggleUpdatesButton]
      ]
    pruneUninterestingButton
  where
    toggleUpdatesButton =
      H.button
        H.! hxGet "/experiments?toggle-updates=1"
        H.! hxTarget "closest #experiment-list-area"
        H.! hxSwap "outerHTML"
        $ if state.liveUpdates
          then "ON"
          else "OFF"

    pruneUninterestingButton =
      H.button
        H.! hxGet "/experiments?prune-uninteresting=1"
        H.! hxTarget "closest #experiment-list-area"
        H.! hxSwap "outerHTML"
        $ "Prune uninteresting"

    experimentSubList title experiments =
      infoBoxWithSideTitle
        title
        (H.toHtml (length experiments))
        (mapM_ experimentSequenceList experiments)

    experimentSequenceList :: ExperimentSequenceInfo -> Html
    experimentSequenceList sequenceInfo =
      let items =
            sort
              [ (size, experimentId)
                | ExperimentInfo {experiment = Experiment {size, experimentId}} <-
                    Map.elems sequenceInfo.experiments
              ]
       in H.div H.! A.class_ "experiment-list-item" $ do
            H.span H.! A.class_ "experiment-list-uuid" $ do
              H.toHtml (show sequenceInfo.sequenceId.uuid)

              whenJust (headMay items) $ \(size, experimentId) ->
                H.div $
                  experimentLink sequenceInfo.sequenceId experimentId size

              when (length items > 1) $
                H.details $ do
                  H.summary "Other runs"
                  H.ul H.! A.class_ "experiment-list-run-list" $ do
                    sequence_
                      [ H.li (experimentLink sequenceInfo.sequenceId experimentId size)
                        | (size, experimentId) <- tailSafe items
                      ]

    experimentLink :: ExperimentSequenceId -> ExperimentId -> Int -> Html
    experimentLink sequenceId experimentId size =
      H.a
        H.! hxTarget "#run-info"
        H.! hxSwap "outerHTML"
        H.! hxPushUrl "true"
        H.! hxGet [i|/experiments/#{sequenceId ^. #uuid}/#{experimentId ^. #uuid}|]
        H.! A.href [i|/experiments/#{sequenceId ^. #uuid}/#{experimentId ^. #uuid}|]
        $ H.toHtml ("Size " <> show size)

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

infoBoxWithSideTitle :: Html -> Html -> Html -> Html
infoBoxWithSideTitle title count body =
  H.div H.! A.class_ "info-box" $ do
    H.h2 H.! A.class_ "info-box-title" $ do
      H.span title
      H.span count
    H.div H.! A.class_ "info-box-content" $
      body

blazeHtml :: Html -> ActionM ()
blazeHtml = html . LT.pack . H.renderHtml

newtype UUIDParam = UUIDParam UUID

instance Parsable UUIDParam where
  parseParam txt = case UUID.fromText (LT.toStrict txt) of
    Just uuid -> Right (UUIDParam uuid)
    Nothing -> Left ("'" <> txt <> "' is not a valid UUID")

--------------------------- Event Streams -----------------------------------------

type EventStream = StreamingBody

data EventStreamEvent = EventStreamEvent
  { event :: LB.ByteString,
    data_ :: LB.ByteString
  }

eventStreamEventToBuilder :: EventStreamEvent -> Binary.Builder
eventStreamEventToBuilder EventStreamEvent {event, data_} =
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
  Nothing -> False

makeFieldLabelsNoPrefix ''WebUIState
makeFieldLabelsNoPrefix ''ExperimentSequenceInfo
makeFieldLabelsNoPrefix ''ExperimentInfo
