{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# HLINT ignore "Redundant <$>" #-}

module WebUI (WebUIState, newWebUIState, handleProgress, runWebUI) where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Concurrent.STM (STM, TMVar, atomically, isEmptyTMVar, newTMVar, retry, takeTMVar, tryPutTMVar)
import Control.Monad (forM, forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execStateT)
import Data.Binary.Builder qualified as Binary
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Lazy.Char8 qualified as LB
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.String.Interpolate (i)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Experiments (DesignSource (..), Experiment (..), ExperimentProgress (..), ExperimentResult (..), RunnerInfo)
import GHC.Generics (Generic)
import Network.HTTP.Types (status200, urlDecode, urlEncode)
import Network.Wai (StreamingBody)
import Optics (At (at), makeFieldLabelsNoPrefix, use, (%), (%?), (^.), (^?), _Just)
import Optics.State.Operators ((%=), (.=))
import Text.Blaze.Html.Renderer.Pretty qualified as H
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx (hxExt, hxGet, hxPushUrl, hxSwap, hxTarget, hxTrigger)
import Text.Blaze.Htmx.ServerSentEvents (sseConnect)
import Text.Printf (printf)
import Util (whenJust)
import Web.Scotty (ActionM, Parsable (..), addHeader, get, header, html, next, param, raw, scotty, setHeader, status, stream)

data ExperimentInfo = ExperimentInfo
  { experiment :: Experiment,
    runs :: Map RunnerInfo RunInfo
  }
  deriving (Generic)

data RunInfo
  = CompletedRun ExperimentResult
  | ActiveRun RunnerInfo
  deriving (Eq, Show, Generic)

data WebUIState = WebUIState
  { runningExperiments :: Map UUID ExperimentInfo,
    interestingExperiments :: Map UUID ExperimentInfo,
    experimentsSem :: Semaphore,
    totalRunCount :: Int,
    totalRunCountSem :: Semaphore
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
        experimentsSem,
        totalRunCount = 0,
        totalRunCountSem
      }

handleProgress :: MVar WebUIState -> ExperimentProgress -> IO ()
handleProgress stateVar progress = do
  state <- readMVar stateVar

  modifyMVar_ stateVar . execStateT $ case progress of
    NewExperiment experiment -> do
      #runningExperiments % at experiment.uuid .= Just (ExperimentInfo experiment [])
      liftIO . atomically $ signalSemaphore state.experimentsSem
    BeginRun uuid runnerInfo -> do
      -- FIXME: Report error if uuid does not exist
      #runningExperiments % at uuid %? #runs % at runnerInfo .= Just (ActiveRun runnerInfo)
    RunCompleted result -> do
      -- FIXME: Report error if uuid does not exist
      #runningExperiments % at result.uuid %? #runs % at result.runnerInfo .= Just (CompletedRun result)
      #totalRunCount %= (+ 1)
      liftIO . atomically $ signalSemaphore state.experimentsSem
      liftIO . atomically $ signalSemaphore state.totalRunCountSem
    ExperimentCompleted uuid -> do
      -- FIXME: Report error if experiment does not exist
      -- FIXME: Report error if experiment still has active runs
      mExperiment <- use (#runningExperiments % at uuid)
      whenJust mExperiment $ \experiment -> do
        #runningExperiments % at uuid .= Nothing
        #interestingExperiments
          % at uuid
          .= if shouldKeep experiment
            then Just experiment
            else Nothing

runWebUI :: MVar WebUIState -> IO ()
runWebUI stateVar = scotty 8888 $ do
  get "/resources/style.css" $
    raw $
      LB.fromStrict $(embedFile =<< makeRelativeToProject "resources/style.css")

  get "/resources/htmx.js" $ do
    addHeader "Content-Type" "application/javascript"
    raw $
      LB.fromStrict $(embedFile =<< makeRelativeToProject "resources/htmx.js")

  get "/experiments/:uuid/runs/:runnerInfo" $ do
    UUIDParam uuid <- param "uuid"
    runnerInfo <- urlDecode False <$> param "runnerInfo"
    state <- liftIO (readMVar stateVar)
    isHtmxRequest <- (Just "true" ==) <$> header "HX-Request"

    liftIO $ printf "Looking for runner %s" (show runnerInfo)

    let mExperimentInfo =
          Map.lookup uuid state.runningExperiments
            <|> Map.lookup uuid state.interestingExperiments
    let mExperiment = mExperimentInfo ^? _Just % #experiment
    let mRunInfo = mExperimentInfo ^? _Just % #runs % at (T.decodeUtf8 runnerInfo) % _Just

    case (mExperiment, mRunInfo) of
      (Just experiment, Just ri)
        | isHtmxRequest -> blazeHtml (runInfoBlock experiment ri)
        | otherwise -> blazeHtml (runInfoPage state experiment ri)
      _ -> next

  get "/events" $ do
    state <- liftIO (readMVar stateVar)
    status status200
    setHeader "Content-Type" "text/event-stream"
    stream . eventStreamFromIO . atomically $ do
      waitForSemaphore state.experimentsSem
      pure [EventStreamEvent {event = "experiment-list", data_ = "update"}]

  get "/experiments" $ do
    state <- liftIO (readMVar stateVar)

    blazeHtml (experimentList state)

  get "/" $ do
    state <- liftIO (readMVar stateVar)
    blazeHtml (indexPage state)

htmlBase :: Html -> Html
htmlBase content = H.docTypeHtml $ do
  H.head $ do
    H.title "Equifuzz"
    H.link H.! A.rel "stylesheet" H.! A.href "/resources/style.css"
    -- H.script H.! A.src "/resources/htmx.min.js.gz" $ ""
    H.script H.! A.src "/resources/htmx.js" $ ""
    H.script H.! A.src "https://unpkg.com/htmx.org/dist/ext/sse.js" $ ""
  H.body H.! hxExt "sse" H.! sseConnect "/events" $ do
    H.main
      content

indexPage :: WebUIState -> Html
indexPage state =
  htmlBase $ do
    experimentList state
    H.div H.! A.id "run-info" $ pure ()

runInfoPage :: WebUIState -> Experiment -> RunInfo -> Html
runInfoPage state experiment run =
  htmlBase $ do
    experimentList state
    runInfoBlock experiment run

runInfoBlock :: Experiment -> RunInfo -> Html
runInfoBlock experiment run = H.div H.! A.id "run-info" H.! A.class_ "long" $ do
  infoBoxNoTitle . table $
    [ ["UUID", H.toHtml (show experiment.uuid)],
      ["Expected Result", if experiment.expectedResult then "Equivalent" else "Non-equivalent"],
      ["Comparison Value", H.text experiment.comparisonValue],
      case run of
        CompletedRun result ->
          [ "Actual Result",
            case result.proofFound of
              Just True -> "Equivalent"
              Just False -> "Non-equivalent"
              Nothing -> "Inconclusive"
          ]
        _ -> []
    ]

  infoBox "Design" (H.pre $ H.text ("\n" <> experiment.design.source))

  whenJust (run ^? #_CompletedRun % #counterExample % _Just) $ \counterExample ->
    infoBox
      "Counter-example"
      (H.pre $ H.text ("\n" <> counterExample))

  whenJust (run ^? #_CompletedRun % #fullOutput) $ \output ->
    infoBox
      "Full output"
      (H.pre $ H.text ("\n" <> output))

experimentList :: WebUIState -> Html
experimentList state = H.div
  H.! A.id "experiment-list"
  H.! (hxTrigger "sse:experiment-list" <> hxGet "/experiments" <> hxSwap "outerHTML")
  $ do
    experimentSubList "Running" state.runningExperiments
    experimentSubList "Interesting" state.interestingExperiments
  where
    experimentSubList title experiments =
      infoBoxWithSideTitle
        title
        (H.toHtml (length experiments))
        (mapM_ experimentListItem experiments)

    experimentListItem :: ExperimentInfo -> Html
    experimentListItem info =
      H.div H.! A.class_ "experiment-list-item" $ do
        H.span H.! A.class_ "experiment-list-uuid" $
          H.toHtml (show info.experiment.uuid)
        H.ul H.! A.class_ "experiment-list-run-list" $
          forM_ (Map.keys info.runs) $ \(runnerInfo :: RunnerInfo) ->
            H.li
              ( H.a
                  H.! hxTarget "#run-info"
                  H.! hxSwap "outerHTML"
                  H.! hxPushUrl "true"
                  H.! hxGet [i|/experiments/#{show (info ^. #experiment % #uuid)}/runs/#{urlEncode False (T.encodeUtf8 runnerInfo)}|]
                  H.! A.href [i|/experiments/#{show (info ^. #experiment % #uuid)}/runs/#{urlEncode False (T.encodeUtf8 runnerInfo)}|]
                  $ H.toHtml runnerInfo
              )

table :: [[Html]] -> Html
table rows = H.table $
  forM_ rows $ \row ->
    H.tr $
      forM_ row $ \content ->
        H.td content

sideBySide :: Html -> Html
sideBySide = H.div H.! A.class_ "side-by-side"

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

checkSemaphore :: Semaphore -> STM Bool
checkSemaphore (Semaphore mvar) = not <$> isEmptyTMVar mvar

signalSemaphore :: Semaphore -> STM ()
signalSemaphore (Semaphore mvar) = void $ tryPutTMVar mvar ()

chooseSemaphore :: [(Semaphore, STM a)] -> STM a
chooseSemaphore choices = do
  signalledChoices <- forM choices $ \(s, x) -> do
    isSignalled <- checkSemaphore s
    return $
      if isSignalled
        then Just (s, x)
        else Nothing
  case catMaybes signalledChoices of
    (s, x) : _ -> do
      waitForSemaphore s -- Cleared the signal (and only this signal)
      x
    [] -> retry

shouldKeep :: ExperimentInfo -> Bool
shouldKeep info =
  let proofsFound = [proofFound | CompletedRun (ExperimentResult {proofFound}) <- Map.elems info.runs]
   in any (/= Just info.experiment.expectedResult) proofsFound

makeFieldLabelsNoPrefix ''WebUIState
makeFieldLabelsNoPrefix ''ExperimentInfo
makeFieldLabelsNoPrefix ''RunInfo
