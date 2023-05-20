{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# HLINT ignore "Redundant <$>" #-}

module WebUI (WebUIState, newWebUIState, handleProgress, runWebUI) where

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
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Experiments (Experiment (..), ExperimentProgress (..), ExperimentResult (..), RunnerInfo)
import GHC.Generics (Generic)
import Network.HTTP.Types (status200, urlDecode, urlEncode)
import Network.Wai (StreamingBody)
import Optics (At (at), makeFieldLabelsNoPrefix, traversed, (%), (%?), (^.), (^..), (^?), _Just)
import Optics.State.Operators ((%=), (.=))
import Text.Blaze.Html.Renderer.Pretty qualified as H
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx (hxExt, hxGet, hxSwap, hxTrigger)
import Text.Blaze.Htmx.ServerSentEvents (sseConnect)
import Text.Printf (printf)
import Web.Scotty (ActionM, Parsable (..), addHeader, get, html, next, notFound, param, raw, scotty, setHeader, status, stream)

data ExperimentInfo = ExperimentInfo
  { experiment :: Experiment,
    runs :: Map RunnerInfo RunInfo
  }
  deriving (Generic)

data RunInfo
  = CompletedRun ExperimentResult
  | ActiveRun RunnerInfo
  deriving (Eq, Show)

data WebUIState = WebUIState
  { interestingExperiments :: Map UUID ExperimentInfo,
    interestingExperimentsSem :: Semaphore,
    totalRunCount :: Int,
    totalRunCountSem :: Semaphore
  }
  deriving (Generic)

newWebUIState :: IO WebUIState
newWebUIState = do
  interestingExperimentsSem <- atomically newSemaphore
  totalRunCountSem <- atomically newSemaphore
  return
    WebUIState
      { interestingExperiments = Map.empty,
        interestingExperimentsSem,
        totalRunCount = 0,
        totalRunCountSem
      }

handleProgress :: MVar WebUIState -> ExperimentProgress -> IO ()
handleProgress stateVar progress = do
  state <- readMVar stateVar

  modifyMVar_ stateVar . execStateT $ case progress of
    NewExperiment experiment -> do
      #interestingExperiments % at experiment.uuid .= Just (ExperimentInfo experiment [])
      liftIO . atomically $ signalSemaphore state.interestingExperimentsSem
    BeginRun uuid runnerInfo -> do
      -- FIXME: Report error if uuid does not exist
      #interestingExperiments % at uuid %? #runs % at runnerInfo .= Just (ActiveRun runnerInfo)
    RunCompleted result -> do
      -- FIXME: Report error if uuid does not exist
      #interestingExperiments % at result.uuid %? #runs % at result.runnerInfo .= Just (CompletedRun result)
      #totalRunCount %= (+ 1)
      liftIO . atomically $ signalSemaphore state.interestingExperimentsSem
      liftIO . atomically $ signalSemaphore state.totalRunCountSem
    ExperimentCompleted uuid -> do
      -- FIXME: Report error if experiment does not exist
      -- FIXME: Report error if experiment still has active runs
      #interestingExperiments % at uuid %= \case
        Nothing -> Nothing
        Just info
          | shouldKeep info -> Just info
          | otherwise -> Nothing

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

    liftIO $ printf "Looking for runner %s" (show runnerInfo)

    case state
      ^? #interestingExperiments
      % at uuid
      %? #runs
      % at (T.decodeUtf8 runnerInfo)
      % _Just of
      Nothing -> next
      Just info -> blazeHtml (runInfo info)

  get "/experiments/stream" $ do
    state <- liftIO (readMVar stateVar)
    status status200
    setHeader "Content-Type" "text/event-stream"
    stream . eventStreamFromIO . atomically $ do
      waitForSemaphore state.interestingExperimentsSem
      pure [EventStreamEvent {event = "experiment-list", data_ = "update"}]

  get "/experiments" $ do
    state <- liftIO (readMVar stateVar)

    blazeHtml (experimentList state.interestingExperiments)

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
  H.body H.! hxExt "sse" H.! sseConnect "/experiments/stream" $ do
    H.main
      content

runInfo :: RunInfo -> Html
runInfo = const (H.p "Stuff goes here")

indexPage :: WebUIState -> Html
indexPage state =
  htmlBase $ do
    experimentList state.interestingExperiments
    H.div H.! A.id "experiment-info" $ pure ()

experimentList ::
  Foldable t =>
  -- | Experiment list to show from
  t ExperimentInfo ->
  Html
experimentList experiments =
  H.div
    H.! A.class_ "info-box"
    H.! A.id "experiment-list"
    -- H.! hxTrigger "sse:experiment-list"
    -- H.! hxGet "/experiments"
    -- H.! hxSwap "outerHTML"
    $ do
      H.h2 H.! A.class_ "info-box-title flex-spread" $ do
        H.span "Experiments"
        H.span (H.toHtml (length experiments))

      H.div H.! A.class_ "info-box-content long" $
        mapM_ experimentListItem experiments
  where
    experimentListItem :: ExperimentInfo -> Html
    experimentListItem info =
      H.details
        H.! A.class_ "experiment-list-item"
        -- H.! hxGet ("/experiments/" <> fromString (show uuid))
        -- H.! hxTarget "#experiment-info"
        -- H.! hxSwap "outerHTML"
        $ do
          H.summary (H.toHtml (show info.experiment.uuid))
          H.ul $
            forM_ (Map.keys info.runs) $ \(runnerInfo :: RunnerInfo) ->
              H.li
                $ H.a
                  H.! A.href [i|/experiments/#{show (info ^. #experiment % #uuid)}/runs/#{urlEncode False (T.encodeUtf8 runnerInfo)}|]
                $ H.toHtml runnerInfo

-- "stuff and things"

-- runInfo :: ExperimentInfo -> Html
-- runInfo info = H.div
--   H.! A.id "experiment-info"
--   -- H.!? (experimentBucket info == Running, hxReloadFrom ("/experiments/" <> fromString (show info.experiment.uuid)))
--   $ do
--     H.div H.! A.class_ "experiment-details info-box" $
--       H.div H.! A.class_ "info-box-content" $
--         H.table $ do
--           H.tr $ do
--             H.td "UUID"
--             H.td $ fromString $ show info.experiment.uuid
--           H.tr $ do
--             H.td "Expected Result"
--             H.td $
--               if info.experiment.expectedResult
--                 then "Equivalent"
--                 else "Non-equivalent"
--           case info.result of
--             Nothing -> pure ()
--             Just result -> do
--               H.tr $ do
--                 H.td "Actual Result"
--                 H.td $ case result.proofFound of
--                   Just True -> "Equivalent"
--                   Just False -> "Non-equivalent"
--                   Nothing -> "Inconclusive"
--     H.div H.! A.class_ "info-box long experiment-source-spec" $ do
--       H.h2 H.! A.class_ "info-box-title" $ "Source"
--       H.div H.! A.class_ "info-box-content long" $
--         H.pre $
--           H.text ("\n" <> info.experiment.designSpec.source)
--     H.div H.! A.class_ "info-box experiment-source-impl" $ do
--       H.h2 H.! A.class_ "info-box-title" $ "Implementation"
--       H.div H.! A.class_ "info-box-content long" $
--         H.pre $
--           H.text ("\n" <> info.experiment.designImpl.source)

--     experimentExtraOutput CounterExample info

-- data OutputType = Log | CounterExample
--   deriving (Show, Eq, Ord)

-- instance Parsable OutputType where
--   parseParam "log" = Right Log
--   parseParam "counter-example" = Right CounterExample
--   parseParam txt = Left ("'" <> txt <> "' is not a valid output type")

-- experimentExtraOutput :: OutputType -> ExperimentInfo -> Html
-- experimentExtraOutput tab info = whenJust info.result $ \result -> do
--   let text = case tab of
--         Log -> result.fullOutput
--         CounterExample -> fromMaybe "" (result.counterExample)

--   H.div H.! A.class_ "info-box tab-box experiment-extra-output" $ do
--     H.div H.! A.class_ "tabs" $ do
--       let uuid = fromString (show info.experiment.uuid)
--       H.span
--         H.! hxGet ("/experiments/" <> uuid <> "/outputs/log")
--         H.! hxTarget "closest .experiment-extra-output"
--         H.! hxSwap "outerHTML"
--         H.! (if tab == Log then A.class_ "tab selected" else A.class_ "tab")
--         $ "Log"
--       H.span
--         H.! hxGet ("/experiments/" <> uuid <> "/outputs/counter-example")
--         H.! hxTarget "closest .experiment-extra-output"
--         H.! hxSwap "outerHTML"
--         H.! (if tab == CounterExample then A.class_ "tab selected" else A.class_ "tab")
--         $ "Counter Example"
--     H.div H.! A.class_ "tab-box-content long" $
--       H.pre $
--         H.text ("\n" <> text)

-- -- | htmx attributes to make an element continuously poll-reload from a URL
-- hxReloadFrom :: H.AttributeValue -> H.Attribute
-- hxReloadFrom url = hxGet url <> hxTrigger "load delay:5s" <> hxSwap "outerHTML"

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
