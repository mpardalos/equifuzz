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
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (IsString (fromString))
import Data.Text.Lazy qualified as LT
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Experiments (DesignSource (..), Experiment (..), ExperimentProgress (..), ExperimentResult (..))
import GHC.Generics (Generic)
import Network.HTTP.Types (status200)
import Network.Wai (StreamingBody)
import Optics (At (at), makeFieldLabelsNoPrefix, use, (%))
import Optics.State.Operators ((.=))
import Text.Blaze.Html.Renderer.Pretty qualified as H
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx (hxExt, hxGet, hxSwap, hxTarget, hxTrigger)
import Text.Blaze.Htmx.ServerSentEvents (sseConnect)
import Web.Scotty (ActionM, Parsable (..), addHeader, get, html, param, raw, scotty, setHeader, status, stream)

data ExperimentInfo = ExperimentInfo
  { experiment :: Experiment,
    result :: Maybe ExperimentResult
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''ExperimentInfo

data WebUIState = WebUIState
  { running :: Map UUID ExperimentInfo,
    runningSem :: Semaphore,
    interesting :: Map UUID ExperimentInfo,
    interestingSem :: Semaphore,
    uninteresting :: Map UUID ExperimentInfo,
    uninterestingSem :: Semaphore
  }
  deriving (Generic)

newWebUIState :: IO WebUIState
newWebUIState = do
  runningSem <- atomically newSemaphore
  interestingSem <- atomically newSemaphore
  uninterestingSem <- atomically newSemaphore
  return
    WebUIState
      { running = Map.empty,
        runningSem,
        interesting = Map.empty,
        interestingSem,
        uninteresting = Map.empty,
        uninterestingSem
      }

handleProgress :: MVar WebUIState -> ExperimentProgress -> IO ()
handleProgress stateVar progress = do
  state <- readMVar stateVar

  modifyMVar_ stateVar . execStateT $ case progress of
    Began experiment -> do
      #running % at experiment.uuid .= Just (ExperimentInfo experiment Nothing)
      liftIO . atomically $ signalSemaphore state.runningSem
    Aborted experiment -> do
      #running % at experiment.uuid .= Nothing
      liftIO . atomically $ signalSemaphore state.runningSem
    Completed result -> do
      use (#running % at result.uuid) >>= \case
        Nothing -> pure () -- FIXME: Report this as an error
        Just (ExperimentInfo experiment _) -> do
          #running % at experiment.uuid .= Nothing
          liftIO . atomically $ signalSemaphore state.runningSem
          if Just experiment.expectedResult == result.proofFound
            then do
              #uninteresting % at result.uuid .= Just (ExperimentInfo experiment (Just result))
              liftIO . atomically $ signalSemaphore state.uninterestingSem
            else do
              #interesting % at result.uuid .= Just (ExperimentInfo experiment (Just result))
              liftIO . atomically $ signalSemaphore state.interestingSem

-- #experiments % at result.uuid %? #result .= Just result

runWebUI :: MVar WebUIState -> IO ()
runWebUI stateVar = scotty 8888 $ do
  get "/resources/style.css" $
    raw $
      LB.fromStrict $(embedFile =<< makeRelativeToProject "resources/style.css")

  get "/resources/htmx.js" $ do
    addHeader "Content-Type" "application/javascript"
    raw $
      LB.fromStrict $(embedFile =<< makeRelativeToProject "resources/htmx.js")

  get "/experiments/:uuid" $ do
    UUIDParam uuid <- param "uuid"
    state <- liftIO (readMVar stateVar)

    whenJust
      ( Map.lookup uuid state.running
          <|> Map.lookup uuid state.interesting
          <|> Map.lookup uuid state.uninteresting
      )
      $ \info ->
        blazeHtml (experimentInfo info)

  get "/experiments/:uuid/outputs/:output-type" $ do
    UUIDParam uuid <- param "uuid"
    outputType <- param "output-type"
    state <- liftIO (readMVar stateVar)

    whenJust
      ( Map.lookup uuid state.running
          <|> Map.lookup uuid state.interesting
          <|> Map.lookup uuid state.uninteresting
      )
      $ \info ->
        blazeHtml (experimentExtraOutput outputType info)

  get "/experiments/stream" $ do
    state <- liftIO (readMVar stateVar)
    status status200
    setHeader "Content-Type" "text/event-stream"
    stream . eventStreamFromIO . atomically . chooseSemaphore $
      [ (state.runningSem, pure [EventStreamEvent {event = "running-list", data_ = "update"}]),
        (state.interestingSem, pure [EventStreamEvent {event = "interesting-list", data_ = "update"}]),
        (state.uninterestingSem, pure [EventStreamEvent {event = "uninteresting-list", data_ = "update"}])
      ]

  get "/running-list" $ do
    state <- liftIO (readMVar stateVar)
    blazeHtml (runningList state)

  get "/interesting-list" $ do
    state <- liftIO (readMVar stateVar)
    blazeHtml (interestingList state)

  get "/uninteresting-list" $ do
    state <- liftIO (readMVar stateVar)
    blazeHtml (uninterestingList state)

  get "/" $ do
    state <- liftIO (readMVar stateVar)
    blazeHtml (indexPage state)

indexPage :: WebUIState -> Html
indexPage state = H.docTypeHtml $ do
  H.head $ do
    H.title "Equifuzz"
    H.link H.! A.rel "stylesheet" H.! A.href "/resources/style.css"
    -- H.script H.! A.src "/resources/htmx.min.js.gz" $ ""
    H.script H.! A.src "/resources/htmx.js" $ ""
    H.script H.! A.src "https://unpkg.com/htmx.org/dist/ext/sse.js" $ ""
  H.body H.! hxExt "sse" H.! sseConnect "/experiments/stream" $ do
    H.main $ do
      runningList state
      interestingList state
      uninterestingList state
      H.div H.! A.id "experiment-info" $ pure ()

runningList :: WebUIState -> Html
runningList state =
  experimentUUIDList
    "Running"
    "running-list"
    "sse:running-list"
    "/running-list"
    (Map.keys state.running)

interestingList :: WebUIState -> Html
interestingList state =
  experimentUUIDList
    "Interesting"
    "interesting-list"
    "sse:interesting-list"
    "/interesting-list"
    (Map.keys state.interesting)

uninterestingList :: WebUIState -> Html
uninterestingList state =
  experimentUUIDList
    "Uninteresting"
    "uninteresting-list"
    "sse:uninteresting-list"
    "/uninteresting-list"
    (Map.keys state.uninteresting)

experimentUUIDList ::
  Foldable t =>
  -- | Title
  Html ->
  -- | id for the info-box
  H.AttributeValue ->
  -- | hxTrigger value for reloading
  H.AttributeValue ->
  -- | hxGet value for reloading
  H.AttributeValue ->
  -- | Experiment list to show from
  t UUID ->
  Html
experimentUUIDList title elementId trigger updateUrl uuids =
  H.div
    H.! A.class_ "info-box"
    H.! A.id elementId
    H.! hxTrigger trigger
    H.! hxGet updateUrl
    $ do
      H.h2 H.! A.class_ "info-box-title flex-spread" $ do
        H.span title
        H.span (H.toHtml (length uuids))

      H.div H.! A.class_ "info-box-content long" $
        mapM_ experimentUUIDItem uuids

experimentUUIDItem :: UUID -> Html
experimentUUIDItem uuid =
  H.div
    H.! A.class_ "experiment-list-item"
    H.! hxGet ("/experiments/" <> fromString (show uuid))
    H.! hxTarget "#experiment-info"
    H.! hxSwap "outerHTML"
    $ H.toHtml (show uuid)

experimentInfo :: ExperimentInfo -> Html
experimentInfo info = H.div
  H.! A.id "experiment-info"
  H.!? (experimentBucket info == Running, hxReloadFrom ("/experiments/" <> fromString (show info.experiment.uuid)))
  $ do
    H.div H.! A.class_ "experiment-details info-box" $
      H.div H.! A.class_ "info-box-content" $
        H.table $ do
          H.tr $ do
            H.td "UUID"
            H.td $ fromString $ show info.experiment.uuid
          H.tr $ do
            H.td "Expected Result"
            H.td $
              if info.experiment.expectedResult
                then "Equivalent"
                else "Non-equivalent"
          case info.result of
            Nothing -> pure ()
            Just result -> do
              H.tr $ do
                H.td "Actual Result"
                H.td $ case result.proofFound of
                  Just True -> "Equivalent"
                  Just False -> "Non-equivalent"
                  Nothing -> "Inconclusive"
    H.div H.! A.class_ "info-box long experiment-source-spec" $ do
      H.h2 H.! A.class_ "info-box-title" $ "Source"
      H.div H.! A.class_ "info-box-content long" $
        H.pre $
          H.text ("\n" <> info.experiment.designSpec.source)
    H.div H.! A.class_ "info-box experiment-source-impl" $ do
      H.h2 H.! A.class_ "info-box-title" $ "Implementation"
      H.div H.! A.class_ "info-box-content long" $
        H.pre $
          H.text ("\n" <> info.experiment.designImpl.source)

    experimentExtraOutput CounterExample info

data OutputType = Log | CounterExample
  deriving (Show, Eq, Ord)

instance Parsable OutputType where
  parseParam "log" = Right Log
  parseParam "counter-example" = Right CounterExample
  parseParam txt = Left ("'" <> txt <> "' is not a valid output type")

experimentExtraOutput :: OutputType -> ExperimentInfo -> Html
experimentExtraOutput tab info = whenJust info.result $ \result -> do
  let text = case tab of
        Log -> result.fullOutput
        CounterExample -> fromMaybe "" (result.counterExample)

  H.div H.! A.class_ "info-box tab-box experiment-extra-output" $ do
    H.div H.! A.class_ "tabs" $ do
      let uuid = fromString (show info.experiment.uuid)
      H.span
        H.! hxGet ("/experiments/" <> uuid <> "/outputs/log")
        H.! hxTarget "closest .experiment-extra-output"
        H.! hxSwap "outerHTML"
        H.! (if tab == Log then A.class_ "tab selected" else A.class_ "tab")
        $ "Log"
      H.span
        H.! hxGet ("/experiments/" <> uuid <> "/outputs/counter-example")
        H.! hxTarget "closest .experiment-extra-output"
        H.! hxSwap "outerHTML"
        H.! (if tab == CounterExample then A.class_ "tab selected" else A.class_ "tab")
        $ "Counter Example"
    H.div H.! A.class_ "tab-box-content long" $
      H.pre $
        H.text ("\n" <> text)

-- | htmx attributes to make an element continuously poll-reload from a URL
hxReloadFrom :: H.AttributeValue -> H.Attribute
hxReloadFrom url = hxGet url <> hxTrigger "load delay:5s" <> hxSwap "outerHTML"

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x

data ExperimentBucket = Running | Interesting | Uninteresting
  deriving (Show, Eq, Ord)

experimentBucket :: ExperimentInfo -> ExperimentBucket
experimentBucket ExperimentInfo {result = Nothing} = Running
experimentBucket (ExperimentInfo experiment (Just result))
  | Just experiment.expectedResult == result.proofFound = Uninteresting
  | otherwise = Interesting

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
