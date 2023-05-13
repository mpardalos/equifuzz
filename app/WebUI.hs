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
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (execStateT)
import Data.ByteString.Lazy qualified as LB
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text.Lazy qualified as LT
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Experiments (DesignSource (..), Experiment (..), ExperimentProgress (..), ExperimentResult (..))
import GHC.Generics (Generic)
import Optics (At (at), makeFieldLabelsNoPrefix, (%), (%?))
import Optics.State.Operators ((.=))
import Text.Blaze.Html.Renderer.Pretty qualified as H
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx (hxGet, hxSwap, hxTarget, hxTrigger)
import Web.Scotty (ActionM, Parsable (..), addHeader, get, html, next, param, raw, scotty)

data ExperimentInfo = ExperimentInfo
  { experiment :: Experiment,
    result :: Maybe ExperimentResult
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''ExperimentInfo

newtype WebUIState = WebUIState
  { experiments :: Map UUID ExperimentInfo
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''WebUIState

newWebUIState :: WebUIState
newWebUIState = WebUIState {experiments = Map.empty}

handleProgress :: MVar WebUIState -> ExperimentProgress -> IO ()
handleProgress stateVar progress =
  modifyMVar_ stateVar . execStateT $ case progress of
    Began experiment ->
      #experiments % at experiment.uuid .= Just (ExperimentInfo experiment Nothing)
    Aborted experiment ->
      #experiments % at experiment.uuid .= Nothing
    Completed result ->
      #experiments % at result.uuid %? #result .= Just result

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

    whenJust (Map.lookup uuid state.experiments) $ \info ->
      blazeHtml (experimentInfo info)

  get "/experiments/:uuid/outputs/:output-type" $ do
    UUIDParam uuid <- param "uuid"
    outputType <- param "output-type"
    state <- liftIO (readMVar stateVar)

    whenJust (Map.lookup uuid state.experiments) $ \info ->
      blazeHtml (experimentExtraOutput outputType info)

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
  H.body $ do
    H.main $ do
      runningList state
      interestingList state
      uninterestingList state
      H.div H.! A.id "experiment-info" $ pure ()

runningList :: WebUIState -> Html
runningList =
  experimentUUIDList
    "Running"
    "running-list"
    "/running-list"
    Running

interestingList :: WebUIState -> Html
interestingList =
  experimentUUIDList
    "Interesting"
    "interesting-list"
    "/interesting-list"
    Interesting

uninterestingList :: WebUIState -> Html
uninterestingList =
  experimentUUIDList
    "Uninteresting"
    "uninteresting-list"
    "/uninteresting-list"
    Uninteresting

experimentUUIDList :: Html -> H.AttributeValue -> H.AttributeValue -> ExperimentBucket -> WebUIState -> Html
experimentUUIDList title elementId updateUrl bucket state =
  H.div H.! A.class_ "info-box" H.! A.id elementId H.! hxReloadFrom updateUrl $ do
    H.h2 H.! A.class_ "info-box-title" $ title

    let uuids =
          [ uuid
            | (uuid, experiment) <- Map.toList state.experiments,
              experimentBucket experiment == bucket
          ]
    H.div H.! A.class_ "info-box-content long" $
      forM_ uuids $ \uuid -> do
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
