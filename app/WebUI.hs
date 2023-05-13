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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Experiments (DesignSource (..), Experiment (..), ExperimentProgress (..), ExperimentResult (..))
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status404)
import Optics (At (at), makeFieldLabelsNoPrefix, (%), (%?))
import Optics.State.Operators ((.=))
import Text.Blaze.Html.Renderer.Pretty qualified as H
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx (hxGet, hxSwap, hxTarget, hxTrigger)
import Web.Scotty (ActionM, ScottyM, addHeader, file, get, html, next, param, scotty, status)

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
    file "resources/style.css"
  get "/resources/htmx.js" $ do
    addHeader "Content-Type" "application/javascript"
    file "resources/htmx.js"
  get "/experiments/:uuid" $ do
    uuid <-
      UUID.fromString <$> param "uuid" >>= \case
        Just uuid -> pure uuid
        Nothing -> next
    state <- liftIO (readMVar stateVar)

    case Map.lookup uuid state.experiments of
      Just experimentInfo ->
        html
          . LT.pack
          . H.renderHtml
          $ experimentDetails experimentInfo
      Nothing -> status status404
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

hxReloadFrom :: H.AttributeValue -> H.Attribute
hxReloadFrom url = hxGet url <> hxTrigger "load delay:5s" <> hxSwap "outerHTML"

runningList :: WebUIState -> Html
runningList state =
  H.div
    H.! A.id "running-list"
    H.! hxReloadFrom "/running-list"
    $ experimentList
      "Running"
      [ uuid
        | (uuid, experiment) <- Map.toList state.experiments,
          experimentBucket experiment == Running
      ]

interestingList :: WebUIState -> Html
interestingList state =
  H.div
    H.! A.id "interesting-list"
    H.! hxReloadFrom "/interesting-list"
    $ experimentList
      "Interesting"
      [ uuid
        | (uuid, experiment) <- Map.toList state.experiments,
          experimentBucket experiment == Interesting
      ]

uninterestingList :: WebUIState -> Html
uninterestingList state =
  H.div
    H.! A.id "uninteresting-list"
    H.! hxReloadFrom "/uninteresting-list"
    $ experimentList
      "Uninteresting"
      [ uuid
        | (uuid, experiment) <- Map.toList state.experiments,
          experimentBucket experiment == Uninteresting
      ]

experimentList :: Text -> [UUID] -> Html
experimentList name uuids = do
  H.h2 H.! A.class_ "experiment-list-title" $ H.toHtml name
  H.div H.! A.class_ "experiment-list" $ forM_ uuids $ \uuid -> do
    H.div
      H.! A.class_ "experiment-list-item"
      H.! hxGet ("/experiments/" <> fromString (show uuid))
      H.! hxTarget "#experiment-info"
      H.! hxSwap "outerHTML"
      $ H.toHtml (show uuid)

experimentDetails :: ExperimentInfo -> Html
experimentDetails info = H.div
  H.! A.id "experiment-info"
  H.! hxReloadFrom ("/experiments/" <> fromString (show info.experiment.uuid))
  $ do
    (H.div H.! A.class_ "experiment-details info-box") $
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
    H.div H.! A.class_ "info-box long experiment-source-spec" $
      H.pre $
        H.text ("\n" <> info.experiment.designSpec.source)
    H.div H.! A.class_ "info-box long experiment-source-impl" $
      H.pre $
        H.text ("\n" <> info.experiment.designImpl.source)

    whenJust info.result $ \result -> do
      H.div H.! A.class_ "info-box long experiment-log" $
        H.pre $
          H.text ("\n" <> result.fullOutput)

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
