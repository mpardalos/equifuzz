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
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
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
import Optics (At (at), makeFieldLabelsNoPrefix, traversed, (%), (&), (.~), (?~), (^..))
import Text.Blaze.Html.Renderer.Pretty qualified as H
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Htmx (hxGet, hxSwap, hxTarget)
import Web.Scotty (addHeader, file, get, html, next, param, scotty, status)

data ExperimentInfo = ExperimentInfo
  { experiment :: Experiment,
    result :: Maybe ExperimentResult
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''ExperimentInfo

data WebUIState = WebUIState
  { running :: Map UUID ExperimentInfo,
    interesting :: Map UUID ExperimentInfo,
    uninteresting :: Map UUID ExperimentInfo
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''WebUIState

newWebUIState :: WebUIState
newWebUIState =
  WebUIState
    { running = Map.empty,
      interesting = Map.empty,
      uninteresting = Map.empty
    }

lookupPopKey :: Ord k => k -> Map k v -> Maybe (v, Map k v)
lookupPopKey k m = do
  v <- Map.lookup k m
  Just (v, Map.delete k m)

handleProgress :: MVar WebUIState -> ExperimentProgress -> IO ()
handleProgress stateVar progress =
  modifyMVar_ stateVar $
    pure . \state ->
      case progress of
        Began experiment ->
          state & #running % at (experiment.uuid) ?~ ExperimentInfo experiment Nothing
        Aborted experiment ->
          case lookupPopKey experiment.uuid state.running of
            Nothing -> state
            Just (_, runningWithoutAborted) ->
              state & #running .~ runningWithoutAborted
        Completed result ->
          case lookupPopKey result.uuid state.running of
            Just (experimentInfo, runningWithoutCompleted) ->
              state
                & #running .~ runningWithoutCompleted
                & if result.proofFound == Just experimentInfo.experiment.expectedResult
                  then #uninteresting % at result.uuid ?~ (experimentInfo {result = Just result})
                  else #interesting % at result.uuid ?~ (experimentInfo {result = Just result})
            -- FIXME: Report this as an error. An experiment was completed but it is not in running
            Nothing -> state

runWebUI :: MVar WebUIState -> IO ()
runWebUI stateVar = scotty 8888 $ do
  get "/resources/style.css" $
    file "resources/style.css"
  get "/resources/htmx.min.js.gz" $ do
    addHeader "Content-Type" "application/javascript"
    addHeader "Content-Encoding" "gzip"
    file "resources/htmx.min.js.gz"
  get "/resources/htmx.js" $ do
    addHeader "Content-Type" "application/javascript"
    file "resources/htmx.js"
  get "/experiments/:uuid" $ do
    uuid <-
      UUID.fromString <$> param "uuid" >>= \case
        Just uuid -> pure uuid
        Nothing -> next
    state <- liftIO (readMVar stateVar)

    case Map.lookup uuid state.running
      <|> Map.lookup uuid state.interesting
      <|> Map.lookup uuid state.uninteresting of
      Just experimentInfo ->
        html
          . LT.pack
          . H.renderHtml
          $ experimentDetails experimentInfo
      Nothing -> status status404
  get "/" $ do
    state <- liftIO (readMVar stateVar)
    html
      . LT.pack
      . H.renderHtml
      $ indexPage state

indexPage :: WebUIState -> Html
indexPage state = H.docTypeHtml $ do
  H.head $ do
    H.title "Equifuzz"
    H.link H.! A.rel "stylesheet" H.! A.href "/resources/style.css"
    -- H.script H.! A.src "/resources/htmx.min.js.gz" $ ""
    H.script H.! A.src "/resources/htmx.js" $ ""
  H.body $ do
    H.main $ do
      H.div H.! A.id "running-list" $
        experimentList "Running" (state ^.. (#running % traversed % #experiment % #uuid))
      H.div H.! A.id "interesting-list" $
        experimentList "Interesting" (state ^.. (#interesting % traversed % #experiment % #uuid))
      H.div H.! A.id "uninteresting-list" $
        experimentList "Uninteresting" (state ^.. (#uninteresting % traversed % #experiment % #uuid))
      H.div H.! A.id "experiment-info" $
        pure ()

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
experimentDetails info = H.div H.! A.id "experiment-info" $ do
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
  H.div H.! A.class_ "info-box experiment-source-spec" $
    H.pre $
      H.toHtml $
        info.experiment.designSpec.source
  H.div H.! A.class_ "info-box experiment-source-impl" $
    H.pre $
      H.toHtml $
        info.experiment.designImpl.source

  whenJust info.result $ \result -> do
    H.div H.! A.class_ "info-box experiment-log" $
      H.pre $
        H.toHtml $
          result.fullOutput

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust Nothing _ = pure ()
whenJust (Just x) f = f x
