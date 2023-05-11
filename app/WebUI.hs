{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module WebUI (WebUIState, newWebUIState, handleProgress, runWebUI) where

import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.String.Interpolate (i, __i)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.UUID (UUID)
import Experiments (Experiment (..), ExperimentProgress (..), ExperimentResult (..))
import Optics (makeFieldLabelsNoPrefix, traversed, view, (%), (%~), (&), (.~), (^..))
import Text.Blaze.Html.Renderer.Pretty qualified as H
import Text.Blaze.Html5 (Html)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Web.Scotty (file, get, html, scotty)

data CompletedExperiment = CompletedExperiment
  { experiment :: Experiment,
    result :: ExperimentResult
  }

makeFieldLabelsNoPrefix ''CompletedExperiment

data WebUIState = WebUIState
  { running :: Seq Experiment,
    interesting :: Seq CompletedExperiment,
    uninteresting :: Seq CompletedExperiment
  }

makeFieldLabelsNoPrefix ''WebUIState

newWebUIState :: WebUIState
newWebUIState =
  WebUIState
    { running = Seq.empty,
      interesting = Seq.empty,
      uninteresting = Seq.empty
    }

findAndRemoveL :: (a -> Bool) -> Seq a -> Maybe (a, Seq a)
findAndRemoveL p xs = do
  idx <- Seq.findIndexL p xs
  x <- Seq.lookup idx xs
  return (x, Seq.deleteAt idx xs)

handleProgress :: MVar WebUIState -> ExperimentProgress -> IO ()
handleProgress stateVar progress =
  modifyMVar_ stateVar $
    pure . \state ->
      case progress of
        Began experiment ->
          state & #running %~ (Seq.|> experiment)
        Aborted experiment ->
          case findAndRemoveL ((== experiment.uuid) . view #uuid) state.running of
            Nothing -> state
            Just (_, runningWithoutAborted) ->
              state & #running .~ runningWithoutAborted
        Completed result ->
          case findAndRemoveL ((== result.uuid) . view #uuid) state.running of
            Just (experiment, runningWithoutCompleted) ->
              state
                & #running .~ runningWithoutCompleted
                & if result.proofFound == Just experiment.expectedResult
                  then #uninteresting %~ (Seq.|> CompletedExperiment experiment result)
                  else #interesting %~ (Seq.|> CompletedExperiment experiment result)
            -- FIXME: Report this as an error. An experiment was completed but it is not in running
            Nothing -> state

runWebUI :: MVar WebUIState -> IO ()
runWebUI stateVar = scotty 8888 $ do
  get "/style.css" $ file "resources/style.css"
  get "/" $ do
    state <- liftIO (readMVar stateVar)
    html . LT.pack . H.renderHtml $ indexPage state

indexPage :: WebUIState -> Html
indexPage state = H.docTypeHtml $ do
  H.head $ do
    H.title "Equifuzz"
    H.link H.! A.rel "stylesheet" H.! A.href "/style.css"
  H.body $ do
    H.div H.! A.class_ "list-section" $ do
      experimentList "Running" (state ^.. (#running % traversed % #uuid))
      experimentList "Interesting" (state ^.. (#interesting % traversed % #experiment % #uuid))
      experimentList "Uninteresting" (state ^.. (#uninteresting % traversed % #experiment % #uuid))

experimentList :: Text -> [UUID] -> Html
experimentList name uuids =
  H.div H.! A.class_ "experiment-list-container" $ do
    H.h2 H.! A.class_ "experiment-list-title" $ H.toHtml name
    H.div H.! A.class_ "experiment-list" $ forM_ uuids $ \uuid -> do
      H.div H.! A.class_ "experiment-list-item" $
        H.toHtml (show uuid)
