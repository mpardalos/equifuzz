{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Brick qualified as B
import Brick.BChan qualified as B
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as B
import Brick.Widgets.Center qualified as B
import Brick.Widgets.Dialog qualified as B
import Brick.Widgets.List qualified as B
import Control.Concurrent (forkIO)
import Control.Exception (Exception)
import Control.Monad (void)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Experiments
import Graphics.Vty qualified as Vty
import Optics
import Optics.State.Operators ((%%=), (%=), (.=))

data EndExperiment = EndExperiment
  deriving (Show)

instance Exception EndExperiment

data AppState = AppState
  { running :: Seq Experiment,
    interesting :: Seq ExperimentResult,
    uninteresting :: Seq ExperimentResult
  }

makeFieldLabelsNoPrefix ''AppState

mkContinueDialog :: B.Dialog Bool Text
mkContinueDialog =
  B.dialog
    (Just (B.txt label))
    (Just ("No", [("No", "No", False), ("Yes", "Yes", True)]))
    (T.length label + 6)
  where
    label = "Run another experiment?"

appDraw :: AppState -> B.Widget Text
appDraw st =
  B.hBox
    [ listOf "runninglist" "Running" st.running,
      B.vBox
        [ listOf "interestingList" "Interesting" st.interesting,
          listOf "uninterestingList" "Uninteresting" st.uninteresting
        ]
    ]
  where
    listOf n title items =
      B.border
        . B.vBox
        $ [ B.str title,
            B.hBorder,
            B.renderList (\_ it -> B.str (show it.uuid)) False (B.list n items 1)
          ]

appHandleEvent :: B.BrickEvent Text ExperimentProgress -> B.EventM Text AppState ()
appHandleEvent (B.VtyEvent (Vty.EvKey (Vty.KChar 'q') _)) = B.halt
appHandleEvent (B.AppEvent (Began experiment)) = #running %= (Seq.|> experiment)
appHandleEvent (B.AppEvent (Completed result)) = do
  experimentIdx <- use #running <&> fromJust . Seq.findIndexL ((== result.uuid) . view #uuid)
  experiment <- use #running <&> (`Seq.index` experimentIdx)
  if result.proofFound == experiment.expectedResult
    then #uninteresting %= (Seq.|> result)
    else #interesting %= (Seq.|> result)
  #running %= Seq.deleteAt experimentIdx
appHandleEvent _ = pure ()

main :: IO ()
main = do
  eventChan <- B.newBChan 10

  -- Start experiment running thread
  void . forkIO $
    experimentLoop mkNegativeExperiment runVCFormal (B.writeBChan eventChan)

  -- UI/main thread
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $
    B.customMain @Text
      initialVty
      buildVty
      (Just eventChan)
      B.App
        { appDraw = \s -> [B.withBorderStyle B.unicodeRounded $ B.center $ appDraw s],
          appChooseCursor = B.neverShowCursor,
          appAttrMap =
            const
              ( B.attrMap
                  Vty.defAttr
                  [ (B.buttonSelectedAttr, Vty.defAttr `Vty.withStyle` Vty.standout)
                  ]
              ),
          appHandleEvent,
          appStartEvent = pure ()
        }
      (AppState mempty mempty mempty)
