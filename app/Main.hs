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
import Control.Concurrent (forkFinally, newEmptyMVar)
import Control.Exception (Exception)
import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Experiments
import GHC.Generics (Generic)
import Graphics.Vty qualified as Vty
import Optics

data EndExperiment = EndExperiment
  deriving (Show)

instance Exception EndExperiment

data AppState
  = StGenerating
  | StRunning
  | StWait {continueDialog :: B.Dialog Bool Text}
  deriving (Generic)

makePrismLabels ''AppState

data AppEvent
  = ExperimentProgress ExperimentProgress
  | ExperimentEnded

mkContinueDialog :: B.Dialog Bool Text
mkContinueDialog =
  B.dialog
    (Just (B.txt label))
    (Just ("No", [("No", "No", False), ("Yes", "Yes", True)]))
    (T.length label + 6)
  where
    label = "Run another experiment?"

appDraw :: AppState -> B.Widget Text
appDraw StWait {continueDialog} = B.renderDialog continueDialog (B.hCenter (B.txt "you sure?"))
appDraw _ =
  B.border
    . B.padLeftRight 3
    . B.padTopBottom 1
    . B.str
    $ "Running"

appHandleEvent :: B.BrickEvent Text AppEvent -> B.EventM Text AppState ()
appHandleEvent (B.VtyEvent (Vty.EvKey (Vty.KChar 'q') _)) = B.halt
appHandleEvent (B.AppEvent (ExperimentProgress Generating)) = B.put StGenerating
appHandleEvent (B.AppEvent (ExperimentProgress Running)) = B.put StRunning
appHandleEvent (B.AppEvent (ExperimentProgress (Completed (ExperimentResult _ _)))) = B.put (StWait mkContinueDialog)
appHandleEvent (B.VtyEvent e) =
  B.get >>= \case
    StWait {continueDialog} -> do
      continueDialog' <- B.nestEventM' continueDialog (B.handleDialogEvent e)
      B.put (StWait continueDialog')
      case (B.dialogSelection continueDialog', e) of
        (Just (_, True), Vty.EvKey Vty.KEnter []) -> pure () -- TODO: Signal experiment thread to continue
        (Just (_, False), Vty.EvKey Vty.KEnter []) -> B.halt
        _ -> pure ()
    _ -> pure ()
appHandleEvent _ = pure ()

main :: IO ()
main = do
  runSignal <- newEmptyMVar @()
  eventChan <- B.newBChan 10

  -- Start experiment running thread
  void $
    forkFinally
      (B.writeBChan eventChan (ExperimentProgress (Completed ExperimentResult {})))
      -- ( experimentLoop mkPositiveExperiment runVCFormal $ \progress -> do
      --     B.writeBChan eventChan (ExperimentProgress progress)
      --     when (isCompleted progress) (takeMVar runSignal)
      -- )
      (const (B.writeBChan eventChan ExperimentEnded))

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
      StGenerating
