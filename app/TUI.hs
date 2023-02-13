{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TUI (runTUI) where

import Brick qualified as B
import Brick.BChan qualified as B
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as B
import Brick.Widgets.Center qualified as B
import Brick.Widgets.Dialog qualified as B
import Brick.Widgets.List qualified as B
import Control.Monad (void)
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.UUID (UUID)
import Experiments
import GHC.Records (HasField)
import Graphics.Vty qualified as Vty
import Optics
import Optics.State.Operators ((%=))

instance LabelOptic "name" A_Lens (B.GenericList n t e) (B.GenericList n t e) n n where
  labelOptic = lensVL B.listNameL

instance LabelOptic "elements" A_Lens (B.GenericList n t e) (B.GenericList n t e) (t e) (t e) where
  labelOptic = lensVL B.listElementsL

instance
  (B.Splittable t, Traversable t, Semigroup (t e)) =>
  LabelOptic "selectedElement" A_Traversal (B.GenericList n t e) (B.GenericList n t e) e e
  where
  labelOptic = traversalVL B.listSelectedElementL

data WidgetID
  = RunningList
  | InterestingList
  | UninterestingList
  deriving (Eq, Ord, Show, Enum, Bounded)

cycleNext :: (Enum a, Bounded a, Eq a) => a -> a
cycleNext x
  | x == maxBound = minBound
  | otherwise = succ x

cyclePrevious :: (Enum a, Bounded a, Eq a) => a -> a
cyclePrevious x
  | x == minBound = maxBound
  | otherwise = pred x

data AppState = AppState
  { running :: B.GenericList WidgetID Seq Experiment,
    interesting :: B.GenericList WidgetID Seq ExperimentResult,
    uninteresting :: B.GenericList WidgetID Seq ExperimentResult,
    focusedElementId :: WidgetID
  }

makeFieldLabelsNoPrefix ''AppState

initialAppState :: AppState
initialAppState =
  AppState
    { running = B.list RunningList Seq.empty 1,
      interesting = B.list InterestingList Seq.empty 1,
      uninteresting = B.list UninterestingList Seq.empty 1,
      focusedElementId = RunningList
    }

appDraw :: AppState -> B.Widget WidgetID
appDraw st =
  B.vBox
    [ renderList "Running" st.running,
      renderList "Interesting" st.interesting,
      renderList "Uninteresting" st.uninteresting
    ]
  where

    renderList :: HasField "uuid" a UUID => String -> B.GenericList WidgetID Seq a -> B.Widget WidgetID
    renderList title list =
      let listFocused = list ^. #name == st.focusedElementId
       in B.border
            . B.vBox
            $ [ (if listFocused then B.withAttr (B.attrName "selected") else id)
                  (B.str title),
                B.hBorder,
                B.renderList
                  (\selected it -> B.str ((if listFocused && selected then "→ " else "  ") <> show it.uuid))
                  (list ^. #name == st.focusedElementId)
                  list
              ]

appHandleEvent :: B.BrickEvent WidgetID ExperimentProgress -> B.EventM WidgetID AppState ()
appHandleEvent (B.VtyEvent (Vty.EvKey (Vty.KChar 'q') _)) = B.halt
appHandleEvent (B.VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) = #focusedElementId %= cycleNext
appHandleEvent (B.VtyEvent (Vty.EvKey (Vty.KChar '\t') [Vty.MShift])) = #focusedElementId %= cyclePrevious
appHandleEvent (B.VtyEvent ev) =
  use #focusedElementId >>= \case
    RunningList -> B.zoom (toLensVL #running) (B.handleListEvent ev)
    InterestingList -> B.zoom (toLensVL #interesting) (B.handleListEvent ev)
    UninterestingList -> B.zoom (toLensVL #uninteresting) (B.handleListEvent ev)
appHandleEvent (B.AppEvent (Began experiment)) = #running % #elements %= (Seq.|> experiment)
appHandleEvent (B.AppEvent (Completed result)) = do
  experimentIdx <- use (#running % #elements) <&> fromJust . Seq.findIndexL ((== result.uuid) . view #uuid)
  experiment <- use (#running % #elements) <&> (`Seq.index` experimentIdx)
  if result.proofFound == experiment.expectedResult
    then #uninteresting % #elements %= (Seq.|> result)
    else #interesting % #elements %= (Seq.|> result)
  #running % #elements %= Seq.deleteAt experimentIdx
appHandleEvent _ = pure ()

runTUI :: B.BChan ExperimentProgress -> IO ()
runTUI eventChan = do
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void $
    B.customMain
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
                  [ (B.buttonSelectedAttr, Vty.defAttr `Vty.withStyle` Vty.standout),
                    (B.attrName "selected", Vty.defAttr `Vty.withStyle` Vty.standout)
                  ]
              ),
          appHandleEvent,
          appStartEvent = pure ()
        }
      initialAppState
