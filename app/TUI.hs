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
  LabelOptic "selectedElement" An_AffineTraversal (B.GenericList n t e) (B.GenericList n t e) e e
  where
  labelOptic = singular $ traversalVL B.listSelectedElementL

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
    interesting :: B.GenericList WidgetID Seq (Experiment, ExperimentResult),
    uninteresting :: B.GenericList WidgetID Seq (Experiment, ExperimentResult),
    focusedElementId :: WidgetID
  }

makeFieldLabelsNoPrefix ''AppState

selectedItem :: AffineFold AppState (Experiment, Maybe ExperimentResult)
selectedItem = afolding $ \st -> case st.focusedElementId of
  RunningList -> do
    experiment <- st.running ^? #selectedElement
    return (experiment, Nothing)
  InterestingList -> do
    (experiment, result) <- st.interesting ^? #selectedElement
    return (experiment, Just result)
  UninterestingList -> do
    (experiment, result) <- st.uninteresting ^? #selectedElement
    return (experiment, Just result)

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
  B.hBox
    [ B.hLimit 42 $
        B.vBox
          [ renderList "Running" #uuid st.running,
            renderList "Interesting" (_2 % #uuid) st.interesting,
            renderList "Uninteresting" (_2 % #uuid) st.uninteresting
          ],
      B.border $ case st ^? selectedItem of
        Just item -> renderSelection item
        Nothing -> B.padBottom B.Max $ B.vBox [B.str " ", B.hBorder]
    ]
  where
    renderSelection :: (Experiment, Maybe ExperimentResult) -> B.Widget WidgetID
    renderSelection (experiment, mResult) =
      B.padBottom B.Max
        . B.vBox
        $ [ B.str (show experiment.uuid),
            B.hBorder,
            B.str ("Expected result: " <> if experiment.expectedResult then "Equivalent" else "Non-equivalent")
          ]
          <> case mResult of
            Nothing -> []
            Just result ->
              [ B.str ("Actual result:   " <> if result.proofFound then "Equivalent" else "Non-equivalent")
              ]

    renderList :: Is k A_Getter => String -> Optic' k NoIx a UUID -> B.GenericList WidgetID Seq a -> B.Widget WidgetID
    renderList title uuid list =
      let listFocused = list ^. #name == st.focusedElementId
       in B.border
            . B.vBox
            $ [ (if listFocused then B.withAttr (B.attrName "selected") else id)
                  (B.str title),
                B.hBorder,
                B.renderList
                  (\selected it -> B.str ((if listFocused && selected then "→ " else "  ") <> show (it ^. uuid)))
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
appHandleEvent (B.AppEvent (Began experiment)) = #running % #elements %= (experiment Seq.<|)
appHandleEvent (B.AppEvent (Completed result)) = do
  experimentIdx <- use (#running % #elements) <&> fromJust . Seq.findIndexL ((== result.uuid) . view #uuid)
  experiment <- use (#running % #elements) <&> (`Seq.index` experimentIdx)
  if result.proofFound == experiment.expectedResult
    then #uninteresting % #elements %= ((experiment, result) Seq.<|)
    else #interesting % #elements %= ((experiment, result) Seq.<|)
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
