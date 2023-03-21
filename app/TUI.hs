{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TUI (runTUI, AppEvent (..)) where

import Brick qualified as B
import Brick.BChan qualified as B
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as B
import Brick.Widgets.Center qualified as B
import Brick.Widgets.Dialog qualified as B
import Brick.Widgets.List qualified as B
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Experiments
import Graphics.Vty qualified as Vty
import Optics
import Optics.State.Operators ((%=))
import Shelly qualified as Sh

instance LabelOptic "name" A_Lens (B.GenericList n t e) (B.GenericList n t e) n n where
  labelOptic = lensVL B.listNameL

instance LabelOptic "elements" A_Lens (B.GenericList n t e) (B.GenericList n t e) (t e) (t e) where
  labelOptic = lensVL B.listElementsL

instance
  (B.Splittable t, Traversable t, Semigroup (t e)) =>
  LabelOptic "selectedElement" An_AffineTraversal (B.GenericList n t e) (B.GenericList n t e) e e
  where
  labelOptic = singular $ traversalVL B.listSelectedElementL

data ListID
  = RunningList
  | InterestingList
  | UninterestingList
  deriving (Eq, Ord, Show, Enum, Bounded)

data WidgetID
  = ListID ListID
  | OutputViewport
  deriving (Eq, Ord, Show)

data AppEvent = ExperimentProgress ExperimentProgress

cycleNext :: (Enum a, Bounded a, Eq a) => a -> a
cycleNext x
  | x == maxBound = minBound
  | otherwise = succ x

cyclePrevious :: (Enum a, Bounded a, Eq a) => a -> a
cyclePrevious x
  | x == minBound = maxBound
  | otherwise = pred x

data ExperimentInfo = ExperimentInfo
  { experiment :: Experiment,
    result :: Maybe ExperimentResult,
    modulesDiff :: Maybe Text
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''ExperimentInfo

data AppState = AppState
  { running :: B.GenericList WidgetID Seq ExperimentInfo,
    interesting :: B.GenericList WidgetID Seq ExperimentInfo,
    uninteresting :: B.GenericList WidgetID Seq ExperimentInfo,
    focusedListId :: ListID
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''AppState

focusedList :: Getter AppState (B.GenericList WidgetID Seq ExperimentInfo)
focusedList =
  to
    ( \st -> case st.focusedListId of
        RunningList -> st.running
        InterestingList -> st.interesting
        UninterestingList -> st.uninteresting
    )

selectedItem :: AffineFold AppState ExperimentInfo
selectedItem = focusedList % #selectedElement

initialAppState :: AppState
initialAppState =
  AppState
    { running = B.list (ListID RunningList) Seq.empty 1,
      interesting = B.list (ListID InterestingList) Seq.empty 1,
      uninteresting = B.list (ListID UninterestingList) Seq.empty 1,
      focusedListId = RunningList
    }

appDraw :: AppState -> B.Widget WidgetID
appDraw st =
  B.hBox
    [ B.hLimit 42 $
        B.vBox
          [ renderList "Running" st.running,
            renderList "Interesting" st.interesting,
            renderList "Uninteresting" st.uninteresting
          ],
      case st ^? selectedItem of
        Just item -> renderSelection item
        Nothing -> B.padBottom B.Max $ B.vBox [B.strWrap " "]
    ]
  where
    renderSelection :: ExperimentInfo -> B.Widget WidgetID
    renderSelection (ExperimentInfo experiment mResult mDiff) =
      let start =
            B.vBox
              [ B.str ("UUID:            " <> show experiment.uuid),
                B.str ("Expected result: " <> if experiment.expectedResult then "Equivalent" else "Non-equivalent")
              ]
          resultStuff = case mResult of
            Just result ->
              B.str
                ( "Actual result:   " <> case result.proofFound of
                    Just True -> "Equivalent"
                    Just False -> "Non-equivalent"
                    Nothing -> "Unknown"
                )
            Nothing -> B.emptyWidget
          diffDisplay =
            maybe
              B.emptyWidget
              ( B.borderWithLabel (B.str " Diff ")
                  . B.txtWrap
              )
              mDiff
          outputDisplay =
            maybe
              B.emptyWidget
              ( B.borderWithLabel (B.str " Output ")
                  . B.withVScrollBars B.OnRight
                  . B.viewport OutputViewport B.Vertical
                  . B.txtWrap
                  . view #fullOutput
              )
              mResult
       in B.padBottom B.Max . B.vBox $
            [ B.border $ B.padRight B.Max $ start B.<=> resultStuff,
              diffDisplay,
              outputDisplay
            ]

    renderList :: String -> B.GenericList WidgetID Seq ExperimentInfo -> B.Widget WidgetID
    renderList title list =
      let listFocused = list ^. #name == ListID st.focusedListId
          headerText = B.str (title <> " (" <> show (length (list ^. #elements)) <> ")")
          setHeaderAttr = if listFocused then B.withAttr (B.attrName "selected") else id
          itemMarker itemSelected = if listFocused && itemSelected then "→ " else "  "
          renderItem selected item = B.str (itemMarker selected <> show item.experiment.uuid)
       in B.borderWithLabel (setHeaderAttr headerText) $
            B.renderList renderItem listFocused list

appHandleEvent :: B.BrickEvent WidgetID AppEvent -> B.EventM WidgetID AppState ()
appHandleEvent (B.VtyEvent ev) = case ev of
  (Vty.EvKey (Vty.KChar 'q') _) -> B.halt
  (Vty.EvKey (Vty.KChar 'r') _) -> liftIO . Vty.refresh =<< B.getVtyHandle
  (Vty.EvKey (Vty.KChar '\t') []) -> #focusedListId %= cycleNext
  (Vty.EvKey Vty.KBackTab []) -> #focusedListId %= cyclePrevious
  (Vty.EvKey Vty.KPageUp []) -> B.vScrollPage (B.viewportScroll OutputViewport) B.Up
  (Vty.EvKey Vty.KPageDown []) -> B.vScrollPage (B.viewportScroll OutputViewport) B.Down
  _ ->
    use #focusedListId >>= \case
      RunningList -> B.zoom (toLensVL #running) (B.handleListEvent ev)
      InterestingList -> B.zoom (toLensVL #interesting) (B.handleListEvent ev)
      UninterestingList -> B.zoom (toLensVL #uninteresting) (B.handleListEvent ev)
appHandleEvent (B.AppEvent ev) = case ev of
  (ExperimentProgress (Began experiment)) -> do
    diff <- liftIO $ getDiff experiment
    #running %= B.listInsert 0 (ExperimentInfo experiment Nothing diff)
  (ExperimentProgress (Completed result)) -> do
    experimentIdx <- use (#running % #elements) <&> fromJust . Seq.findIndexL ((== result.uuid) . view (#experiment % #uuid))
    experimentInfo <- use (#running % #elements) <&> (`Seq.index` experimentIdx)
    if result.proofFound == Just experimentInfo.experiment.expectedResult
      then #uninteresting %= B.listInsert 0 (experimentInfo {result = Just result})
      else #interesting %= B.listInsert 0 (experimentInfo {result = Just result})
    #running %= B.listRemove experimentIdx
  (ExperimentProgress (Aborted experiment)) -> do
    runningExperiments <- use (#running % #elements)
    case Seq.findIndexL ((== experiment.uuid) . view (#experiment % #uuid)) runningExperiments of
      Just idx -> #running %= B.listRemove idx
      Nothing -> pure ()
appHandleEvent B.MouseDown {} = pure ()
appHandleEvent B.MouseUp {} = pure ()

-- | Run the experiment's modules through a text diff
getDiff :: Experiment -> IO (Maybe Text)
getDiff Experiment {design1, design2} = do
  Sh.shelly . Sh.silently $ do
    diffExists <- isJust <$> Sh.which "diff"
    if diffExists
      then do
        tmpdir <- T.strip <$> Sh.run "mktemp" ["-d"]
        let path1 = tmpdir Sh.</> ("design1.v" :: FilePath)
        let path2 = tmpdir Sh.</> ("design2.v" :: FilePath)
        Sh.writefile path1 design1.source
        Sh.writefile path2 design2.source
        Sh.errExit False $
          Just <$> Sh.run "diff" [T.pack path1, T.pack path2]
      else return Nothing

runTUI :: B.BChan AppEvent -> IO ()
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
