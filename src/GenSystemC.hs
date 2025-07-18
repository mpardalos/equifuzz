{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenSystemC (
  -- * Generation
  genSystemCProcess,
  generateProcessToSystemC,
  GenerateProcess (..),

  -- ** Configuration
  GenMods,
  Transformation(..),
  GenConfig (..),
)
where

import Control.Monad (guard, join, replicateM, replicateM_, when)
import Control.Monad.Random.Strict (MonadRandom, evalRandIO, getRandomR, uniform, uniformMay)
import Control.Monad.State.Strict (MonadState (get), evalStateT, execState, modify')
import Control.Monad.Writer.Strict (MonadWriter (tell), execWriterT)
import Data.List (nub)
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics (Zoom (zoom), use, (%), _1, _2)
import Optics.State.Operators ((%=), (.=))
import SystemC qualified as SC

-- Import modOperations cfg as SCUnconfigured.operations, because we need to make
-- sure to run its result through GenConfig.modOperations, and never use it
-- directly

import Data.Data (Data)
import Data.Map qualified as Map
import Debug.Trace (traceM)
import Text.Show.Functions ()
import Util (is)

type GenMods = SC.Expr -> Transformation -> Bool

data GenConfig = GenConfig
  { growSteps :: Int
  , transformationAllowed :: GenMods
  , evaluations :: Int
  }
  deriving (Show)

data Transformation
  = CastWithAssignment SC.SCType
  | FunctionalCast SC.SCType
  | Range Int Int
  | Arithmetic SC.BinOp SC.Expr
  | UseAsCondition SC.Expr SC.Expr
  | BitSelect Int
  | ApplyMethod SC.SCMethod
  | ApplyUnaryOp SC.UnaryOp
  deriving stock (Show, Generic, Data, Eq, Ord)

type MonadBuild m = MonadState BuildOutState m

data BuildOutState = BuildOutState
  { statements :: [SC.Statement]
  , headExpr :: SC.Expr
  , nextVarIdx :: Int
  }
  deriving (Generic)

initBuildOutState :: SC.Expr -> BuildOutState
initBuildOutState headExpr =
  BuildOutState
    { statements = []
    , headExpr
    , nextVarIdx = 0
    }

newVar :: MonadBuild m => m Text
newVar = do
  varIdx <- use #nextVarIdx
  #nextVarIdx %= (+ 1)
  return ("x" <> T.pack (show varIdx))

transformationAllowed :: SC.Expr -> Transformation -> Bool
transformationAllowed e (Range bound1 bound2) =
  let hi = max bound1 bound2
      lo = min bound1 bound2
   in lo >= 0 && maybe True (hi <) (SC.knownWidth e.annotation)
transformationAllowed e (BitSelect idx) =
  maybe True (idx <) (SC.knownWidth e.annotation)
transformationAllowed e UseAsCondition{} =
  SC.CBool `elem` (SC.operations e).implicitCasts
transformationAllowed e (ApplyUnaryOp _) =
  (e `is` #_Variable) && (SC.operations e).incrementDecrement
transformationAllowed _ _ = True

applyTransformation :: MonadBuild m => GenConfig -> Transformation -> m ()
applyTransformation cfg transformation = do
  -- First, look for free vars in any expression which is explicitly in the
  -- transformation. If there is some and inputs are disabled, skip the
  -- transformation
  e <- use #headExpr
  when
    ( transformationAllowed e transformation
        && cfg.transformationAllowed e transformation
    )
    $ applyTransformationUnchecked transformation

applyTransformationUnchecked :: MonadBuild m => Transformation -> m ()
applyTransformationUnchecked (CastWithAssignment varType) = do
  e <- use #headExpr
  varName <- newVar
  #statements
    %= ( ++
          [ SC.Declaration varType varName
          , SC.Assignment varName e
          ]
       )
  #headExpr .= SC.Variable varType varName
applyTransformationUnchecked (FunctionalCast castType) =
  #headExpr %= \e -> SC.Cast castType castType e
applyTransformationUnchecked (Range bound1 bound2) = do
  #headExpr %= \e ->
    let hi = max bound1 bound2
        lo = min bound1 bound2
        width = hi - lo + 1
     in case (SC.operations e).partSelect of
          Just resultType ->
            SC.MethodCall
              (resultType width)
              e
              "range"
              [SC.Constant SC.CInt (fromIntegral hi), SC.Constant SC.CInt (fromIntegral lo)]
          _ -> e
applyTransformationUnchecked (Arithmetic op e') =
  #headExpr %= \e ->
    case (SC.operations e).arithmeticResult of
      Just resultType -> SC.BinOp resultType e op e'
      Nothing -> e
applyTransformationUnchecked (UseAsCondition tExpr fExpr) = do
  #headExpr %= \e ->
    SC.Conditional tExpr.annotation e tExpr fExpr
applyTransformationUnchecked (BitSelect idx) = do
  #headExpr %= \e ->
    case (SC.operations e).bitSelect of
      Just bitrefType -> SC.Bitref bitrefType e (fromIntegral idx)
      _ -> e
applyTransformationUnchecked (ApplyMethod method) = do
  #headExpr %= \e ->
    case Map.lookup method (SC.operations e).methods of
      Just sig -> SC.MethodCall sig e (SC.methodName method) []
      Nothing -> e
applyTransformationUnchecked (ApplyUnaryOp op) = do
  #headExpr %= \e -> SC.UnaryOp e.annotation op e

randomTransformationFor ::
  forall m.
  ( MonadRandom m
  , MonadState [(SC.VarName, SC.SCType)] m
  ) =>
  GenConfig ->
  SC.Expr ->
  m Transformation
randomTransformationFor cfg e = go 0
 where
  go :: Int -> m Transformation
  go n = do
    when (n > 20) $
      traceM ("Tried " ++ show n ++ " times to generate an allowed transformation. Check the tool restrictions")
    when (n > 1000) $
      error ("Tried " ++ show n ++ " times to generate an allowed transformation. Something is definitely wrong. Bye.")
    t <- join . uniform $ transformationOptions
    if transformationAllowed e t && cfg.transformationAllowed e t
      then return t
      else go (n + 1)

  transformationOptions :: [m Transformation]
  transformationOptions =
    catMaybes
      [ Just castWithAssignment
      , Just functionalCast
      , range
      , arithmetic
      , Just useAsCondition
      , bitSelect
      , applyMethod
      , applyUnaryOp
      ]

  castWithAssignment :: m Transformation
  castWithAssignment = CastWithAssignment <$> castTargetType

  functionalCast :: m Transformation
  functionalCast = FunctionalCast <$> castTargetType

  someWidth :: m Int
  someWidth = getRandomR (1, 64)

  someBigWidth :: m Int
  someBigWidth = getRandomR (1, 512)

  someWI :: m (Int, Int)
  someWI = do
    w <- someWidth
    i <- getRandomR (0, w)
    return (w, i)

  castTargetType :: m SC.SCType
  castTargetType =
    join . uniform $
      [ SC.SCInt <$> someWidth
      , SC.SCInt <$> someWidth
      , SC.SCUInt <$> someWidth
      , SC.SCBigInt <$> someBigWidth
      , SC.SCBigUInt <$> someBigWidth
      , uncurry SC.SCFixed <$> someWI
      , uncurry SC.SCUFixed <$> someWI
      , pure SC.SCLogic
      , SC.SCBV <$> someWidth
      , SC.SCLV <$> someWidth
      , -- No subrefs or bitrefs because they cannot be constructed
        pure SC.CUInt
      , pure SC.CInt
      , pure SC.CDouble
      , pure SC.CBool
      ]

  range :: Maybe (m Transformation)
  range = do
    exprWidth <- SC.knownWidth e.annotation
    return $ do
      hi <- getRandomR (0, exprWidth - 1)
      lo <- getRandomR (0, hi)
      return (Range hi lo)

  arithmetic :: Maybe (m Transformation)
  arithmetic = do
    resultType <- (SC.operations e).arithmeticResult
    return $ do
      op <- uniform [SC.Plus, SC.Minus, SC.Multiply]
      constant <- someAtomicExpr resultType
      return (Arithmetic op constant)

  useAsCondition :: m Transformation
  useAsCondition =
    UseAsCondition
      <$> someAtomicExpr SC.CInt
      <*> someAtomicExpr SC.CInt

  bitSelect :: Maybe (m Transformation)
  bitSelect = do
    guard (isJust (SC.operations e).bitSelect)
    width <- SC.knownWidth e.annotation
    return (BitSelect <$> getRandomR (0, width - 1))

  applyMethod :: Maybe (m Transformation)
  applyMethod = do
    let options = ApplyMethod <$> Map.keys (SC.operations e).methods
    guard (not . null $ options)
    return (uniform options)

  applyUnaryOp :: Maybe (m Transformation)
  applyUnaryOp = do
    guard (SC.operations e).incrementDecrement
    guard (e `is` #_Variable)
    return (ApplyUnaryOp <$> uniform [minBound :: SC.UnaryOp .. maxBound])

  -- TODO: Some systemc types cannot be reliably constructed from literals on
  -- all equivalence checkers, and hence we prevent them from appearing as
  -- inputs (where they would need to be constructed from literals).  This
  -- should, really, be a per-EC setting, but that then means that we need to
  -- vary how literals are constructed in `limitToEvaluations` per-EC. That
  -- function is in the "experiment generation" part of the code, and
  -- therefore not easily configurable per-EC. So we stick to lowest common
  -- denominator of all ECs here and also in `limitToEvaluations`.
  typeAllowedAsInput :: SC.SCType -> Bool
  typeAllowedAsInput (SC.SCBigInt n) = n <= 64
  typeAllowedAsInput (SC.SCBigUInt n) = n <= 64
  typeAllowedAsInput SC.SCFixed{} = False
  typeAllowedAsInput SC.SCUFixed{} = False
  typeAllowedAsInput _ = True

  someAtomicExpr :: SC.SCType -> m SC.Expr
  someAtomicExpr t
    | typeAllowedAsInput t =
        join . uniform $
          [ someConstant t
          , someExistingVar t
          , someNewVar t
          ]
    | otherwise = someConstant t

  someConstant :: SC.SCType -> m SC.Expr
  someConstant t = SC.Constant t <$> getRandomR (-1024, 1024)

  someExistingVar :: SC.SCType -> m SC.Expr
  someExistingVar t = do
    existingVars <- get
    uniformMay [n | (n, t') <- existingVars, t' == t] >>= \case
      Just n -> return (SC.Variable t n)
      Nothing -> someNewVar t

  someNewVar :: SC.SCType -> m SC.Expr
  someNewVar t = do
    existingVars <- get
    name <- someVarName
    if name `elem` map fst existingVars
      -- Yes, maybe an infinite loop, but don't worry about it.
      -- After enough tries we will get an actually fresh name
      then someNewVar t
      else do
        modify' ((name, t) :)
        return (SC.Variable t name)

  someVarName :: m Text
  someVarName = T.pack <$> replicateM 5 (uniform ['a' .. 'z'])

seedExpr :: MonadRandom m => m SC.Expr
seedExpr = do
  value <- getRandomR (-128, 128)
  return (SC.Constant SC.CInt value)

genSystemCProcess :: GenConfig -> IO GenerateProcess
genSystemCProcess cfg = evalRandIO $ do
  seed <- seedExpr
  transformations <- execWriterT . flip evalStateT (initBuildOutState seed, []) $
    replicateM_ cfg.growSteps $ do
      e <- use (_1 % #headExpr)
      transformation <- zoom _2 (randomTransformationFor cfg e)
      tell [transformation]
      zoom _1 (applyTransformation cfg transformation)

  return $ GenerateProcess cfg seed transformations

generateProcessToSystemC :: GenConfig -> Text -> GenerateProcess -> SC.FunctionDeclaration
generateProcessToSystemC cfg name GenerateProcess{seed, transformations} =
  let finalState = (`execState` initBuildOutState seed) $ do
        mapM_ (applyTransformation cfg) transformations
        -- This is only used here, in generateProcessToSystemC. We want this to
        -- \*not* be in the GenerateProcess, so that it is dynamically added in
        -- the reduced experiments, depending on what the reduction has left as the final expression.
        finalizeIfNeeded
      body = finalState.statements ++ [SC.Return finalState.headExpr]
      args = nub [(t, v) | (t, v) <- SC.freeVars body, T.length v == 5]
   in SC.FunctionDeclaration
        { returnType = finalState.headExpr.annotation
        , name
        , args
        , body
        }
 where
  finalizeIfNeeded :: MonadBuild m => m ()
  finalizeIfNeeded =
    use (#headExpr % #annotation) >>= \case
      SC.SCInt{} -> pure ()
      SC.SCFixed{} -> pure ()
      SC.SCUInt{} -> pure ()
      SC.SCUFixed{} -> pure ()
      SC.SCBigInt _ -> pure ()
      SC.SCBigUInt _ -> pure ()
      SC.SCLogic -> pure ()
      SC.SCBV{} -> pure ()
      SC.SCLV{} -> pure ()
      -- Explicitly cast native types
      SC.CInt -> applyTransformation cfg (FunctionalCast SC.CInt)
      SC.CUInt -> applyTransformation cfg (FunctionalCast SC.CUInt)
      SC.CDouble -> applyTransformation cfg (FunctionalCast SC.CDouble)
      SC.CBool -> pure ()
      SC.SCFxnumSubref{width} -> applyTransformation cfg (FunctionalCast (SC.SCUInt width))
      SC.SCIntSubref{width} -> applyTransformation cfg (FunctionalCast (SC.SCUInt width))
      SC.SCUIntSubref{width} -> applyTransformation cfg (FunctionalCast (SC.SCUInt width))
      SC.SCSignedSubref{width} -> applyTransformation cfg (FunctionalCast (SC.SCBigInt width))
      SC.SCUnsignedSubref{width} -> applyTransformation cfg (FunctionalCast (SC.SCBigUInt width))
      SC.SCIntBitref -> applyTransformation cfg (FunctionalCast SC.CBool)
      SC.SCUIntBitref -> applyTransformation cfg (FunctionalCast SC.CBool)
      SC.SCSignedBitref -> applyTransformation cfg (FunctionalCast SC.CBool)
      SC.SCUnsignedBitref -> applyTransformation cfg (FunctionalCast SC.CBool)

data GenerateProcess = GenerateProcess
  { cfg :: GenConfig
  , seed :: SC.Expr
  , transformations :: [Transformation]
  }
  deriving (Generic, Show)
