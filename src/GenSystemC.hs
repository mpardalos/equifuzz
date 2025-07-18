{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}


module GenSystemC (
  -- * Generation
  genSystemC,
  generateFromProcess,
  GenerateProcess (..),

  -- ** Configuration
  OperationsMod,
  GenConfig (..),
  GenMods (..),
  TransformationFlags (..),
  allTransformations,
)
where

import Control.Monad (replicateM_)
import Control.Monad.Random.Strict (Rand, StdGen)
import Control.Monad.State.Strict (evalStateT, execState)
import Control.Monad.Writer.Strict (MonadWriter (tell), execWriterT)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Optics (Zoom (zoom), use, (%), _1, _2)
import SystemC qualified as SC
import Control.Monad (guard, join, when, replicateM)
import Control.Monad.Random.Strict (MonadRandom, getRandomR, uniform, uniformMay)
import Control.Monad.State.Strict (MonadState (get), modify')
import Data.Maybe (catMaybes, isJust)
import Optics.State.Operators ((%=), (.=))

-- Import modOperations cfg as SCUnconfigured.operations, because we need to make
-- sure to run its result through GenConfig.modOperations, and never use it
-- directly

import Data.Map qualified as Map
import SystemC qualified as SCUnconfigured (operations)
import Util (is)
import Data.Data (Data)
import Text.Show.Functions()

type OperationsMod = SC.Expr -> SC.Operations -> SC.Operations

data TransformationFlags = TransformationFlags
  { castWithAssignment :: Bool
  , functionalCast :: Bool
  , range :: Bool
  , arithmetic :: Bool
  , useAsCondition :: Bool
  , bitSelect :: Bool
  , applyMethod :: Bool
  , applyUnaryOp :: Bool
  }
  deriving (Show)

allTransformations :: TransformationFlags
allTransformations =
  TransformationFlags
    { castWithAssignment = True
    , functionalCast = True
    , range = True
    , arithmetic = True
    , useAsCondition = True
    , bitSelect = True
    , applyMethod = True
    , applyUnaryOp = True
    }

data GenMods = GenMods
  { operations :: OperationsMod
  , transformations :: TransformationFlags
  , inputs :: Bool
  }
  deriving (Show)

data GenConfig = GenConfig
  { growSteps :: Int
  , mods :: GenMods
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

assignAllowed :: GenConfig -> SC.Expr -> SC.SCType -> Bool
assignAllowed cfg expr toType =
  let flags = (modOperations cfg expr).assignTo
   in case toType of
        SC.SCInt{} -> flags.scInt
        SC.SCUInt{} -> flags.scUInt
        SC.SCBigInt{} -> flags.scBigInt
        SC.SCBigUInt{} -> flags.scBigUInt
        SC.SCFixed{} -> flags.scFixed
        SC.SCUFixed{} -> flags.scUFixed
        SC.SCFxnumSubref{} -> flags.scFxnumSubref
        SC.SCIntSubref{} -> flags.scIntSubref
        SC.SCUIntSubref{} -> flags.scUIntSubref
        SC.SCSignedSubref{} -> flags.scSignedSubref
        SC.SCUnsignedSubref{} -> flags.scUnsignedSubref
        SC.SCIntBitref -> flags.scIntBitref
        SC.SCUIntBitref -> flags.scUIntBitref
        SC.SCSignedBitref -> flags.scSignedBitref
        SC.SCUnsignedBitref -> flags.scUnsignedBitref
        SC.SCLogic -> flags.scLogic
        SC.SCBV{} -> flags.scBV
        SC.SCLV{} -> flags.scLV
        SC.CUInt -> flags.cUInt
        SC.CInt -> flags.cInt
        SC.CDouble -> flags.cDouble
        SC.CBool -> flags.cBool

castAllowed :: GenConfig -> SC.Expr -> SC.SCType -> Bool
castAllowed cfg expr toType =
  let flags = (modOperations cfg expr).constructorInto
   in case toType of
        SC.SCInt{} -> flags.scInt
        SC.SCUInt{} -> flags.scUInt
        SC.SCBigInt{} -> flags.scBigInt
        SC.SCBigUInt{} -> flags.scBigUInt
        SC.SCFixed{} -> flags.scFixed
        SC.SCUFixed{} -> flags.scUFixed
        SC.SCFxnumSubref{} -> flags.scFxnumSubref
        SC.SCIntSubref{} -> flags.scIntSubref
        SC.SCUIntSubref{} -> flags.scUIntSubref
        SC.SCSignedSubref{} -> flags.scSignedSubref
        SC.SCUnsignedSubref{} -> flags.scUnsignedSubref
        SC.SCIntBitref -> flags.scIntBitref
        SC.SCUIntBitref -> flags.scUIntBitref
        SC.SCSignedBitref -> flags.scSignedBitref
        SC.SCUnsignedBitref -> flags.scUnsignedBitref
        SC.SCLogic -> flags.scLogic
        SC.SCBV{} -> flags.scBV
        SC.SCLV{} -> flags.scLV
        SC.CUInt -> flags.cUInt
        SC.CInt -> flags.cInt
        SC.CDouble -> flags.cDouble
        SC.CBool -> flags.cBool

applyTransformation :: MonadBuild m => GenConfig -> Transformation -> m ()
applyTransformation cfg transformation
  -- First, look for free vars in any expression which is explicitly in the
  -- transformation. If there is some and inputs are disabled, skip the
  -- transformation
  | not cfg.mods.inputs && not (null (SC.freeVars transformation)) = pure ()
applyTransformation cfg (CastWithAssignment varType) = do
  e <- use #headExpr
  varName <- newVar
  when (assignAllowed cfg e varType) $ do
    #statements
      %= ( ++
            [ SC.Declaration varType varName
            , SC.Assignment varName e
            ]
         )
    #headExpr .= SC.Variable varType varName
applyTransformation cfg (FunctionalCast castType) =
  #headExpr %= \e ->
    if castAllowed cfg e castType
      then SC.Cast castType castType e
      else e
applyTransformation cfg (Range bound1 bound2) = do
  #headExpr %= \e ->
    let hi = max bound1 bound2
        lo = min bound1 bound2
        width = hi - lo + 1
        rangeInBounds = lo >= 0 && maybe True (hi <) (SC.knownWidth e.annotation)
     in case (modOperations cfg e).partSelect of
          Just resultType
            | rangeInBounds ->
                SC.MethodCall
                  (resultType width)
                  e
                  "range"
                  [SC.Constant SC.CInt (fromIntegral hi), SC.Constant SC.CInt (fromIntegral lo)]
          _ -> e
applyTransformation cfg (Arithmetic op e') =
  #headExpr %= \e ->
    case (modOperations cfg e).arithmeticResult of
      Just resultType -> SC.BinOp resultType e op e'
      Nothing -> e
applyTransformation cfg (UseAsCondition tExpr fExpr) = do
  #headExpr %= \e ->
    if SC.CBool `elem` (modOperations cfg e).implicitCasts
      then SC.Conditional tExpr.annotation e tExpr fExpr
      else e
applyTransformation cfg (BitSelect idx) = do
  #headExpr %= \e ->
    let idxInBounds = maybe True (idx <) (SC.knownWidth e.annotation)
     in case (modOperations cfg e).bitSelect of
          Just bitrefType | idxInBounds -> SC.Bitref bitrefType e (fromIntegral idx)
          _ -> e
applyTransformation cfg (ApplyMethod method) = do
  #headExpr %= \e ->
    case Map.lookup method (modOperations cfg e).methods of
      Just sig -> SC.MethodCall sig e (SC.methodName method) []
      Nothing -> e
applyTransformation cfg (ApplyUnaryOp op) = do
  #headExpr %= \e ->
    if (e `is` #_Variable) && (modOperations cfg e).incrementDecrement
      then SC.UnaryOp e.annotation op e
      else e

randomTransformationFor ::
  forall m.
  ( MonadRandom m
  , MonadState [(SC.VarName, SC.SCType)] m
  ) =>
  GenConfig ->
  SC.Expr ->
  m Transformation
randomTransformationFor cfg e
  | null transformationOptions = error ("No transformations possible on this expression: " <> show e)
  | otherwise = join . uniform $ transformationOptions
 where
    transformationOptions :: [m Transformation]
    transformationOptions = catMaybes
      [ guard cfg.mods.transformations.castWithAssignment >> castWithAssignment
      , guard cfg.mods.transformations.functionalCast >> functionalCast
      , guard cfg.mods.transformations.range >> range
#ifndef EVALUATION_VERSION
      , guard cfg.mods.transformations.arithmetic >> arithmetic
      , guard cfg.mods.transformations.useAsCondition >> useAsCondition
      , guard cfg.mods.transformations.bitSelect >> bitSelect
      , guard cfg.mods.transformations.applyMethod >> applyMethod
      , guard cfg.mods.transformations.applyUnaryOp >> applyUnaryOp
#endif
      ]
    castWithAssignment :: Maybe (m Transformation)
    castWithAssignment = fmap CastWithAssignment <$> assignmentCastTargetType

    functionalCast :: Maybe (m Transformation)
    functionalCast = fmap FunctionalCast <$> functionalCastTargetType

    someWidth :: m Int
    someWidth = getRandomR (1, 64)

    someBigWidth :: m Int
    someBigWidth = getRandomR (1, 512)

    someWI :: m (Int, Int)
    someWI = do
      w <- someWidth
      i <- getRandomR (0, w)
      return (w, i)

    assignmentCastTargetType :: Maybe (m SC.SCType)
    assignmentCastTargetType =
      let ts = (modOperations cfg e).assignTo
          gens :: [m SC.SCType]
          gens = catMaybes
            [ guard ts.scInt >> Just (SC.SCInt <$> someWidth)
            , guard ts.scInt >> Just (SC.SCInt <$> someWidth)
            , guard ts.scUInt >> Just (SC.SCUInt <$> someWidth)
            , guard ts.scBigInt >> Just (SC.SCBigInt <$> someBigWidth)
            , guard ts.scBigUInt >> Just (SC.SCBigUInt <$> someBigWidth)
            , guard ts.scFixed >> Just (uncurry SC.SCFixed <$> someWI )
            , guard ts.scUFixed >> Just (uncurry SC.SCUFixed <$> someWI )
            , guard ts.scLogic >> Just (pure SC.SCLogic)
            , guard ts.scBV >> Just (SC.SCBV <$> someWidth)
            , guard ts.scLV >> Just (SC.SCLV <$> someWidth)
            -- No subrefs or bitrefs because they cannot be constructed
            , guard ts.cUInt >> Just (pure SC.CUInt)
            , guard ts.cInt >> Just (pure SC.CInt)
            , guard ts.cDouble >> Just (pure SC.CDouble)
            , guard ts.cBool >> Just (pure SC.CBool)
            ]

       in if null gens
             then Nothing
             else Just $ join (uniform gens)

    functionalCastTargetType :: Maybe (m SC.SCType)
    functionalCastTargetType =
      let cs = (modOperations cfg e).constructorInto
          gens :: [m SC.SCType]
          gens = catMaybes
            [ guard cs.scInt >> Just (SC.SCInt <$> someWidth)
            , guard cs.scInt >> Just (SC.SCInt <$> someWidth)
            , guard cs.scUInt >> Just (SC.SCUInt <$> someWidth)
            , guard cs.scBigInt >> Just (SC.SCBigInt <$> someBigWidth)
            , guard cs.scBigUInt >> Just (SC.SCBigUInt <$> someBigWidth)
            , guard cs.scFixed >> Just (uncurry SC.SCFixed <$> someWI )
            , guard cs.scUFixed >> Just (uncurry SC.SCUFixed <$> someWI )
            , guard cs.scLogic >> Just (pure SC.SCLogic)
            , guard cs.scBV >> Just (SC.SCBV <$> someWidth)
            , guard cs.scLV >> Just (SC.SCLV <$> someWidth)
            -- No subrefs or bitrefs because they cannot be constructed
            , guard cs.cUInt >> Just (pure SC.CUInt)
            , guard cs.cInt >> Just (pure SC.CInt)
            , guard cs.cDouble >> Just (pure SC.CDouble)
            , guard cs.cBool >> Just (pure SC.CBool)
            ]

       in if null gens
             then Nothing
             else Just $ join (uniform gens)

    range :: Maybe (m Transformation)
    range = do
      guard (isJust (modOperations cfg e).partSelect)
      exprWidth <- SC.knownWidth e.annotation
      return $ do
        hi <- getRandomR (0, exprWidth - 1)
        lo <- getRandomR (0, hi)
        return (Range hi lo)

    arithmetic :: Maybe (m Transformation)
    arithmetic = do
      resultType <- (modOperations cfg e).arithmeticResult
      return $ do
        op <- uniform [SC.Plus, SC.Minus, SC.Multiply]
        constant <- someAtomicExpr resultType
        return (Arithmetic op constant)

    useAsCondition :: Maybe (m Transformation)
    useAsCondition = do
      guard (SC.CBool `elem` (modOperations cfg e).implicitCasts)
      return
        ( UseAsCondition
            <$> someAtomicExpr SC.CInt
            <*> someAtomicExpr SC.CInt
        )

    bitSelect :: Maybe (m Transformation)
    bitSelect = do
      guard (isJust (modOperations cfg e).bitSelect)
      width <- SC.knownWidth e.annotation
      return (BitSelect <$> getRandomR (0, width - 1))

    applyMethod :: Maybe (m Transformation)
    applyMethod = do
      let options = ApplyMethod <$> Map.keys (modOperations cfg e).methods
      guard (not . null $ options)
      return (uniform options)

    applyUnaryOp :: Maybe (m Transformation)
    applyUnaryOp = do
      guard (modOperations cfg e).incrementDecrement
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
      | typeAllowedAsInput t && cfg.mods.inputs = join . uniform $
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
    someVarName = T.pack <$> replicateM 5 (uniform ['a'..'z'])

seedExpr :: MonadRandom m => m SC.Expr
seedExpr = do
  value <- getRandomR (-128, 128)
  return (SC.Constant SC.CInt value)

modOperations :: GenConfig -> SC.Expr -> SC.Operations
modOperations cfg e = cfg.mods.operations e (SCUnconfigured.operations e)

genSystemC :: GenConfig -> Rand StdGen GenerateProcess
genSystemC cfg = do
  seed <- seedExpr
  transformations <- execWriterT . flip evalStateT (initBuildOutState seed, []) $
    replicateM_ cfg.growSteps $ do
      e <- use (_1 % #headExpr)
      transformation <- zoom _2 (randomTransformationFor cfg e)
      tell [transformation]
      zoom _1 (applyTransformation cfg transformation)

  return $ GenerateProcess cfg seed transformations

generateFromProcess :: GenConfig -> Text -> GenerateProcess -> SC.FunctionDeclaration
generateFromProcess cfg name GenerateProcess{seed, transformations} =
  let finalState = (`execState` initBuildOutState seed) $ do
        mapM_ (applyTransformation cfg) transformations
        -- This is only used here, in generateFromProcess. We want this to
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
