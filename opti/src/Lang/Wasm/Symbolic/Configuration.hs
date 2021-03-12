{-# LANGUAGE ScopedTypeVariables, UnicodeSyntax, TupleSections, TemplateHaskell,
             GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses
#-}

-- | A Configuration is a set of program states with the same static structure.
-- As this state is usually /very large/, it is symbolically represented.
module Lang.Wasm.Symbolic.Configuration
  ( Configuration (..)
  , ConfigurationCore (..)
  , Activation
  , Global
  , Stack
  , init
  , initWithStack
  , toTop
  , environment
  , union
  , popI32
  , popI64
  , pushI32
  , pushI64
    -- * Lenses
  , cfgCore, cfgSymbolics
  , ccActivation, ccStack, ccGlobals, ccMem, ccConstraint
  , cfgActivation, cfgStack, cfgGlobals, cfgMem, cfgConstraint
  ) where

import           Prelude hiding ( init )
import           Melude
-- Stdlib imports
import qualified Data.List.NonEmpty as NE
-- Extra stdlib imports
import qualified Control.Monad.State as S
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( ValType (..), GlobalType, Limits, WI32 (..), TI32, TI64, TF32, TF64, Sx (..)
  , PVal (..), valType )
import qualified Lang.Wasm.Data as WD
import           Lang.Wasm.Data
-- Local imports
import qualified Lang.Wasm.Symbolics as Y
import           Lang.Wasm.Symbolics
  ( Symbolic, Symbolics, Environment, MonadSymbolics )
import qualified Lang.Wasm.Symbolic.General as SG
import           Lang.Wasm.Symbolic.General
  ( StaticStructure (..), LocalStaticStructure (..), SymbolicMem (..) )
--   ( Activation (..), Stack (..), Mem (..), Global (..), StaticStructure (..)
--   , LocalStaticStructure (..), saParams, saLocals, smLimits
--   )
import           Lang.Wasm.Algebra ( SimpleAlgebra (..), MonadWasmState (..) )
import Lang.Wasm.Markers ( TI33, TMem )


import System.IO.Unsafe ( unsafePerformIO )

-- # Data Structures #

-- | A configuration is a set of states. A state is a concrete assignment to
-- values. This configuration is symbolically represented.
--
-- The marker does nothing, except for being a convenience in preventing
-- configurations with different markers to get tangled.
data Configuration marker =
  Configuration {
    _cfgCore        :: ConfigurationCore marker
    -- A configuration contains its own "environment"
  , _cfgSymbolics   :: Symbolics marker
  }
  deriving Show

data ConfigurationCore marker =
  ConfigurationCore {
    _ccActivation  :: Activation marker
  , _ccStack       :: Stack marker
  , _ccGlobals     :: IdList (Global marker)
    -- | A program memory. Not all programs have a memory.
  , _ccMem         :: Maybe (SymbolicMem marker)
  , _ccConstraint  :: Symbolic marker Bool
  }
  deriving Show

-- | Symbolic activation set
type Activation env =
  PActivation (Symbolic env TI32) (Symbolic env TI64) () ()

-- | Symbolic global set
type Global env =
  PGlobal (Symbolic env TI32) (Symbolic env TI64) () ()

-- | Symbolic stack set
type Stack env =
  PStack (Symbolic env TI32) (Symbolic env TI64) () ()

$(makeLenses ''Configuration)
$(makeLenses ''ConfigurationCore)
$(makeLenses ''SymbolicMem)

cfgActivation :: Lens' (Configuration env) (Activation env)
cfgActivation = cfgCore . ccActivation

cfgStack :: Lens' (Configuration env) (Stack env)
cfgStack = cfgCore . ccStack

cfgGlobals :: Lens' (Configuration env) (IdList (Global env))
cfgGlobals = cfgCore . ccGlobals

cfgMem :: Lens' (Configuration env) (Maybe (SymbolicMem env))
cfgMem = cfgCore . ccMem

cfgConstraint :: Lens' (Configuration env) (Symbolic env Bool)
cfgConstraint = cfgCore . ccConstraint

-- |
--
popI32 :: Configuration env -> Maybe (Symbolic env TI32, Configuration env)
popI32 c =
  do
    (v, s) <- WD.popStackI32 (c^.cfgStack)
    return (v, c & cfgStack .~ s)
    
popI64 :: Configuration env -> Maybe (Symbolic env TI64, Configuration env)
popI64 c =
  do
    (v, s) <- WD.popStackI64 (c^.cfgStack)
    return (v, c & cfgStack .~ s)

-- |
--
-- WARNING: The symbolic I32 must be in the configuration's symbolic environment
pushI32 :: Symbolic env TI32 -> Configuration env -> Configuration env
pushI32 v c = c & cfgStack %~ WD.pushStackI32 v

-- |
--
-- WARNING: The symbolic I32 must be in the configuration's symbolic environment
pushI64 :: Symbolic env TI64 -> Configuration env -> Configuration env
pushI64 v c = c & cfgStack %~ WD.pushStackI64 v

-- staticStructure :: Configuration env -> StaticStructure
-- staticStructure s =
--   let a = s^.cfgActivation
--   in
--   StaticStructure
--     (Just $ LocalStaticStructure
--       (map Ast.valType (IdList.toList (WD.aParams a) ++ IdList.toList (WD.aLocals a)))
--       (NE.map (map Ast.valType) (s^.cfgStack))
--     )
--     (map (Ast.valType . WD.gVal) $ IdList.toList (s^.cfgGlobals))
--     (isJust (s^.cfgMem))

-- extractStateCore :: Configuration env
--                  -> (Symbolics env, ProgStateCore env)
-- extractStateCore c =
--   -- It's trapped or unreachable whenever the constraint is False
--   let (isTrapped, sym') = S.runState (Y.vBNot (c^.cfgConstraint)) (c^.cfgSymbolics)
--   in
--   ( sym'
--   , SG.ProgStateCore
--       isTrapped
--       (c^.cfgActivation)
--       (c^.cfgStack)
--       (c^.cfgGlobals)
--       (c^.cfgMem)
--   )

environment :: Configuration env -> Environment env
environment c = (c^.cfgSymbolics, c^.cfgCore.ccConstraint)

-- fromStateCore :: (Symbolics env, SG.ProgStateCore env)
--               -> Configuration env
-- fromStateCore (sym, c) =
--   let (constraint, sym') = S.runState (Y.vBNot (c^. SG.scIsTrapped)) sym
--   in
--   Configuration
--     (c ^. SG.scActivation)
--     (c ^. SG.scStack)
--     (c ^. SG.scGlobals)
--     (c ^. SG.scMem)
--     sym'
--     constraint

-- | Creates a fresh configuration with the specified static structure.
init :: [GlobalType] -- ^ Global variable types
     -> [ValType]    -- ^ Parameter types
     -> [ValType]    -- ^ Local types
     -> Maybe Limits -- ^ Memory bounds (no memory if `Nothing`)
     -> Configuration env
init globals params locals memSize =
  initWithStack globals params locals True memSize ([]:|[[]])

-- | Creates a fresh configuration with the specified static structure.
initWithStack :: [GlobalType]       -- ^ Global variable types
              -> [ValType]          -- ^ Parameter types
              -> [ValType]          -- ^ Local types
              -> Bool               -- ^ Are locals zero
              -> Maybe Limits       -- ^ Memory bounds (no memory if `Nothing`)
              -> NonEmpty [ValType] -- ^ Stack values (head is top)
              -> Configuration env
initWithStack globals params locals areLocalsZero memSize stackTypes =
  uncurry Configuration $ Y.runSymbolicsState init' Y.empty
  where
  init' :: Y.SymbolicsState env (ConfigurationCore env)
  init' =
    do
      -- locals
      paramVals <- IdList.fromList <$> mapM Y.vUnknown params
      localVals <-
        if areLocalsZero then
          IdList.fromList <$> mapM Y.vConstZero locals
        else
          IdList.fromList <$> mapM Y.vUnknown locals
      stack <- mapM (mapM Y.vUnknown) stackTypes

      -- globals
      globalVals <- IdList.fromList <$> mapM initGlobal globals

      -- memory
      (mem, constraint) <-
        case memSize of
          Just mem' -> mapFst Just <$> initMem mem'
          Nothing   -> (Nothing,) <$> Y.vBoolConst True
      
      return $
        ConfigurationCore
          (PActivation paramVals localVals)
          stack
          globalVals
          mem
          constraint
  -- | Creates a memory with corresponding constraint from the memory limits.
  initMem :: Limits
          -> Y.SymbolicsState env (SymbolicMem env, Symbolic env Bool)
  initMem limits =
    do
      memPageCount <- Y.vI32Unknown
      memConstraint <- constrainMemSize limits memPageCount
      memData <- Y.vMemUnknown
      return (SymbolicMem memData memPageCount limits, memConstraint)
  -- | Create an unknown global with the specified type
  initGlobal :: GlobalType -> Y.SymbolicsState env (Global env)
  initGlobal gt = WD.PGlobal (Ast.gtMut gt) <$> Y.vUnknown (Ast.gtType gt)

-- | Returns a constraint on the number of pages by the static limits
constrainMemSize :: MonadSymbolics env m
                 => Limits
                 -> Symbolic env TI32
                 -> m (Symbolic env Bool)
constrainMemSize limits numPages =
  do
    let (memMin, memMax) = realLimits limits
    memLower <- Y.vI32Const (WI32 $ toNum memMin)
    memUpper <- Y.vI32Const (WI32 $ toNum memMax)
    -- memMin <= memSize <= memMax
    memConstraint1 <- Y.vI32Le U memLower numPages
    memConstraint2 <- Y.vI32Le U numPages memUpper
    Y.vBAnd memConstraint1 memConstraint2

-- | Converts the `Configuration` into another `Configuration` with identical
-- structure, which is the "top" of the lattice. This is the /least constrained/
-- `Configuration`. That is, any other `Configuration` with identical static
-- structure is a (non-strict) subset of the top `Configuration`.
toTop :: Configuration env -> Configuration env2
toTop c =
  initWithStack
    (map WD.pGlobalType $ IdList.toList (c^.cfgGlobals))
    (map valType $ IdList.toList $ WD.aParams (c^.cfgActivation))
    (map valType $ IdList.toList $ WD.aLocals (c^.cfgActivation))
    False
    ((c^.cfgMem) <&> view smLimits)
    (WD.stackTypes (c^.cfgStack))

-- | See documentation of `Limits`. No more than 128*1024 /pages/ are possible
-- (as that reaches 8 GiB). Produces the limits in the number of 64KiB /pages/.
realLimits :: Limits -> (Int, Int)
realLimits l =
  let low = clamp 0 (128*1024) (Ast.lMin l)
  in
  ( fromInteger low
  , fromInteger $ clamp low (128*1024) (fromMaybe (128*1024) $ Ast.lMax l)
  )

type EnvId env = Int

union :: forall m a b . MonadFail m => Configuration a -> Configuration b -> m (Configuration a)
union a b =
  do
    -- trace ("X " ++ show (a^.cfgStack) ++ " " ++ show (b^.cfgStack)) $ return ()
    -- (aCore, env)  <- Y.runCombiner (rebuildCore (a^.cfgCore)) (a^.cfgSymbolics) Y.empty
    -- trace "Y" $ return ()
    let (aCore, env) = (a^.cfgCore, a^.cfgSymbolics)
    (bCore, env') <- Y.runCombiner (rebuildCore (b^.cfgCore)) (b^.cfgSymbolics) env
    -- trace "Z" $ return ()
    (mCCore, env'') <- Y.runSymbolicsStateT (runMaybeT $ unionCore aCore bCore) env'
    -- trace ("W " ++ show (isJust mCCore)) $ return ()
    cCore <- failMaybeMsg "Statically unequal configurations" mCCore
    return $ Configuration cCore env''
  where
  rebuildCore :: ConfigurationCore d -> Y.SymbolicsCombiner d e m (ConfigurationCore e)
  rebuildCore a =
    do
      constraint <- Y.combineBool (a^.ccConstraint)
      activation <- WD.mapActivationVal Y.combineVal (a^.ccActivation)
      stack <- WD.mapStackVal Y.combineVal (a^.ccStack)
      globals <- mapM (WD.mapGlobalValM Y.combineVal) (a^.ccGlobals)
      mem <- rebuildMem (a^.ccMem)
      return $ ConfigurationCore activation stack globals mem constraint
  rebuildMem :: Maybe (SymbolicMem d) -> Y.SymbolicsCombiner d e m (Maybe (SymbolicMem e))
  rebuildMem Nothing = return Nothing
  rebuildMem (Just mem) =
    do
      d <- Y.combineMem (mem^.smData)
      pc <- Y.combineI32 (mem^.smPageCount)
      return $ Just $ SymbolicMem d pc (mem^.smLimits)
  unionCore :: MonadSymbolics c m2
            => ConfigurationCore c
            -> ConfigurationCore c
            -> MaybeT m2 (ConfigurationCore c)
  unionCore a b =
    do
      constraint <- Y.vBOr (a^.ccConstraint) (b^.ccConstraint)
      -- Under the combined path constraint, `aCond` represents the condition on
      -- set represented by `a` contained within their union.
      -- aCond <- Y.vBAnd (a^.ccConstraint) =<< join (Y.vBOr <$> Y.vBNot (b^.ccConstraint) <*> Y.vBoolUnknown)
      let aCond = a^.ccConstraint
      activation <- selectActivation aCond (a^.ccActivation) (b^.ccActivation)
      stack <- selectStack aCond (a^.ccStack) (b^.ccStack)
      globals <- IdList.zipWithExactM (selectGlobal aCond) (a^.ccGlobals) (b^.ccGlobals)
      mem <- selectMem aCond (a^.ccMem) (b^.ccMem)
      return $ ConfigurationCore activation stack globals mem constraint

-- | Alias: @Symbolic env Bool -> val -> val -> MaybeT m val@
--
-- Alias for symbolic selection functions (i.e., logical if-then-else)
-- @select True a b = a@, @select False a b = b@.
--
-- This may fail over the `MaybeT` when the values have mismatching static
-- structures.
type Selection env val =
  forall m . MonadSymbolics env m => Symbolic env Bool -> val -> val -> MaybeT m val

selectActivation :: Selection env (Activation env)
selectActivation = WD.zipActivationVal . selectVal

selectStack :: Selection env (Stack env)
selectStack = WD.zipStackVal . selectVal

selectGlobal :: Selection env (Global env)
selectGlobal = WD.zipGlobalVal . selectVal

selectMem :: Selection env (Maybe (SymbolicMem env))
selectMem c (Just a) Nothing  = fail "Statically unequal memories"
selectMem c Nothing  (Just b) = fail "Statically unequal memories"
selectMem c Nothing  Nothing  = return Nothing
selectMem c (Just a) (Just b) =
  do
    failIf ((a^.smLimits) /= (b^.smLimits)) "Statically unequal memories"
    d <- Y.vMemSelect c (a^.smData) (b^.smData)
    pc <- Y.vI32Select c (a^.smPageCount) (b^.smPageCount)
    return $ Just $ SymbolicMem d pc (a^.smLimits)

selectVal :: Selection env (Y.SymbolicVal env)
selectVal c =
  Ast.zipValM
    (Y.vI32Select c)
    (Y.vI64Select c)
    (\_ _ -> return ())
    (\_ _ -> return ())


-- # Class Instances #

instance Y.MonadSymbolics env m =>
            MonadWasmState
              (Symbolic env TI32)
              (Symbolic env TI64)
              ()
              ()
              (Maybe (SymbolicMem env))
              (StateT (ConfigurationCore env) (MaybeT m)) where
  popStack  =
    withStateLens ccStack $ StateT WD.popStackVal

  peekStack =
    withStateLens ccStack $ getsM WD.peekStackVal

  pushStack =
    withStateLens ccStack . S.modify . WD.pushStack

  pushLabel =
    withStateLens ccStack $ S.modify $ WD.pushStackLabel []

  popLabel =
    withStateLens ccStack $ modifyM WD.popStackLabel

  getLocal =
    withStateLens ccActivation . getsM . WD.getLocal

  setLocal =
    withStateLens ccActivation .: modifyM .: WD.setLocal

  getGlobal =
    withStateLens ccGlobals . getsM . WD.getGlobal

  setGlobal =
    withStateLens ccGlobals .: modifyM .: WD.setGlobalForce
  
  getGlobals =
    S.gets (map WD.gVal . IdList.toList . view ccGlobals)

  getMemory =
    S.gets (view ccMem)
  
  setMemory m =
    S.modify $ \s -> s & ccMem .~ m

  storeI32   = modifyM .: storeAny Y.vI32Store
  storeI64   = modifyM .: storeAny Y.vI64Store
  storeF32 addr ()  = modifyM . storeAny Y.vI32Store addr =<< lift Y.vI32Unknown
  storeF64 addr ()  = modifyM . storeAny Y.vI64Store addr =<< lift Y.vI64Unknown
  store8I32  = modifyM .: storeAny Y.vI32Store8
  store16I32 = modifyM .: storeAny Y.vI32Store16
  store8I64  = modifyM .: storeAny Y.vI64Store8
  store16I64 = modifyM .: storeAny Y.vI64Store16
  store32I64 = modifyM .: storeAny Y.vI64Store32

  loadI32   = StateT . loadAny Y.vI32Load
  loadI64   = StateT . loadAny Y.vI64Load
  loadF32 _ = return ()
  loadF64 _ = return ()
  load8I32  = StateT .: loadAny . Y.vI32Load8
  load16I32 = StateT .: loadAny . Y.vI32Load16
  load8I64  = StateT .: loadAny . Y.vI64Load8
  load16I64 = StateT .: loadAny . Y.vI64Load16
  load32I64 = StateT .: loadAny . Y.vI64Load32

  memGrow numPages =
    do
      s <- S.get
      case s ^. ccMem of
        Just mem ->
          do
            isGrown <- lift Y.vBoolUnknown
            prevNumPages <- lift Y.vI32Unknown
            grownNumPages <- lift $ Y.vI32Add prevNumPages numPages
            constraint <-
              case Ast.lMax (mem ^. smLimits) of
                Nothing -> lift $ Y.vBoolConst True
                Just mx -> lift (Y.vI32Le U grownNumPages =<< Y.vI32Const (WI32 $ toNum mx))
            constraint' <- lift $ Y.vBAnd constraint (s ^. ccConstraint)
            neg1 <- lift $ Y.vI32Const $ WI32 0xFFFFFFFF --  -1
            S.modify $ ccConstraint .~ constraint'
            lift $ Y.vI32Select isGrown prevNumPages neg1
        Nothing  -> fail "Program has no memory"

  memSize =
    do
      s <- S.get
      case s ^. ccMem of
        Just mem ->
          do
            numPages <- lift Y.vI32Unknown
            constraint <-
              case Ast.lMax (mem ^. smLimits) of
                Nothing -> lift $ Y.vBoolConst True
                Just mx -> lift (Y.vI32Le U numPages =<< Y.vI32Const (WI32 $ toNum mx))
            constraint' <- lift $ Y.vBAnd constraint (s ^. ccConstraint)
            S.modify $ ccConstraint .~ constraint'
            return numPages
        Nothing  -> fail "Program has no memory"

instance Y.MonadSymbolics env m
         => SG.MonadSymbolicWasm
              env
              ()
              ()
              (Maybe (SymbolicMem env))
              (StateT (ConfigurationCore env) (MaybeT m)) where
  trapIf isTrap =
    do
      -- Configuration do not actually "trap". The states satisfying the
      -- trapping condition never reach the next program point.
      s <- S.get
      constraint <- Y.vBAnd (s^.ccConstraint) =<< Y.vBNot isTrap
      S.put $ s & ccConstraint .~ constraint


-- # Helpers #

storeAny :: MonadSymbolics env m
         => ( Symbolic env TMem -> Symbolic env TI33 -> a -> m (Symbolic env TMem) )
         -> SG.SymbolicAddress env
         -> a
         -> ConfigurationCore env
         -> m (ConfigurationCore env)
storeAny f addr val s =
  case s ^. ccMem of
    Just mem ->
      do
        let (_, numPages) = realLimits (mem ^. smLimits)
        numPagesSym <- Y.vI32Const $ WI32 $ toNum numPages
        (isOk, symAddr) <- SG.boundAddr addr numPagesSym
        d <- f (mem ^. smData) symAddr val
        constraint <- Y.vBAnd isOk (s ^. ccConstraint)
        let mem' = mem & smData .~ d
        return $ s & ccMem ?~ mem' & ccConstraint .~ constraint
    _ -> fail "Invalid operation"

loadAny :: MonadSymbolics env m
        => ( Symbolic env TMem -> Symbolic env TI33 -> m a )
        -> SG.SymbolicAddress env
        -> ConfigurationCore env
        -> m (a, ConfigurationCore env)
loadAny f addr c =
  case c ^. ccMem of
    Just mem ->
      do
        let (_, numPages) = realLimits (mem ^. smLimits)
        numPagesSym <- Y.vI32Const $ WI32 $ toNum numPages
        (isOk, symAddr) <- SG.boundAddr addr numPagesSym
        val <- f (mem ^. smData) symAddr
        constraint <- Y.vBAnd isOk (c ^. ccConstraint)
        return (val, c & ccConstraint .~ constraint)
    _ -> fail "Invalid operation"
