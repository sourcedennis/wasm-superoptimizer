{-# LANGUAGE ScopedTypeVariables, ConstraintKinds,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             TemplateHaskell, RankNTypes
#-}


-- | Symbolic program state representation
--
-- Parts of the data structure is defined in `Lang.Wasm.Symbolic.General`, as it
-- is shared with the configuration.
module Lang.Wasm.Symbolic.ProgramState
  ( -- * Data structures
    SymbolicProgramState (..)
  , SymbolicLocalState (..)
  , SymbolicMem (..)
  , SymbolicActivation (..)
  , SymbolicStack (..)
  , SymbolicGlobal (..)
  , ExtSelection (..)
    -- ** Lenses
  , sLocalState, sGlobals, sMem, sWorld
  , sIsTrapped, sActivation, sStack
    -- * Functions
  , trap
  , fromConfiguration
    -- * Symbolic Operations
  , selectLocalState
  , selectActivation
  , selectStack
  , selectMaybeLocalState
  , selectProgState
  ) where

import Melude
-- Stdlib imports
import qualified Data.List.NonEmpty as NE
-- Extra stdlib imports
import qualified Control.Monad.State as S
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local library imports
import qualified Lang.Wasm.Data as WD
import           Lang.Wasm.Data ( PActivation, PGlobal, PStack )
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast
  ( WI32 (..), WI64 (..), WF32, WF64, TI32, TI64, TF32, TF64, IUnop (..), IBinop (..)
  , FUnop (..), FBinop (..), ITestop (..), IRelop (..), FRelop (..), Cvtop (..)
  , PrmInstr (..), VarInstr (..), MemInstr (..), SimpleInstr (..), MemArg (..)
  , Sx (..), PVal (..), Limits
  )
-- Local imports
import qualified Lang.Wasm.Symbolics as Y
import           Lang.Wasm.Symbolics
  ( ExtSymbolics, Symbolic, TWorld, MonadExtSymbolics, MonadSymbolics
  , SymbolicVal (..) )
import qualified Lang.Wasm.Symbolic.General as SG
import           Lang.Wasm.Symbolic.General
  ( MonadSymbolicWasm (..), SymbolicAddress, SymbolicMem (..)
  , smLimits, smData, smPageCount
  )
import qualified Lang.Wasm.Symbolic.Configuration as C
import           Lang.Wasm.Symbolic.Configuration
  ( Configuration (..)
  , cfgActivation, cfgStack, cfgGlobals, cfgMem, cfgSymbolics, cfgConstraint )
import           Lang.Wasm.Algebra ( SimpleAlgebra (..), MonadWasmState (..) )
import           Lang.Wasm.Markers ( TMem, TI33 )


-- # Data Structures #

-- | A symbolic state.
--
-- Think of this as a single concrete state (which happens to be symbolically
-- represented), which is parametric over some input values.
data SymbolicProgramState env =
  SymbolicProgramState {
    -- | When the local state is `Nothing`, execution has trapped
    -- unconditionally. As execution cannot continue on these branches, the
    -- stack has no relevance. Memory, globals, and side-effects from the
    -- preceding execution remain important to the caller, though.
    _sLocalState  :: Maybe (SymbolicLocalState env)
  , _sGlobals     :: IdList (SymbolicGlobal env)
  , _sMem         :: Maybe (SymbolicMem env)

  , _sWorld       :: Symbolic env TWorld
  }
  deriving Show

-- | A fragment of the symbolic state that is local to an execution trace.
-- 
-- A group of execution traces that has trapped unconditionally (e.g., by
-- reaching an @unreachable@ instruction), has no way of progressing the stack,
-- as execution is aborted and control returned to the caller. However, a
-- trapped `SymbolicState` still needs to be unified with other execution traces
-- (which may have a stack). To accomodate this, the state relating to the
-- execution trace is represented with a `SymbolicLocalState` (which may be
-- `Nothing` in `SymbolicState`).
data SymbolicLocalState env =
  SymbolicLocalState {
    -- | A symbolic boolean which states whether an execution has trapped. As
    -- this is a symbolic value, it may only be `True` for some of the
    -- represented executions. (e.g., this happens by division by 0 - but only
    -- for the traces where the divisor is actually 0)
    _sIsTrapped   :: Symbolic env Bool
    -- | The stack frame of the function which is currently active
  , _sActivation  :: SymbolicActivation env
    -- | The stack containing only values. (The stack frame is extracted to
    -- `symsActivation` and labels are omitted and enforced by control flow in
    -- the process graph)
  , _sStack       :: SymbolicStack env
  }
  deriving Show

-- | Fully symbolic activation
type SymbolicActivation env =
  PActivation
    (Symbolic env TI32)
    (Symbolic env TI64)
    (Symbolic env TF32)
    (Symbolic env TF64)

-- | Fully symbolic stack
type SymbolicStack env =
  PStack
    (Symbolic env TI32)
    (Symbolic env TI64)
    (Symbolic env TF32)
    (Symbolic env TF64)

-- | Fully symbolic global
type SymbolicGlobal env =
  PGlobal
    (Symbolic env TI32)
    (Symbolic env TI64)
    (Symbolic env TF32)
    (Symbolic env TF64)


$(makeLenses ''SymbolicProgramState)
$(makeLenses ''SymbolicLocalState)


-- # Utilities #

-- | Trap the execution trace. No further executions can transform it after.
--
-- If this were a concrete execution, control is returned to the host.
trap :: SymbolicProgramState env -> SymbolicProgramState env
trap = sLocalState .~ Nothing

-- | Creates a fresh symbolic state in the (returned) environment.
--
-- It symbolically represents /only/ the states contained in the configuration.
fromConfiguration :: Configuration env
                  -> (SymbolicProgramState env, Y.ExtEnvironment env)
fromConfiguration c =
  let ((s, constraint),env) = Y.runExtSymbolicsState (fromConfiguration' c) (Y.liftExt $ c^.cfgSymbolics)
  in (s, (env, constraint))
  where
  fromConfiguration' :: Y.MonadExtSymbolics env m
                     => Configuration env
                     -> m (SymbolicProgramState env, Symbolic env Bool)
  fromConfiguration' c =
    do
      w <- Y.vWorldUnknown
      -- Configurations do not trap. Instead, the value upon which it trapped
      -- are excluded as set members.
      isTrapped <- Y.vBoolConst False
      sActivation <- WD.mapActivationVal fromVal (c^.cfgActivation)
      sStack <- WD.mapStackVal fromVal (c^.cfgStack)
      sGlobals <- mapM (WD.mapGlobalValM fromVal) (c^.cfgGlobals)
      let s =
            SymbolicProgramState {
              _sLocalState =
                Just $ SymbolicLocalState {
                  _sIsTrapped   = isTrapped
                , _sActivation  = sActivation
                , _sStack       = sStack
                }
            , _sGlobals  = sGlobals
            , _sMem      = c^.cfgMem
            , _sWorld    = w
            }
      return ( s, c^.cfgConstraint )
  fromVal :: Y.MonadExtSymbolics env m => Y.SymbolicVal env -> m (Y.ExtSymbolicVal env)
  fromVal (VI32 v)  = return $ VI32 v
  fromVal (VI64 v)  = return $ VI64 v
  fromVal (VF32 ()) = VF32 <$> Y.vF32Unknown
  fromVal (VF64 ()) = VF64 <$> Y.vF64Unknown


-- # Instances #

-- ## Symbolic State Instances ##

instance Y.MonadExtSymbolics env m =>
            MonadWasmState
              (Symbolic env TI32)
              (Symbolic env TI64)
              (Symbolic env TF32)
              (Symbolic env TF64)
              (Maybe (SymbolicMem env))
              (StateT (SymbolicProgramState env) (MaybeT m)) where
  popStack  =
    liftLocalState $ withStateLens sStack $ StateT WD.popStackVal

  peekStack =
    liftLocalState $ withStateLens sStack $ getsM WD.peekStackVal

  pushStack val =
    liftLocalState $ withStateLens sStack $ S.modify $ WD.pushStack val

  pushLabel =
    liftLocalState $ withStateLens sStack $ S.modify $ WD.pushStackLabel []

  popLabel =
    liftLocalState $ withStateLens sStack $ modifyM WD.popStackLabel

  getLocal idx =
    liftLocalState $ withStateLens sActivation $ getsM (WD.getLocal idx)

  setLocal idx v =
    liftLocalState $ withStateLens sActivation $ modifyM (WD.setLocal idx v)

  getGlobal gIdx =
    withStateLens sGlobals $ getsM (WD.getGlobal gIdx)

  setGlobal gIdx v =
    withStateLens sGlobals $ modifyM (WD.setGlobal gIdx v)

  getGlobals =
    S.gets (map WD.gVal . IdList.toList . view sGlobals)

  getMemory =
    S.gets (view sMem)
  
  setMemory m =
    S.modify $ \s -> s & sMem .~ m

  storeI32   = modifyM .: storeAny Y.vI32Store
  storeI64   = modifyM .: storeAny Y.vI64Store
  storeF32   = modifyM .: storeAny Y.vF32Store
  storeF64   = modifyM .: storeAny Y.vF64Store
  store8I32  = modifyM .: storeAny Y.vI32Store8
  store16I32 = modifyM .: storeAny Y.vI32Store16
  store8I64  = modifyM .: storeAny Y.vI64Store8
  store16I64 = modifyM .: storeAny Y.vI64Store16
  store32I64 = modifyM .: storeAny Y.vI64Store32

  loadI32   = StateT . loadAny Y.vI32Load
  loadI64   = StateT . loadAny Y.vI64Load
  loadF32   = StateT . loadAny Y.vF32Load
  loadF64   = StateT . loadAny Y.vF64Load
  load8I32  = StateT .: loadAny . Y.vI32Load8
  load16I32 = StateT .: loadAny . Y.vI32Load16
  load8I64  = StateT .: loadAny . Y.vI64Load8
  load16I64 = StateT .: loadAny . Y.vI64Load16
  load32I64 = StateT .: loadAny . Y.vI64Load32

  memGrow numPages =
    do
      s <- S.get
      case (s ^. sLocalState, s ^. sMem) of
        (Just ls, Just mem) ->
          do
            let w = s ^. sWorld
                oldPageCount = mem ^. SG.smPageCount
            isOk <- lift $ Y.vMemGrowBool w oldPageCount
            w'   <- lift $ Y.vMemGrowWorld w oldPageCount
            okPageCount <- lift $ Y.vI32Add oldPageCount numPages
            newPageCount <- lift $ Y.vI32Select isOk okPageCount oldPageCount
            let mem' = mem & SG.smPageCount .~ newPageCount
            S.put (s & sMem ?~ mem' & sWorld .~ w')
            return newPageCount
        _ -> fail "Invalid operation"

  memSize =
    do
      s <- S.get
      case s ^. sMem of
        Just mem -> return (mem ^. SG.smPageCount)
        _ -> fail "Invalid operation"

instance Y.MonadExtSymbolics env m
         => MonadSymbolicWasm
              env
              (Symbolic env TF32)
              (Symbolic env TF64)
              (Maybe (SymbolicMem env))
              (StateT (SymbolicProgramState env) (MaybeT m)) where
  trapIf b =
    do
      s <- S.get
      case s ^. sLocalState of
        Nothing -> fail "Unconditionally Trapped execution"
        Just ls ->
          do
            isTrapped' <- lift $ Y.vBOr b (ls^.sIsTrapped)
            let ls' = ls & sIsTrapped .~ isTrapped'
            S.put $ s & sLocalState ?~ ls'


-- # Symbolic Operations #

-- | Alias: @Symbolic env Bool -> val -> val -> MaybeT m val@
--
-- Alias for symbolic selection functions (i.e., logical if-then-else)
-- @select True a b = a@, @select False a b = b@.
--
-- This may fail over the `MaybeT` when the values have mismatching static
-- structures.
type ExtSelection env val =
  forall m . MonadExtSymbolics env m => Symbolic env Bool -> val -> val -> MaybeT m val

selectLocalState :: ExtSelection env (SymbolicLocalState env)
selectLocalState c a b =
  do
    isTrapped <- lift (Y.vBoolSelect c (a^.sIsTrapped) (b^.sIsTrapped))
    activation <- selectActivation c (a^.sActivation) (b^.sActivation)
    stack <- selectStack c (a^.sStack) (b^.sStack)
    return $ SymbolicLocalState isTrapped activation stack

selectActivation :: ExtSelection env (SymbolicActivation env)
selectActivation = WD.zipActivationVal . selectVal

selectStack :: ExtSelection env (SymbolicStack env)
selectStack = WD.zipStackVal . selectVal

selectGlobal :: ExtSelection env (SymbolicGlobal env)
selectGlobal = WD.zipGlobalVal . selectVal

selectMem :: ExtSelection env (SymbolicMem env)
selectMem c a b =
  do
    failIf ((a^.smLimits) /= (b^.smLimits)) "Statically unequal memories"
    d <- Y.vMemSelect c (a^.smData) (b^.smData)
    pc <- Y.vI32Select c (a^.smPageCount) (b^.smPageCount)
    return $ SymbolicMem d pc (a^.smLimits)

selectVal :: ExtSelection env (Y.ExtSymbolicVal env)
selectVal c =
  Ast.zipValM
    (Y.vI32Select c)
    (Y.vI64Select c)
    (Y.vF32Select c)
    (Y.vF64Select c)

-- |
-- 
-- Mismatching `Maybe` constructors (`Just` or `Nothing`) does /not/ signify a
-- mismatching static structure. E.g., if one trace is unconditionally trapped
-- (by reaching an @unreachable@), while the other trace is conditionally
-- trapped for all branches (e.g., all paths divided by 0), then the trapping
-- conditions of the paths fully match.
selectMaybeLocalState :: ExtSelection env (Maybe (SymbolicLocalState env))
selectMaybeLocalState c (Just a) (Just b) = Just <$> selectLocalState c a b
selectMaybeLocalState _ Nothing  Nothing  = return Nothing
selectMaybeLocalState c Nothing  (Just b) = -- left is unconditionally trapped
  do
    -- select c True b = c | (!c & b)
    isTrapped <- lift $ join (Y.vBoolSelect c <$> Y.vBoolConst True <*> pure (b^.sIsTrapped))
    -- For trapped executions, the stack and activations do not matter
    return $ Just $ SymbolicLocalState isTrapped (b^.sActivation) (b^.sStack)
selectMaybeLocalState c (Just a) Nothing  = -- right is unconditionally trapped
  do
    -- select c a True = (c & a) | !c
    isTrapped <- lift (Y.vBoolSelect c (a^.sIsTrapped) =<< Y.vBoolConst True)
    -- For trapped executions, the stack and activations do not matter
    return $ Just $ SymbolicLocalState isTrapped (a^.sActivation) (a^.sStack)

selectProgState :: ExtSelection env (SymbolicProgramState env)
selectProgState c a b =
  do
    localState <- selectMaybeLocalState c (a^.sLocalState) (b^.sLocalState)
    globals    <- IdList.zipWithExactM (selectGlobal c) (a^.sGlobals) (b^.sGlobals)
    mems       <- selectMaybe selectMem c (a^.sMem) (b^.sMem)
    world      <- Y.vWorldSelect c (a^.sWorld) (b^.sWorld)

    return $ SymbolicProgramState localState globals mems world
  where
  selectMaybe :: ExtSelection env a -> ExtSelection env (Maybe a)
  selectMaybe f c (Just a) (Just b) = Just <$> f c a b
  selectMaybe _ _ Nothing  Nothing  = return Nothing
  selectMaybe _ _ _        _        = fail "Mismatching static structures"


-- # Helpers #

liftLocalState :: MonadFail m
               => StateT (SymbolicLocalState env) m a
               -> StateT (SymbolicProgramState env) m a
liftLocalState m =
  do
    s <- S.get
    case s ^. sLocalState of
      Nothing -> fail "Unconditionally Trapped execution"
      Just ls ->
        do
          (a, ls') <- lift $ runStateT m ls
          S.modify $ \s -> s & sLocalState ?~ ls'
          return a

storeAny :: MonadSymbolics env m
         => ( Symbolic env TMem -> Symbolic env TI33 -> a -> m (Symbolic env TMem) )
         -> SymbolicAddress env
         -> a
         -> SymbolicProgramState env
         -> m (SymbolicProgramState env)
storeAny f addr val s =
  case (s ^. sLocalState, s ^. sMem) of
    (Just ls, Just mem) ->
      do
        (isOk, symAddr) <- SG.boundAddr addr (mem ^. SG.smPageCount)
        d <- f (mem ^. SG.smData) symAddr val
        isTrapped <- Y.vBOr (ls^.sIsTrapped) =<< Y.vBNot isOk
        let mem' = mem & SG.smData .~ d
            ls' = ls & sIsTrapped .~ isTrapped
        return $ s & sLocalState ?~ ls' & sMem ?~ mem'
    _ -> fail "Invalid operation"

loadAny :: MonadSymbolics env m
        => ( Symbolic env TMem -> Symbolic env TI33 -> m a )
        -> SymbolicAddress env
        -> SymbolicProgramState env
        -> m (a, SymbolicProgramState env)
loadAny f addr s =
  case (s ^. sLocalState, s ^. sMem) of
    (Just ls, Just mem) ->
      do
        (isOk, symAddr) <- SG.boundAddr addr (mem ^. SG.smPageCount)
        val <- f (mem ^. SG.smData) symAddr
        isTrapped <- Y.vBOr (ls^.sIsTrapped) =<< Y.vBNot isOk
        let ls' = ls & sIsTrapped .~ isTrapped
        return (val, s & sLocalState ?~ ls')
    _ -> fail "Invalid operation"
