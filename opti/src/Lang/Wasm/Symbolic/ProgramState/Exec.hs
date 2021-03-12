{-# LANGUAGE RankNTypes, KindSignatures, TupleSections #-}

module Lang.Wasm.Symbolic.ProgramState.Exec
  ( execTree
  ) where

import Melude
-- Stdlib imports
import qualified Data.List.NonEmpty as NE
-- Extra stdlib imports
import qualified Control.Monad.State as S
-- Local library imports
import qualified Lang.Wasm.Data as WD
import           Lang.Wasm.Ast
  ( TI32, SimpleInstr, WI32 (..), Sx (..), FuncIdx (..), ValType, PVal, valType )
-- Local imports
import           Lang.Wasm.Solver ( MonadSolver (..) )
import           Lang.Wasm.Process
  -- ( Edge (..), Tree (..), DropKeep (..), DroppedVals, KeptVals, newDropKeep )
import qualified Lang.Wasm.Process as P
import qualified Lang.Wasm.Symbolics as Y
import           Lang.Wasm.Symbolics
  ( ExtSymbolics, MonadExtSymbolics, Symbolic, TWorld )
import qualified Lang.Wasm.Symbolic.ProgramState as SymState
import           Lang.Wasm.Symbolic.ProgramState
  ( SymbolicProgramState (..), ExtSelection (..), sWorld, sLocalState, sStack, selectProgState )
import           Lang.Wasm.Symbolic.ProgramState.ExecSimple ( execSimpleInstr )

-- | Performs /bounded symbolic execution/ over a process tree, whose resulting
-- symbolic state is returned. If all branches terminate within the bound, the
-- obtained symbolic state fully characterizes the program behaviour w.r.t. the
-- initial state.
--
-- A `Nothing` within the `MaybeT` signifies a static error (e.g., the types of
-- symbolic values mismatch) or that the depth bound was reached. A `Nothing` in
-- the result signifies that the initial state is unreachable.
--
-- TODO: Define equivalence checking over process graphs.
execTree :: MonadSolver m
         => Int                      -- ^ Execution depth bound
         -> P.Tree a                 -- ^ Tree to execute
         -> Bool                     -- ^ May return
         -> SymbolicProgramState env -- ^ Initial state
         -> Symbolic env Bool        -- ^ Path Constraint
         -> Y.ExtSymbolicsStateT env (MaybeT m) (Maybe (SymbolicProgramState env))
execTree 0 _ _ _ _ = fail "Depth bound reached"
execTree _ (NodeTerminal _) mayReturn s c =
  if'
    <$> (lift . isUnsat =<< Y.getsExtSymbolics (, c))
    <*> return Nothing -- Unreachable path
    <*> return (Just s)
execTree _ (NodeTrapped _) mayReturn s c =
  if'
    <$> (lift . isUnsat =<< Y.getsExtSymbolics (, c))
    <*> return Nothing -- Unreachable path
    <*> return (Just $ SymState.trap s)
execTree _ (NodeReturn _) mayReturn s c =
  if mayReturn then
    return (Just s)
  else
    -- Note that most jumps are inlined into the tree
    trace "Cannot reason about *external* jumps (return)" $
    fail "Cannot reason about *external* jumps (return)" -- TODO
execTree _ (NodeNaturalEnd _) mayReturn s c =
  return $ Just s
execTree b (Node a e) mayReturn s c =
  execEdge b e mayReturn s c

execEdge :: forall m env a
         .  MonadSolver m
         => Int                      -- ^ Execution depth bound
         -> TreeEdge a               -- ^ Edge to execute
         -> Bool                     -- ^ May Return
         -> SymbolicProgramState env -- ^ Initial state
         -> Symbolic env Bool        -- ^ Path Constraint
         -> Y.ExtSymbolicsStateT env (MaybeT m) (Maybe (SymbolicProgramState env))
execEdge 0 _ _ _ _ = fail "Depth bound reached"
execEdge b (PteInstr instr x) mayReturn s c =
  do
    s' <- liftSym $ execSimpleInstr s instr
    execTree (b-1) x mayReturn s' c
    -- join (execTree (b-1) x <$> execSimpleInstr' instr s <*> pure c)
execEdge b (PteIf ft xTrue xFalse) mayReturn s c =
  do
    res <- lift $ popI32 s
    case res of
      Nothing -> return (Just s) -- Unconditionally trapped
      Just (cond, s') ->
        do
          cTrue <- Y.vBAnd c =<< Y.vBI32 cond
          cFalse <- Y.vBAnd c =<< Y.vBNot =<< Y.vBI32 cond

          -- No need to bother with labels here

          isTrueUnsat <- isUnsat =<< Y.getsExtSymbolics (,cTrue)
          isFalseUnsat <- isUnsat =<< Y.getsExtSymbolics (,cFalse)

          sFinalTrue <-
            if isTrueUnsat then
              return Nothing -- Unreachable path
            else
              execTree (b-1) xTrue mayReturn s' cTrue
          sFinalFalse <-
            if isFalseUnsat then
              return Nothing -- Unreachable path
            else
              execTree (b-1) xFalse mayReturn s' cFalse

          liftSym $ selectMaybeProgState cTrue sFinalTrue sFinalFalse
execEdge b (PteBr _) mayReturn s c =
  -- note that internal jumps are inlined through PteIf
  trace "Cannot reason about *external* jumps" $
  fail "Cannot reason about *external* jumps"
execEdge b (PteBrIf _ _) mayReturn s c =
  trace "Cannot reason about *external* conditional jumps" $
  fail "Cannot reason about *external* conditional jumps"
execEdge b (PteBrTable _ _) mayReturn s c =
  trace "Cannot reason about *external* table jumps" $
  fail "Cannot reason about *external* table jumps"
  -- do
  --   res <- lift $ popI32 s
  --   case res of
  --     Nothing -> return (Just s) -- Unconditionally trapped
  --     Just (takenBranch, s') ->
  --       do
  --         branches <- catMaybes <$> mapM (\(i, branch) -> execBranch i takenBranch numKeep branch s' c) (zip [0..] xs)
          
  --         -- Default branch; if x > n
  --         isDefaultSelected <- Y.vI32Gt U takenBranch =<< Y.vI32Const (WI32 $ toNum $ length xs)
  --         Just s'' <- return $ applyDropKeep (newDropKeep numKeep defV) s'
  --         c' <- Y.vBAnd c isDefaultSelected
  --         isDefaultUnsat <- isUnsat =<< S.gets (,c')

  --         defaultRes <-
  --           if isDefaultUnsat then
  --             return Nothing -- Unreachable path
  --           else
  --             execTree (b-1) defT s'' c'

  --         liftSym $ selectBranch branches defaultRes
  -- where
  -- execBranch :: MonadSolver m
  --            => Int                -- ^ Index in branch table
  --            -> Symbolic env TI32  -- ^ Taken branch
  --            -> KeptVals           -- ^ Number of values kept on top of stack
  --            -> (DroppedVals, Tree a) -- ^ 
  --            -> ProgState env      -- ^ Prior state
  --            -> Symbolic env Bool  -- ^ Parent Path Constraint
  --            -> StateT (ExtSymbolics env) (MaybeT m) (Maybe (Symbolic env Bool, ProgState env))
  -- execBranch branchI takenBranchI numKeep (dv, t) s c =
  --   do
  --     isBranchSelected <- Y.vI32Eq takenBranchI =<< Y.vI32Const (WI32 $ toNum branchI)
  --     Just s' <- return $ applyDropKeep (newDropKeep numKeep dv) s
  --     c' <- Y.vBAnd c isBranchSelected
  --     isBranchUnsat <- isUnsat =<< S.gets (,c')

  --     if isBranchUnsat then
  --       return Nothing -- Unreachable path
  --     else
  --       fmap (isBranchSelected,) <$> execTree (b-1) t s' c'
  -- selectBranch :: MonadExtSymbolics env m
  --              => [(Symbolic env Bool, ProgState env)] -- ^ Branches with select condition
  --              -> Maybe (ProgState env)                -- ^ Default branch
  --              -> MaybeT m (Maybe (ProgState env))
  -- selectBranch []                   y = return y
  -- selectBranch ((isSelected, s):xs) y = selectMaybeProgState isSelected (Just s) =<< selectBranch xs y
execEdge b (PteCall (FuncIdx fIdx) t) mayReturn s c =
  trace "Function call not implemented" $
  fail "Function call not implemented"
execEdge b (PteCallIndirect _ _) mayReturn s c =
  trace "Indirect function call not implemented" $
  fail "Indirect function call not implemented"
execEdge b (PteKeepDrop kv dv t) mayReturn s c =
  do
    -- Just s' <- return $ applyDropKeep (DKKeep numKeep dv) s
    
    -- execTree (b-1) t s' c
    return undefined

-- | Swaps the `MaybeT` and `StateT` monads
liftSym :: Monad m => MaybeT (Y.ExtSymbolicsStateT s m) a -> Y.ExtSymbolicsStateT s (MaybeT m) a
liftSym x =
  Y.ExtSymbolicsStateT $ StateT $ \s ->
    MaybeT $
      do
        let Y.ExtSymbolicsStateT st = runMaybeT x
        (mA, b) <- runStateT st s
        return ( (,b) <$> mA )

selectMaybeProgState :: ExtSelection env (Maybe (SymbolicProgramState env))
selectMaybeProgState c (Just a) (Just b) = Just <$> selectProgState c a b
selectMaybeProgState _ (Just a) Nothing  = return $ Just a
selectMaybeProgState _ Nothing  (Just b) = return $ Just b
selectMaybeProgState _ Nothing  Nothing  = return Nothing

popI32 :: Monad m => SymbolicProgramState env -> MaybeT m (Maybe (Symbolic env TI32, SymbolicProgramState env))
popI32 s =
  case s^.sLocalState of
    Nothing -> return Nothing -- Unconditionally trapped
    Just ls ->
      case WD.popStackI32 (ls^.sStack) of
        Nothing -> fail "Invalid program"
        Just (x, stack') ->
          let ls' = ls & sStack .~ stack'
              s'  = s & sLocalState ?~ ls'
          in return $ Just (x, s')

-- | A wrapper around the `execSimpleInstr` function for full symbolic
-- execution.
-- execSimpleInstr' :: MonadSolver m
--                  => SimpleInstr
--                  -> ProgState env
--                  -> StateT (ExtSymbolics env) (MaybeT m) (ProgState env)
-- execSimpleInstr' instr state =
--   case SymState.extractStateCore state of
--     Nothing -> -- Unconditionally trapped. Nothing changes
--       return state
--     Just core ->
--       do
--         Just (w', core') <-
--           runMaybeT $
--             execSimpleInstr
--               growMemory
--               Y.vWorldSelect
--               (state^.sWorld)
--               instr
--               core
--         return $ SymState.fromStateCore core' w'
--   where
--   growMemory :: MonadExtSymbolics env m
--              => Symbolic env TWorld
--              -> Symbolic env TI32
--              -> m (Symbolic env TWorld, Symbolic env Bool)
--   growMemory w s = (,) <$> Y.vMemGrowWorld w s <*> Y.vMemGrowBool w s


-- # Helpers

-- applyDropKeep :: DropKeep
--               -> SymbolicProgramState env
--               -> Maybe (SymbolicProgramState env)
-- applyDropKeep dk = P.applyDropKeep dk & (sLocalState . _Just . sStack)
