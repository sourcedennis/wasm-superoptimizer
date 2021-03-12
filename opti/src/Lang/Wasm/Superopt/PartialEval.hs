{-# LANGUAGE ScopedTypeVariables #-}

-- | Uses a SMT solver to evaluate expressions to constants. Note that this goes
-- further than constant propagation + folding. Example:
--
-- ```
-- if a > b:              if a > b:
--   if b > c:              if b > c:
--     if c > a:              if False:
--       ...                    ...
-- ```
module Lang.Wasm.Superopt.PartialEval where

import Melude
-- Stdlib imports
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- External library imports
import qualified Data.Vector as Vector
import           Data.Vector ( Vector )
import qualified Control.Monad.State as S
import           Control.Monad.State ( State )
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local library imports
import           Lang.Wasm.Process
  ( GraphFrozen, GraphMut, Node (..), GraphNode, GraphEdge (..), NodeId, freezeGraph )
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( InstrCtx, SimpleInstr (..), PrmInstr (..), ValType (..), WI32 (..), WI64 (..) )
import qualified Lang.Wasm.Data as WD
-- Local imports
import Lang.Wasm.Superopt.MonadSuperopt ( MonadSuperopt (..) )
import Lang.Wasm.Solver as Sv
import Lang.Wasm.Symbolic.Configuration as C

data Mode = ModeNoConsts | ModeExprConsts | ModeAllConsts
  deriving Eq

findConstants :: forall m a env
              .  (MonadFail m, MonadSuperopt m)
              => TimeMs
              -> Mode
              -> InstrCtx
              -> GraphFrozen a
              -> IntMap (Configuration env)
              -> m (GraphFrozen ())
findConstants t mode ctx (startI, g, endI) cs =
  do
    ((startI', endI'), g') <- runStateT ((,) <$> visit startI <*> visit endI) IdList.empty
    return $ freezeGraph (startI', g', endI')
  where
  replaceAfterI32 :: NodeId -> StateT (IdList (GraphNode ())) m NodeId
  replaceAfterI32 i =
    do
      cAfter <- failMaybeMsg "Missing configuration" (IntMap.lookup i cs)

      (tTaken, mV) <- lift $ evalTopI32 t cAfter
      lift $ logConstantReplaced tTaken (isJust mV)
      case mV of
        Just v ->
          newNode . (Node () . PgeInstr (SPrmInstrI32 PDrop))
            =<< newNode . (Node () . PgeInstr (SConstI32 v))
            =<< visit i
        Nothing -> visit i

  replaceAfterI64 :: NodeId -> StateT (IdList (GraphNode ())) m NodeId
  replaceAfterI64 i =
    do
      cAfter <- failMaybeMsg "Missing configuration" (IntMap.lookup i cs)

      (tTaken, mV) <- lift $ evalTopI64 t cAfter
      lift $ logConstantReplaced tTaken (isJust mV)
      case mV of
        Just v ->
          newNode . (Node () . PgeInstr (SPrmInstrI64 PDrop))
            =<< newNode . (Node () . PgeInstr (SConstI64 v))
            =<< visit i
        Nothing -> visit i
  
  visit :: NodeId -> StateT (IdList (GraphNode ())) m NodeId
  visit nodeI =
    do
      node <- failMaybeMsg "" (g !? nodeI)
      case node of
        Node a (PgeInstr instr j) ->
          do
            cAfter <- failMaybeMsg "Missing configuration" (IntMap.lookup j cs)

            isI32 <- producesI32 ctx instr
            isI64 <- producesI64 ctx instr

            if mode == ModeNoConsts || isConstant instr then
              -- No need to replace an instruction that is already a constant
              newNode . (Node () . PgeInstr instr) =<< visit j
            else if isI32 then
              newNode . (Node () . PgeInstr instr) =<< replaceAfterI32 j
            else if isI64 then
              newNode . (Node () . PgeInstr instr) =<< replaceAfterI64 j
            else
              newNode . (Node () . PgeInstr instr) =<< visit j
        Node a (PgeIf ft ifG elseG j) ->
          do
            c <- failMaybeMsg "Missing configuration" (IntMap.lookup nodeI cs)
            (t, branchVal) <- lift $ evalTopBool t c
            lift $ logBranchEliminated t (isJust branchVal)
            j' <-
              if mode /= ModeAllConsts then
                visit j
              else if Vector.toList (Ast.ftResults ft) == [TI32] then
                replaceAfterI32 j
              else if Vector.toList (Ast.ftResults ft) == [TI64] then
                replaceAfterI64 j
              else
                visit j
            case branchVal of
              Nothing ->
                do
                  ifG' <- visit ifG
                  elseG' <- visit elseG
                  newNode (Node () (PgeIf ft ifG' elseG' j'))
              Just True ->
                do
                  ifG' <- visit ifG
                  newNode . (Node () . PgeInstr (SPrmInstrI32 PDrop)) =<< newNode (Node () (PgeBlock ft ifG' j'))
              Just False ->
                do
                  elseG' <- visit elseG
                  newNode . (Node () . PgeInstr (SPrmInstrI32 PDrop)) =<< newNode (Node () (PgeBlock ft elseG' j'))
        Node a (PgeBlock ft bodyG j) ->
          do
            bodyG' <- visit bodyG
            j' <-
              if mode /= ModeAllConsts then
                visit j
              else if Vector.toList (Ast.ftResults ft) == [TI32] then
                replaceAfterI32 j
              else if Vector.toList (Ast.ftResults ft) == [TI64] then
                replaceAfterI64 j
              else
                visit j
            newNode $ Node () (PgeBlock ft bodyG' j')
        Node a (PgeLoop ft bodyG j) ->
          do
            bodyG' <- visit bodyG
            j' <-
              if mode /= ModeAllConsts then
                visit j
              else if Vector.toList (Ast.ftResults ft) == [TI32] then
                replaceAfterI32 j
              else if Vector.toList (Ast.ftResults ft) == [TI64] then
                replaceAfterI64 j
              else
                visit j
            newNode $ Node () (PgeLoop ft bodyG' j')
        Node a (PgeBr lIdx) ->
          newNode (Node () (PgeBr lIdx))
        Node a (PgeBrIf lIdx j) ->
          do
            c <- failMaybeMsg "Missing configuration" (IntMap.lookup nodeI cs)
            (t, branchVal) <- lift $ evalTopBool t c
            lift $ logBranchEliminated t (isJust branchVal)
            case branchVal of
              Nothing ->
                newNode . (Node () . PgeBrIf lIdx) =<< visit j
              Just True ->
                newNode . (Node () . PgeInstr (SPrmInstrI32 PDrop)) =<< newNode (Node () (PgeBr lIdx))
              Just False ->
                newNode . (Node () . PgeInstr (SPrmInstrI32 PDrop)) =<< visit j
        Node a (PgeBrTable lIdxs lIdx) ->
          do
            c <- failMaybeMsg "Missing configuration" (IntMap.lookup nodeI cs)
            (t, branchVal) <- lift $ evalTopI32 t c
            lift $ logBranchEliminated t (isJust branchVal)
            case branchVal of
              Nothing ->
                newNode $ Node () (PgeBrTable lIdxs lIdx)
              Just (WI32 v) ->
                case atIndex v lIdxs of
                  Just tIdx ->
                    newNode . (Node () . PgeInstr (SPrmInstrI32 PDrop)) =<< newNode (Node () (PgeBr tIdx))
                  Nothing   ->
                    newNode . (Node () . PgeInstr (SPrmInstrI32 PDrop)) =<< newNode (Node () (PgeBr lIdx))
        Node a (PgeCall fIdx j) ->
          do
            j' <- visit j
            newNode $ Node () (PgeCall fIdx j')
        Node a (PgeCallIndirect ft j) ->
          do
            j' <- visit j
            newNode $ Node () (PgeCallIndirect ft j')
        NodeNaturalEnd _ -> newNode $ NodeNaturalEnd ()
        NodeReturn _     -> newNode $ NodeReturn ()
        NodeTrapped _    -> newNode $ NodeTrapped ()
        NodeTerminal _   -> newNode $ NodeTerminal ()
        NodeStaticError  -> fail "Statically invalid graph"
  newNode :: GraphNode () -> StateT (IdList (GraphNode ())) m NodeId
  newNode n = S.state $ IdList.append n

evalTopI32 :: MonadSuperopt m => TimeMs -> Configuration env -> m (TimeMs, Maybe WI32)
evalTopI32 t c =
  case WD.popStackI32 (c ^. cfgStack) of
    Nothing -> return (0, Nothing)
    Just (i, _) ->
      do
        (tTaken, mI') <- softTimeout t $ evalI32 (environment c) i
        case mI' of
          EvSat i' -> return (tTaken, Just i')
          _ -> return (tTaken, Nothing)

evalTopI64 :: MonadSuperopt m => TimeMs -> Configuration env -> m (TimeMs, Maybe WI64)
evalTopI64 t c =
  case WD.popStackI64 (c ^. cfgStack) of
    Nothing -> return (0, Nothing)
    Just (i, _) ->
      do
        (tTaken, mI') <- softTimeout t $ evalI64 (environment c) i
        case mI' of
          EvSat i' -> return (tTaken, Just i')
          _ -> return (tTaken, Nothing)

evalTopBool :: MonadSuperopt m => TimeMs -> Configuration env -> m (TimeMs, Maybe Bool)
evalTopBool t c =
  case WD.popStackI32 (c ^. cfgStack) of
    Nothing -> return (0, Nothing)
    Just (i, _) ->
      do
        (tTaken, mI') <- softTimeout t $ evalBool (environment c) i
        case mI' of
          EvSat i' -> return (tTaken, Just i')
          _ -> return (tTaken, Nothing)

producesI32 :: MonadFail m => InstrCtx -> SimpleInstr -> m Bool
producesI32 ctx x =
  case Ast.simpleInstrEffect ctx x of
    Just (_, [TI32]) -> return True
    Nothing -> fail "Invalid instruction context"
    _ -> return False

producesI64 :: MonadFail m => InstrCtx -> SimpleInstr -> m Bool
producesI64 ctx x =
  case Ast.simpleInstrEffect ctx x of
    Just (_, [TI64]) -> return True
    Nothing -> fail "Invalid instruction context"
    _ -> return False

isConstant :: SimpleInstr -> Bool
isConstant (SConstI32 _) = True
isConstant (SConstI64 _) = True
-- tee_local is not a constant. However, it makes no sense to replace a constant
-- before and after the it.
--
-- So, don't do this:
-- (i32.const 5)           (i32.const 5)
-- (tee_local 0)    =>     (set_local 0)
--                         (i32.const 5)
isConstant (SVarInstr (Ast.VLocalTee _)) = True
isConstant _ = False
