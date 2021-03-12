{-# LANGUAGE ScopedTypeVariables #-}

module Lang.Wasm.Optimizations.CleanDead
  ( removeDead
  ) where

import Melude
-- Stdlib imports
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
-- Extra stdlib imports
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
-- External library imports
import qualified Data.Vector as Vector
import           Data.Vector ( Vector )
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local library imports
import Algorithm.Graph as AG
import Lang.Wasm.Process
  ( GraphFrozen (..), GraphNode (..), Node (..), GraphEdge (..), NodeId
  , graphFlow, freezeGraph )
-- Local imports
import qualified Algorithm.Dataflow as Dataflow
import           Lang.Wasm.Ast as Ast
import           Lang.Wasm.Data as WD
import qualified Lang.Wasm.Dataflow.Liveness as L
import           Lang.Wasm.Dataflow.Liveness

-- | Remove instructions that assign values to dead variables.
removeDead :: forall m a
           .  MonadFail m
           => InstrCtx
           -> ResultType
           -> GraphFrozen a
           -> m (GraphFrozen ())
removeDead ctx rt graph@(startI, g, endI) =
  do
    liveness <- L.graphLiveness ctx rt graph
    ((startI', endI'), g', _) <- runRWST ((,) <$> rebuild startI <*> rebuild endI) liveness IdList.empty
    return $ freezeGraph (startI', g', endI')
  where
  rebuild :: NodeId -> RWST (IntMap LivenessState) () (IdList (GraphNode ())) m NodeId
  rebuild nodeI =
    do
      node <- failMaybeMsg "" (g !? nodeI)
      case node of
        Node a (PgeInstr (SVarInstr (VLocalSet idx)) j) ->
          do
            ls <- nodeLiveness j
            valLiveness <- WD.getLocal idx (ls^.lsActivation)
            case valLiveness of
              VI32 Dead -> newNode . (Node () . PgeInstr (SPrmInstrI32 PDrop)) =<< rebuild j
              VI64 Dead -> newNode . (Node () . PgeInstr (SPrmInstrI64 PDrop)) =<< rebuild j
              VF32 Dead -> newNode . (Node () . PgeInstr (SPrmInstrF32 PDrop)) =<< rebuild j
              VF64 Dead -> newNode . (Node () . PgeInstr (SPrmInstrF64 PDrop)) =<< rebuild j
              _ -> newNode . (Node () . PgeInstr (SVarInstr (VLocalSet idx))) =<< rebuild j
        Node a (PgeInstr (SVarInstr (VLocalTee idx)) j) ->
          do
            ls <- nodeLiveness j
            valLiveness <- WD.getLocal idx (ls^.lsActivation)
            case valLiveness of
              VI32 Dead -> rebuild j
              VI64 Dead -> rebuild j
              VF32 Dead -> rebuild j
              VF64 Dead -> rebuild j
              _ -> newNode . (Node () . PgeInstr (SVarInstr (VLocalTee idx))) =<< rebuild j
        Node a (PgeInstr (SVarInstr (VGlobalSet idx)) j) ->
          do
            ls <- nodeLiveness j
            valLiveness <- WD.getGlobal idx (ls^.lsGlobals)
            case valLiveness of
              VI32 Dead -> newNode . (Node () . PgeInstr (SPrmInstrI32 PDrop)) =<< rebuild j
              VI64 Dead -> newNode . (Node () . PgeInstr (SPrmInstrI64 PDrop)) =<< rebuild j
              VF32 Dead -> newNode . (Node () . PgeInstr (SPrmInstrF32 PDrop)) =<< rebuild j
              VF64 Dead -> newNode . (Node () . PgeInstr (SPrmInstrF64 PDrop)) =<< rebuild j
              _ -> newNode . (Node () . PgeInstr (SVarInstr (VGlobalSet idx))) =<< rebuild j
        Node a (PgeInstr instr j) ->
          newNode . (Node () . PgeInstr instr) =<< rebuild j
        Node a (PgeIf ft ifG elseG j) ->
          do
            j' <- rebuild j
            ifG' <- rebuild ifG
            elseG' <- rebuild elseG
            newNode (Node () (PgeIf ft ifG' elseG' j'))
        Node a (PgeBlock ft bodyG j) ->
          do
            bodyG' <- rebuild bodyG
            j' <- rebuild j
            newNode $ Node () (PgeBlock ft bodyG' j')
        Node a (PgeLoop ft bodyG j) ->
          do
            bodyG' <- rebuild bodyG
            j' <- rebuild j
            newNode $ Node () (PgeLoop ft bodyG' j')
        Node a (PgeBr lIdx) ->
          newNode (Node () (PgeBr lIdx))
        Node a (PgeBrIf lIdx j) ->
          newNode . (Node () . PgeBrIf lIdx) =<< rebuild j
        Node a (PgeBrTable lIdxs lIdx) ->
          newNode (Node () $ PgeBrTable lIdxs lIdx)
        Node a (PgeCall fIdx j) ->
          newNode . (Node () . PgeCall fIdx) =<< rebuild j
        Node a (PgeCallIndirect ft j) ->
          newNode . (Node () . PgeCallIndirect ft) =<< rebuild j
        NodeNaturalEnd _ -> newNode $ NodeNaturalEnd ()
        NodeReturn _     -> newNode $ NodeReturn ()
        NodeTrapped _    -> newNode $ NodeTrapped ()
        NodeTerminal _   -> newNode $ NodeTerminal ()
        NodeStaticError  -> fail "Statically invalid graph"
  newNode :: GraphNode () -> RWST (IntMap LivenessState) () (IdList (GraphNode ())) m NodeId
  newNode n = S.state $ IdList.append n
  nodeLiveness :: NodeId -> RWST (IntMap LivenessState) () (IdList (GraphNode ())) m LivenessState
  nodeLiveness i =
    do
      ls <- R.ask
      failMaybeMsg "Missing liveness info" (IntMap.lookup i ls)
