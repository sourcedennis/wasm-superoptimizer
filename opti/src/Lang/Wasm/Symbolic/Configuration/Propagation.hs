{-# LANGUAGE ScopedTypeVariables #-}

-- | Information propagation over process graphs
module Lang.Wasm.Symbolic.Configuration.Propagation where

import Melude
-- Stdlib imports
-- Extra stdlib imports
import qualified Control.Monad.State as S
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- Local library imports
import qualified Algorithm.Graph as AG
import qualified Algorithm.Dataflow as Dataflow
import qualified Lang.Wasm.Process as P
import           Lang.Wasm.Process ( GraphFrozen (..), FlowEdge (..), NodeId )
import Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast
  ( WI32 (..), WI64 (..), WF32, WF64, TI32, TI64, TF32, TF64, IUnop (..), IBinop (..)
  , FUnop (..), FBinop (..), ITestop (..), IRelop (..), FRelop (..), Cvtop (..)
  , PrmInstr (..), VarInstr (..), MemInstr (..), SimpleInstr (..), MemArg (..)
  , Sx (..), PVal (..), InstrCtx, ResultType
  )
-- Local imports
import qualified Lang.Wasm.Algebra as A
import qualified Lang.Wasm.Dataflow as WFlow
import           Lang.Wasm.Dataflow ( FlowAlgebra (..) )
import qualified Lang.Wasm.Symbolic.Configuration as Conf
import           Lang.Wasm.Symbolic.Configuration
  ( Configuration (..), ConfigurationCore (..) )
import           Lang.Wasm.Symbolic.Configuration.ExecSimple
  ( execSimpleInstr, configurationAlgebra )
import qualified Lang.Wasm.Symbolic.General as SG
import qualified Lang.Wasm.Symbolics as Y
import           Lang.Wasm.Symbolics ( MonadSymbolics, Symbolic, SymbolicVal (..) )

import Control.Lens ( over )

import Data.List ( intercalate )


type LoopEntries = IntSet

-- | Maximum number of expressions in a symbolic environment. Big environments
-- max eat RAM, causing out-of-memory errors. 3000 works.
type SymbolicsBound = Int


propagate :: forall m a env
          .  MonadFail m
          => SymbolicsBound
          -> InstrCtx
          -> ResultType
          -> Configuration env
          -> GraphFrozen a
          -> m (IntMap (Configuration env))
propagate bound ctx rt c g@(rootI, _, _) =
  do
    flow <- P.graphFwdFlow ctx rt g
    let gOrder = AG.quasiTopologicalOrder (IntSet.fromList . map snd . flow) (IntSet.singleton rootI)
    loopEntries <- P.loopEntries g
    let res = Dataflow.fixTopological (unionConf loopEntries) initVal (map (mapFst transfer) . flow) gOrder
    -- trace (intercalate "\n" $ map show $ IntMap.assocs res) $ return ()
    mapM (failMaybeMsg "Statically invalid transfer") res
  where
  initVal :: NodeId -> Maybe (Configuration env)
  initVal i
    | i == rootI  = Just c
    | otherwise   = Nothing
  unionConf :: LoopEntries
            -> NodeId
            -> Maybe (Configuration env)
            -> Maybe (Configuration env)
            -> Maybe (Configuration env)
  unionConf loopEntries i (Just a) (Just b)
    | i `IntSet.member` loopEntries  = Just $ Conf.toTop a
    | Y.size (a^.Conf.cfgSymbolics) + Y.size (b^.Conf.cfgSymbolics) > bound  =
        -- Make sure the configuration doesn't get too big.
        Just $ Conf.toTop a
    | otherwise = Conf.union a b
  unionConf loopEntries i (Just a) _
    | i `IntSet.member` loopEntries  = Just $ Conf.toTop a
    | otherwise                      = Just a
  unionConf loopEntries i _ (Just b)
    | i `IntSet.member` loopEntries  = Just $ Conf.toTop b
    | otherwise                      = Just b
  unionConf _ _ _ _ = Nothing
  transfer :: FlowEdge -> Maybe (Configuration env) -> Maybe (Configuration env)
  transfer _    Nothing  = Nothing
  transfer edge (Just c) =
    let res  = WFlow.execFlowAlgebra (fromMaybe (error "Missing func") . Ast.itxFuncType ctx) configurationFlowAlgebra edge
        res2 = execStateT res (c ^. Conf.cfgCore)
        (core, sym) = Y.runSymbolicsState (runMaybeT res2) (c ^. Conf.cfgSymbolics)
    in Configuration <$> core <*> pure sym

type SymbolicFlowAlgebra env =
  FlowAlgebra
    (Symbolic env TI32)
    (Symbolic env TI64)
    ()
    ()
    (Maybe (SG.SymbolicMem env))

configurationFlowAlgebra :: Y.MonadSymbolics env m
                         => SymbolicFlowAlgebra env (StateT (ConfigurationCore env) (MaybeT m))
configurationFlowAlgebra =
  FlowAlgebra {
    instrAlgebra   = configurationAlgebra
  , callExternal   =
      \_ gs mem args resTypes -> 
        do
          gs' <- mapM (Y.vUnknown . Ast.valType) gs
          mem' <-
            case mem of
              Nothing -> pure Nothing
              Just mem' -> Just <$> SG.topMem mem'
          res <- mapM Y.vUnknown resTypes
          return (gs', mem', res)
  , callIndirect   =
      \_ gs mem args resTypes -> 
        do
          gs' <- mapM (Y.vUnknown . Ast.valType) gs
          mem' <-
            case mem of
              Nothing -> pure Nothing
              Just mem' -> Just <$> SG.topMem mem'
          res <- mapM Y.vUnknown resTypes
          return (gs', mem', res)
  
    -- Introduces path conditions
  , assertTrue     =
      \b -> modifyM $ Conf.ccConstraint (\c -> Y.vBAnd c =<< Y.vBI32 b)
  , assertConstI32 =
      \v i -> modifyM $ Conf.ccConstraint (\c -> Y.vBAnd c =<< Y.vI32Eq v =<< Y.vI32Const i)
  , assertGeq      =
      \v i -> modifyM $ Conf.ccConstraint (\c -> Y.vBAnd c =<< Y.vI32Ge U v =<< Y.vI32Const i)
  }
