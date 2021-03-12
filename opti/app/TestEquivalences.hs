{-# LANGUAGE MultiParamTypeClasses #-}

-- | The WASM modules in the `data/eqcheck/` directory contain two functions
-- each. The equality (or inequality) between those functions is determined by
-- the test cases in here.
module TestEquivalences
  ( testEquivalences
  , checkFuncEquivalence
  ) where

import Melude
-- Extra stdlib imports
import qualified Data.ByteString as BS
import           Control.Monad ( forM_ )
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
-- External library imports
import           Data.Attoparsec.ByteString ( parseOnly )
-- Local library imports
import qualified Lang.Wasm.BinaryParser as WP
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( lookupFunc, FuncIdx (..), DefinedFunc (..), Func (..)
  , FuncType (..), GlobalType (..), Limits (..), fParams, fResults
  , Module
  )
-- Local imports
import           Lang.Wasm.Dataflow.Liveness as L ( terminalState )
import           Lang.Wasm.Process ( buildGraph, buildTree, freezeGraph )
import qualified Lang.Wasm.Symbolics as Y
import qualified Lang.Wasm.Symbolic.Configuration as Cfg
import qualified Lang.Wasm.Symbolic.ProgramState as SymState
import           Lang.Wasm.Symbolic.ProgramState.Exec ( execTree )
import           Binding.Z3.Z3Solver ( runZ3SolverTimeout )
import           Lang.Wasm.Solver ( areUnequal, EvalResult (..) )
import qualified Helpers as H


testEquivalences :: IO ()
testEquivalences =
  forM_ ["div0","div0_2","mulshl","polynomial","idgcd","memorder","popcount","popcount64","babbage","memle","memarray","leqand"] $ \name ->
    do
      content <- BS.readFile ("../data/eqcheck/" ++ name ++ ".wasm")
      case parseOnly WP.wasmP content of
        Left err -> print ("Failed to parse: " ++ err)
        Right m  ->
          let Just (FuncDefined f1) = lookupFunc (FuncIdx 0) m
              Just (FuncDefined f2) = lookupFunc (FuncIdx 1) m
          in
          do
            -- let funcTypes = Vector.fromList $ map AST.funcType $ AST.funcs m
            -- let globalTypes = Vector.fromList $ map AST.globalType $ AST.globals m
            res <- runMaybeT $ checkFuncEquivalence m f1 f2
            case res of
              Nothing        -> putStrLn "Crashed"
              Just (t,True)  -> putStrLn (ß "Programs (" . ß name . ß ") are equal (" . shows t . ß "ms)" $ "")
              Just (t,False) -> putStrLn (ß "Programs (" . ß name . ß ") are UNEQUAL (or timeout - " . shows t . ß "ms)" $ "")
            return ()

checkFuncEquivalence :: Module
                     -> DefinedFunc
                     -> DefinedFunc
                     -> MaybeT IO (TimeMs, Bool)
checkFuncEquivalence m a b =
  if fParams a /= fParams b || fLocals a /= fLocals b || fResults a /= fResults b then
    do
      liftIO $ putStrLn "Unequal static structure"
      return (0, False)
  else
    do
      let sfc = Ast.funcCtx m a -- StaticFuncContext funcs globals (Vector.fromList (fParams a ++ fLocals a))
      gA@(rootA,graphA,_) <- buildGraph sfc (fBody a)
      gB@(rootB,graphB,_) <- buildGraph sfc (fBody b)

      -- liftIO $ print graphA
      -- liftIO $ print graphB

      let treeA = buildTree sfc [] (fResults a) (freezeGraph gA)
      let treeB = buildTree sfc [] (fResults b) (freezeGraph gB)
      
      -- liftIO $ print treeA
      -- liftIO $ print treeB

      let initConfiguration =
            Cfg.init
              (map Ast.globalType $ Ast.globals m)
              (Vector.toList $ fParams a)
              (Vector.toList $ fLocals a)
              (H.mem0 m)

      let (initState, (sym, constraint)) = SymState.fromConfiguration initConfiguration

      -- liftIO $ print initState
      -- liftIO $ print sym

      let bound = 300000
      let timeMs = 60 * 1000 -- 30 * 60 * 1000

      -- liftIO $ putStrLn "Output of first"
      startTime <- liftIO getTimeMs
      res1 <- liftIO $ runZ3SolverTimeout (runMaybeT $ Y.runExtSymbolicsStateT (execTree bound treeA True initState constraint) sym) timeMs
      Just (Just out1, sym') <- return res1
      -- liftIO $ print out1
      
      -- liftIO $ putStrLn "Output of second"
      res2 <- liftIO $ runZ3SolverTimeout (runMaybeT $ Y.runExtSymbolicsStateT (execTree bound treeB True initState constraint) sym') timeMs
      Just (Just out2, sym'') <- return res2
      -- liftIO $ print out2
      -- liftIO $ print sym''

      -- Use a simple liveness description of the final state. All globals and
      -- stack values are live. Locals are all dead.
      let l = L.terminalState sfc (fResults a)

      res <- liftIO $ runZ3SolverTimeout (areUnequal (sym'', constraint) initState out1 out2 l) timeMs

      case res of
        EvSat x -> liftIO $ print x
        _ -> return ()

      -- liftIO $ print res
      endTime <- liftIO getTimeMs
      
      -- liftIO $ print (endTime - startTime)

      return (endTime - startTime, res == EvUnsat)
