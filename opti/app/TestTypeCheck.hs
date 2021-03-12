
-- | Type checks the obtained process graphs.
--
-- In practice, every WebAssembly file - that is produced by a respectable
-- compiler - will be valid/type correct. This thus mainly improves confidence
-- toward the correctness of graph construction and forward/backward execution.
module TestTypeCheck
  ( testTypeCheck
  ) where

import Melude
-- Stdlib imports
import           Control.Monad ( forM_, when, filterM )
import           Control.Monad.Fail ( MonadFail (..) )
import           Control.Exception ( evaluate )
import           Control.DeepSeq ( NFData, force )
-- Extra stdlib imports
import qualified Data.ByteString as BS
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.Vector as Vector
-- External library imports
import           Data.Attoparsec.ByteString ( parseOnly )
-- Local library imports
import qualified Algorithm.Graph as AG
import qualified Lang.Wasm.Data as WD
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( Module (..), DefinedFunc (..), FuncType, GlobalType, ValType, TypeIdx (..), FuncIdx (..), InstrCtx (..)
  , ResultType
  , funcType, funcs, globals, globalType
  )
import qualified Lang.Wasm.BinaryParser as WP
-- Local imports
import           Lang.Wasm.Process
import qualified Lang.Wasm.Dataflow.TypeCheck as TC
import           Lang.Wasm.Dataflow.TypeCheck ( NodeType (..), FullStack )
import qualified Algorithm.Dataflow as Dataflow
import           Helpers ( FuncDesc (..), buildFuncDescs, fgGraphSize )


testTypeCheck :: IO ()
testTypeCheck =
  do
    putStrLn "---------------------------------------"
    putStrLn "- - - - - - TYPE CHECK TEST - - - - - -"
    putStrLn "---------------------------------------"
    forM_ ["micro_popcount","sha256", "lua", "z3w"] $ \name ->
    -- forM_ ["micro_popcount"] $ \name ->
    -- forM_ ["sha256", "lua"] $ \name ->
      do
        content <- BS.readFile ("../data/macro/" ++ name ++ ".wasm")
        startTime <- getTimeMs
        putStrLn (ß"--- Module \"" . ß name . ß"\" ---" $ "")
        tParseS <- getTimeMs
        case parseOnly WP.wasmP content of
          Left _err -> putStrLn "Failed to parse"
          Right m  ->
            do
              tParseE <- getTimeMs
              putStrLn (ß"Parsed in " . shows (tParseE - tParseS) . ß"ms" $ "")

              let fs = mFuncsDefined m
              putStrLn (ß"#Functions: " . shows (Vector.length fs) $ "")

              putStrLn $ ß"Building graphs" ""

              tGraphS <- getTimeMs
              funcDescs <- evaluate $ force =<< buildFuncDescs m
              tGraphE <- getTimeMs

              putStrLn $ ß"Done building graphs (" . shows (tGraphE - tGraphS) . ß"ms)" $ ""

              t1 <- getTimeMs
              typeCheckedFuncsFwd <- evaluate $ map (\d -> (typeCheckFuncFwd d, d)) funcDescs
              incorrectFuncs1 <- evaluate $ map snd $ filter (\(b,_) -> b /= Just True) typeCheckedFuncsFwd
              t2 <- getTimeMs

              putStrLn (ß"Fwd: " . shows (t2 - t1) . ß"ms" $ "")

              t3 <- getTimeMs
              typeCheckedFuncsBwd <- evaluate $ map (\d -> (typeCheckFuncBwd d, d)) funcDescs
              incorrectFuncs4 <- evaluate $ map snd $ filter (\(b,_) -> b /= Just True) typeCheckedFuncsBwd
              t4 <- getTimeMs

              putStrLn (ß"Bwd: " . shows (t4 - t3) . ß"ms" $ "")
              
              case argMin fgGraphSize incorrectFuncs1 of
                Nothing ->
                  putStrLn $ ß"All " . shows (Vector.length fs) . ß" functions are type-correct (Fwd)" $ ""
                Just d ->
                  do
                    let numIncorrect = length incorrectFuncs1
                    putStrLn $ shows numIncorrect . ß"/" . shows (length fs) . ß" functions produce type-incorrect graphs." $ ""
                    putStrLn $ ß"Smallest graph (" . shows (Ast.unFuncIdx $ fgIdx d) . ß") has " . shows (fgGraphSize d) . ß" nodes" $ ""
                    types <- singleGraphFwdTypes (fgSfc d) (Ast.fResults $ fgFunc d) (fgGraph d)
                    putStrLn $ ß"Function result: " . shows (Ast.fResults $ fgFunc d) $ ""
                    putStrLn "----- AST -----"
                    print (Ast.fBody $ fgFunc d)
                    putStrLn "----- Graph -----"
                    let (rootI, gNodes, endI) = fgGraph d
                    putStrLn $ ß"Root=" . shows rootI . ß" End=" . shows endI $ ""
                    putStrLn ""
                    zipWithM_ (\i g -> putStrLn $ shows i . ß": " . shows g $ "") [0..] (Vector.toList gNodes)
                    putStrLn "----- Flow -----"
                    -- let flow = fromJust $ graphFlow (fgSfc d) (Ast.fResults $ fgFunc d) (fgGraph d)
                    flow <- graphFlow (fgSfc d) (Ast.fResults $ fgFunc d) (fgGraph d)
                    mapM_ (\i -> putStrLn $ shows i . ß": " . shows (flow i) $ "") [0..Vector.length gNodes-1]
                    putStrLn "----- Types -----"
                    mapM_ (\(i,v) -> putStrLn $ shows i . ß": " . shows v $ "") $ IntMap.toList types

              case argMin fgGraphSize incorrectFuncs4 of
                Nothing ->
                  putStrLn $ ß"All " . shows (length fs) . ß" functions are type-correct (Bwd)" $ ""
                Just d ->
                  do
                    let numIncorrect = length incorrectFuncs1
                    putStrLn $ shows numIncorrect . ß"/" . shows (length fs) . ß" functions produce type-incorrect graphs (bwd)." $ ""
                    putStrLn $ ß"Smallest graph (" . shows (Ast.unFuncIdx $ fgIdx d) . ß") has " . shows (fgGraphSize d) . ß" nodes" $ ""
                    types <- singleGraphBwdTypes (fgSfc d) (Ast.fResults $ fgFunc d) (fgGraph d)
                    putStrLn $ ß"Function result: " . shows (Ast.fResults $ fgFunc d) $ ""
                    putStrLn "----- AST -----"
                    print (Ast.fBody $ fgFunc d)
                    putStrLn "----- Graph -----"
                    let (rootI, gNodes, endI) = fgGraph d
                    putStrLn $ ß"Root=" . shows rootI . ß" End=" . shows endI $ ""
                    putStrLn ""
                    zipWithM_ (\i g -> putStrLn $ shows i . ß": " . shows g $ "") [0..] (Vector.toList gNodes)
                    putStrLn "----- Flow (bwd) -----"
                    flow <- graphFlow (fgSfc d) (Ast.fResults $ fgFunc d) (fgGraph d)
                    let invFlow = AG.invertFlow flow rootI
                    mapM_ (\i -> putStrLn $ shows i . ß": " . shows (invFlow i) $ "") [0..Vector.length gNodes-1]
                    putStrLn "----- Types -----"
                    mapM_ (\(i,v) -> putStrLn $ shows i . ß": " . shows v $ "") $ IntMap.toList types

              putStrLn $ ß"Type checked" ""
        putStrLn ""

typeCheckFuncFwd :: MonadFail m => FuncDesc -> m Bool
typeCheckFuncFwd d =
  do
    nodeTypes <- singleGraphFwdTypes (fgSfc d) (Ast.fResults $ fgFunc d) (fgGraph d)
        
        -- `True` iff every node is assigned a valid & consistent type
    let isNodesOk = all TC.isNodeTypeOk nodeTypes
        (_, _, terminalI) = fgGraph d
        isTerminalOk = IntMap.lookup terminalI nodeTypes == Just (TypeOk $ TC.terminalStack $ Ast.fResults $ fgFunc d)
    return (isNodesOk && isTerminalOk)
  
typeCheckFuncBwd :: MonadFail m => FuncDesc -> m Bool
typeCheckFuncBwd d =
  do
    nodeTypes <- singleGraphBwdTypes (fgSfc d) (Ast.fResults $ fgFunc d) (fgGraph d)
        
        -- `True` iff every node is assigned a valid & consistent type
    let isNodesOk = all TC.isNodeTypeOk nodeTypes
        (rootI, _, _) = fgGraph d
        isRootOk = IntMap.lookup rootI nodeTypes == Just (TypeOk ([]:|[[]]))
    return (isNodesOk && isRootOk)

-- | Performs forward type checking on a deterministic process graph.
singleGraphFwdTypes :: MonadFail m => InstrCtx -> ResultType -> GraphFrozen a -> m (IntMap NodeType)
singleGraphFwdTypes sfc rt g@(rootI,_,_) =
  do
    flow <- graphFlow sfc rt g
    let graphTransfer = Dataflow.mapFlow (TC.transfer sfc) flow
    return $ Dataflow.fix TC.confluence (initValFwd rootI) graphTransfer (IntSet.singleton rootI)

-- | Performs backward type checking on a deterministic process graph.
singleGraphBwdTypes :: MonadFail m => InstrCtx -> ResultType -> GraphFrozen a -> m (IntMap NodeType)
singleGraphBwdTypes sfc rt g@(rootI,_,terminalI) =
  do
    flow <- graphFlow sfc rt g
    let graphInvFlow  = AG.invertFlow flow rootI
        graphTransfer = Dataflow.mapFlow (TC.transferBwd sfc) graphInvFlow
    return $ Dataflow.fix TC.confluence (initValBwd terminalI rt) graphTransfer (IntSet.singleton terminalI)

-- | Produces the function that produces the initial value for type checking.
-- The initial value is the empty stack for the root node, and unknown (bottom)
-- for any other node.
initValFwd :: Int -> ( Int -> NodeType )
initValFwd rootI nodeI
  -- Technically, the function body is a block. The "scope outside the function
  -- call" has its own stack. So, two empty scope-stacks are included; one for
  -- the scope outside the function, and one for the scope inside the function.
  | rootI == nodeI  = TypeOk ( [] :| [[]] )
  | otherwise       = TypeUnknown -- bottom

initValBwd :: Int -> ResultType -> ( Int -> NodeType )
initValBwd terminalI rt nodeI
  | terminalI == nodeI  = TypeOk ((map (WD.val () () () ()) $ reverse $ Vector.toList rt) :| [])
  | otherwise           = TypeUnknown -- bottom

-- | Produces the function that produces the initial value for type checking.
-- The initial value is the empty stack for the root node, and unknown (bottom)
-- for any other node.
initVals :: IntMap FullStack -> ( Int -> NodeType )
initVals m nodeI = maybe TypeUnknown TypeOk (nodeI `IntMap.lookup` m)
