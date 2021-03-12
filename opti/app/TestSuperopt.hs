
module TestSuperopt
  ( testSuperopt
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


-- TODO
import qualified Data.ByteString.Builder as LBS
import qualified Data.ByteString.Lazy as LBS
import qualified Lang.Wasm.BinaryUnparser as WUP
import           Helpers ( FuncDesc (..), buildFuncDescs, fgGraphSize )
import           Lang.Wasm.Superopt.GraphOpt ( Settings (..), superopt )
import           Helpers as H
import           Helpers ( FuncDesc (..), fgGraphSize, buildFuncDescs, mem0 )
import qualified Lang.Wasm.Superopt.MonadSuperopt as MS
import System.IO ( hFlush, stdout )
import Lang.Wasm.Optimizations.PullDrop ( pullDrop )
import Lang.Wasm.Optimizations.CleanDead ( removeDead )

-- Currently broken. TODO: Fix

testSuperopt :: IO ()
testSuperopt = return ()
--   do
--     putStrLn "---------------------------------------"
--     putStrLn "- - - - - - SUPEROPT TEST - - - - - -"
--     putStrLn "---------------------------------------"
--     forM_ ["sha256"] $ \name ->
--       do
--         content <- BS.readFile ("../data/macro/" ++ name ++ ".wasm")
--         startTime <- getTimeMs
--         putStrLn (ß"--- Module \"" . ß name . ß"\" ---" $ "")
--         tParseS <- getTimeMs
--         case parseOnly WP.wasmP content of
--           Left _err -> putStrLn "Failed to parse"
--           Right m  ->
--             do
--               tParseE <- getTimeMs
--               putStrLn (ß"Parsed in " . shows (tParseE - tParseS) . ß"ms" $ "")

--               let fs = mFuncsDefined m
--               putStrLn (ß"#Functions: " . shows (Vector.length fs) $ "")

--               putStrLn $ ß"Building graphs" ""

--               tGraphS <- getTimeMs
--               funcDescs <- evaluate $ force =<< buildFuncDescs m
--               tGraphE <- getTimeMs

--               funcs <- H.buildFuncDescs m
--               let numFuncs = length funcs

--               let settings = Settings (10*1000) (5*60*1000) (60*1000*1000) 100000 3000

--               (totalStats, xs) <- mapSnd reverse <$> foldM
--                 (\(allStats, zs) (i, d) ->
--                   do
--                     let ps = Ast.fParams $ fgFunc d
--                     let ls = Ast.fLocals $ fgFunc d
--                     let rt = Ast.fResults $ fgFunc d

--                     (g, stats) <- MS.runSuperUtil
--                           (runMaybeT $ superopt settings (fgSfc d) (fgGraph d) ps ls rt (mem0 m))
--                           (30*60*1000) Nothing False
                    
--                     putStrLn "Done superoptimizing function"

--                     hFlush stdout

--                     outAst <- H.cleanAndToAst (d { fgGraph = fromMaybe (fgGraph d) g })
--                     outAst' <- evaluate $ force outAst
                    
--                     let allStats' = allStats <> stats

--                     return (allStats', (stats,outAst'):zs)
--                 ) mempty (zip [0..] funcs)
              
--               definedFuncs <- evaluate $ force $ Vector.fromList $ map snd xs
--               b <- evaluate $ force $ LBS.toLazyByteString (WUP.wasmW $ m { mFuncsDefined = definedFuncs })
--               LBS.writeFile ("../data/" ++ name ++ ".out.wasm") b
              
--               b <- evaluate $ force $ LBS.toLazyByteString (WUP.wasmW $ pullDrop $ m { mFuncsDefined = definedFuncs })
--               LBS.writeFile ("../data/" ++ name ++ ".out.clean.wasm") b
--         putStrLn ""