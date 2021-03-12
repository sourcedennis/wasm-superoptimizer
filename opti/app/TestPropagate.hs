-- | Tests the propagation of configurations over control flow.
--
-- WARNING: Do not attempt this on a weak machine. You probably need 64GiB+ of
-- memory.
module TestPropagate
  ( testPropagate
  ) where

import Melude
-- Stdlib imports
import           Control.Monad.Fail ( MonadFail )
import           Control.Monad ( forM_, when )
import           Control.Exception ( evaluate )
import           Control.DeepSeq ( force )
import           Text.Printf ( printf )
-- Extra stdlib imports
import qualified Data.ByteString as BS
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.Vector as Vector
import qualified Data.ByteString.Builder as LBS
import qualified Data.ByteString.Lazy as LBS
-- External library imports
import qualified Data.IdList as IdList
import           Data.Attoparsec.ByteString ( parseOnly )
-- Local library imports
import qualified Algorithm.Graph as AG
import qualified Lang.Wasm.Data as WD
import qualified Lang.Wasm.Ast as Ast
import qualified Lang.Wasm.Process as P
import Lang.Wasm.Ast
  ( PVal (..), Module (..) )
import qualified Lang.Wasm.BinaryParser as WP
import qualified Lang.Wasm.BinaryUnparser as WUP
-- Local imports
import Lang.Wasm.Process
import qualified Lang.Wasm.Symbolics as Y
import qualified Lang.Wasm.Symbolic.Configuration as Cfg
import qualified Lang.Wasm.Symbolic.Configuration.Propagation as PG
import           Helpers as H
import           Helpers ( FuncDesc (..), fgGraphSize, buildFuncDescs, mem0 )

import Data.List ( sortOn )

import Lang.Wasm.Superopt.PartialEval ( findConstants, Mode (..) )
import qualified Lang.Wasm.Superopt.MonadSuperopt as MS
import Lang.Wasm.Optimizations.CleanFuncs ( removeUnusedFuncs )
import Lang.Wasm.Optimizations.PullDrop ( pullDrop )
import Lang.Wasm.Optimizations.CleanDead ( removeDead )


import System.IO ( hFlush, stdout )

testPropagate :: IO ()
testPropagate =
  do
    putStrLn "----------------------------------------"
    putStrLn "- - - INFORMATION PROPAGATION TEST - - -"
    putStrLn "----------------------------------------"
    let envBound = 20000 -- 3_000 for big programs. 10_000 for smaller programs
        timePerConst = 1000 -- ms
        isSubstConsts = True -- If False, only eliminate branches
        mode = ModeExprConsts

    -- forM_ ["macro/sha256", "projects/wasm_lua/main", "macro/z3w", "macro/lua_mini", "macro/raytracer"] $ \name ->
    forM_ ["slumps/addition_chains", "slumps/pascal_matrix_generation", "slumps/babbage_problem"
          ,"slumps/banker", "slumps/bitwise_IO", "slumps/eban_numbers", "slumps/flipping_bits_game"
          ,"slumps/lua", "slumps/resistor_mesh", "slumps/zebra_puzzle"] $ \name ->
      do
        content <- BS.readFile ("../data/" ++ name ++ ".wasm")
        startTime <- getTimeMs
        putStrLn (ß"--- Module \"" . ß name . ß"\" ---" $ "")
        tParseS <- getTimeMs
        case parseOnly WP.wasmP content of
          Left _err -> putStrLn "Failed to parse"
          Right mLarge  ->
            do
              tParseE <- getTimeMs
              putStrLn (ß"Parsed in " . shows (tParseE - tParseS) . ß"ms" $ "")

              m' <- removeUnusedFuncs mLarge
              m <- evaluate $ force m'
              tConvertE <- getTimeMs
              putStrLn (ß"Cleaned in " . shows (tConvertE - tParseE) . ß"ms" $ "")
              
              b <- evaluate $ force $ LBS.toLazyByteString (WUP.wasmW m)
              tParseE <- getTimeMs

              LBS.writeFile ("../data/" ++ name ++ ".clean.wasm") b

              let fs = mFuncsDefined m
              putStrLn (ß"#Functions: " . shows (Vector.length fs) $ "")

              t3 <- getTimeMs

              funcs <- H.buildFuncDescs m
              let numFuncs = length funcs

              (totalStats, xs) <- mapSnd reverse <$> foldM
                (\(allStats, zs) (i, d) ->
                  do
                    let cfg =
                          Cfg.init
                            (map Ast.globalType $ Ast.globals m)
                            (Vector.toList $ Ast.fParams $ fgFunc d)
                            (Vector.toList $ Ast.fLocals $ fgFunc d)
                            (mem0 m)

                    -- mapM_ putStrLn $ showGraphLines (fgGraph d)

                    putStrLn $ ß"Func " . shows i . ß"/" . shows numFuncs . ß" " . shows (fgIdx d) $ ""

                    s <- getTimeMs
                    cfgs <- evaluate =<< PG.propagate envBound (fgSfc d) (Ast.fResults $ fgFunc d) cfg (fgGraph d)
                    e <- getTimeMs
                    putStrLn $ ß"Propagated over function (size=". shows (fgGraphSize d).ß"). Time: " . shows (e-s) . ß"ms" $ ""
                    let vals = map (Y.size . view Cfg.cfgSymbolics) $ IntMap.elems cfgs
                    putStrLn $ ß"Vals: " . shows (avg vals) . ß" - " . shows (foldr max 0 vals) . ß" Time: " . shows (e-s) . ß"ms" $ ""

                    s' <- getTimeMs
                    (g', stats) <- MS.runSuperUtil (runMaybeT $ findConstants timePerConst mode (fgSfc d) (fgGraph d) cfgs) (30*60*1000) Nothing False
                    evaluate $ force g'
                    evaluate $ force stats
                    e' <- getTimeMs
                    putStrLn $ ß"Constant replacements done. Time: " . shows (e'-s') . ß"ms" $ ""

                    hFlush stdout

                    print stats

                    allStats' <- evaluate $ force (allStats <> stats)
                    putStrLn $ ß"All stats " . shows allStats' $ ""

                    outAst <- H.cleanAndToAst (d { fgGraph = fromMaybe (fgGraph d) g' })
                    outAst' <- evaluate $ force outAst

                    return (allStats', (stats,outAst'):zs)
                ) mempty (zip [0..] funcs)

              -- let invalids = filter (not . snd) xs
              -- case sortOn (fgGraphSize . fst) invalids of
              --   [] -> putStrLn "All ok"
              --   ((d, _):_) ->
              --     do
              --       print $ fgIdx d
              --       let cfg =
              --             Cfg.init
              --               (map Ast.globalType $ Ast.globals m)
              --               (Vector.toList $ Ast.fParams $ fgFunc d)
              --               (Vector.toList $ Ast.fLocals $ fgFunc d)
              --               (mem0 m)
              --       mapM_ putStrLn $ showGraphLines (fgGraph d)
              --       res <- PG.propagate' (fgSfc d) (Ast.fResults $ fgFunc d) cfg (fgGraph d)
              --       mapM_ print $ IntMap.assocs res
              --       -- putStrLn "Function propagation done"

              t4 <- getTimeMs

              putStrLn $ ß"Propagation done. Time: " . shows (t4-t3) . ß"ms" $ ""
              print $ mconcat $ map fst xs
              
              definedFuncs <- evaluate $ force $ Vector.fromList $ map snd xs
              b <- evaluate $ force $ LBS.toLazyByteString (WUP.wasmW $ m { mFuncsDefined = definedFuncs })
              LBS.writeFile ("../data/" ++ name ++ ".out.wasm") b
              
              b <- evaluate $ force $ LBS.toLazyByteString (WUP.wasmW $ pullDrop $ m { mFuncsDefined = definedFuncs })
              LBS.writeFile ("../data/" ++ name ++ ".out.clean.wasm") b
        putStrLn ""

avg :: [Int] -> Int
avg [] = 0
avg xs = sum xs `div` length xs
