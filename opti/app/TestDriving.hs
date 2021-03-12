{-# LANGUAGE MultiParamTypeClasses #-}

-- | This test hooks into the driving parts of the superoptimizer.
module TestDriving
  ( testDriving
  ) where

import Melude
-- Stdlib imports
import           Control.Monad ( forM_, when )
import           Control.Exception ( evaluate )
import           Control.DeepSeq ( force )
import           Text.Printf ( printf )
import qualified System.Timeout as Timeout
-- Extra stdlib imports
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as LBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.Vector as Vector
-- External library imports
import qualified Data.IdList as IdList
import           Data.Attoparsec.ByteString ( parseOnly )
-- Local library imports
import qualified Algorithm.Graph as AG
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast
  ( PVal (..), Module (..) )
import qualified Lang.Wasm.BinaryParser as WP
import qualified Lang.Wasm.BinaryUnparser as WUP
-- Local imports
import qualified Lang.Wasm.Symbolic.Configuration as Cfg
import qualified Lang.Wasm.Process as P
import Lang.Wasm.Process
  -- ( isNodeTerminal, isNodeTrapped, singleGraphFlow )
import qualified Lang.Wasm.Dataflow.Liveness as L
import           Lang.Wasm.Dataflow.Liveness
  ( LivenessLattice (..), LivenessState (..) )
import qualified Algorithm.Dataflow as Dataflow
import           Helpers ( FuncDesc (..), buildFuncDescs )
-- import qualified Lang.Wasm.Synthesis as LinearSynth
import           Binding.Z3.Z3Solver ( runZ3SolverTimeoutAndCount )
import qualified Helpers as H
import Lang.Wasm.Drive ( drive )
import Lang.Wasm.Optimizations.PullDrop ( pullDrop )


testDriving :: IO ()
testDriving =
  -- forM_ ["idgcd","babbage","bubblesort"] $ \name ->
  forM_ ["drive/two_ifs", "drive/idgcd", "drive/bubblesort4", "drive/babbage"] $ \name ->
    do
      content <- BS.readFile ("../data/" ++ name ++ ".wasm")
      tParseS <- getTimeMs
      case parseOnly WP.wasmP content of
        Left err -> print ("Failed to parse: " ++ err)
        Right m  ->
          do
            tParseE <- getTimeMs
            putStrLn (ß"Parsed in " . shows (tParseE - tParseS) . ß"ms" $ "")

            let fs = mFuncsDefined m
            putStrLn (ß"#Functions: " . shows (Vector.length fs) $ "")

            putStrLn "Building graphs"

            tGraphS <- getTimeMs
            descs' <- buildFuncDescs m
            descs <- evaluate $ force descs'
            tGraphE <- getTimeMs

            putStrLn $ ß"Done building graphs (" . shows (tGraphE - tGraphS) . ß"ms)" $ ""

            outFuncs <- forM descs
              $ \d ->
                do
                  putStrLn ""
                  putStrLn ""
                  putStrLn name
                  putStrLn $ intercalateShow (ß"\n") (map ß $ P.showGraphLines $ fgGraph d) ""
                  putStrLn ""
                  let ft = Ast.fType (fgFunc d)
                      stack = []
                      rt = Ast.ftResults ft
                      tree = P.boundTree 20 $ P.buildTree (fgSfc d) stack rt (fgGraph d)
                  putStrLn $ intercalateShow (ß"\n") (map ß $ P.showTreeLines tree) ""
                  putStrLn ""

                  let a = fgFunc d
                  let rt = Ast.fResults a
                  let c =
                        Cfg.init
                          (map Ast.globalType $ Ast.globals m)
                          (Vector.toList $ Ast.fParams a)
                          (Vector.toList $ Ast.fLocals a)
                          (H.mem0 m)

                  let bound = 300000
                  let timeMs = 60 * 1000 -- 30 * 60 * 1000

                  putStrLn "go"

                  startTime <- liftIO getTimeMs
                  (res1, numZ3Calls) <- liftIO $ runZ3SolverTimeoutAndCount (drive (fgSfc d) rt bound c (fgGraph d)) timeMs
                  endTime <- liftIO getTimeMs

                  putStrLn $ showString "Done driving. " . shows (endTime - startTime) . showString "ms" $ ""
                  putStrLn $ showString "Num Z3 calls: " . shows numZ3Calls $ ""

                  newAst <-
                    case fmap (mapTree (fmap fst)) res1 of
                      Nothing ->
                        do
                          putStrLn "Failed to build tree"
                          return $ fgFunc d
                      Just tree2 ->
                        do
                          putStrLn "Done"
                          -- putStrLn $ intercalateShow (ß"\n") (map ß $ P.showTreeLines tree2) ""
                          case P.treeToGraph tree2 of
                            Nothing ->
                              do
                                putStrLn "Failed to build graph"
                                return $ fgFunc d
                            Just g' ->
                              do
                                ast <- H.cleanAndToAst (d { fgGraph = g' })
                                putStrLn ""
                                return ast
                  endTime2 <- liftIO getTimeMs
                  putStrLn $ showString "Total done. " . shows (endTime2 - startTime) . showString "ms" $ ""
                  return newAst

            putStrLn "Fully done"
            
            -- putStrLn $ ß"Propagation done. Time: " . shows (t4-t3) . ß"ms" $ ""
            -- print $ mconcat $ map fst xs
            
            -- definedFuncs <- evaluate $ force $ Vector.fromList $ map snd xs
            b <- evaluate $ force $ LBS.toLazyByteString (WUP.wasmW $ m { mFuncsDefined = Vector.fromList outFuncs })
            LBS.writeFile ("../data/" ++ name ++ ".out.wasm") b
            
            b <- evaluate $ force $ LBS.toLazyByteString (WUP.wasmW $ pullDrop $ m { mFuncsDefined = Vector.fromList outFuncs })
            LBS.writeFile ("../data/" ++ name ++ ".out.clean.wasm") b

