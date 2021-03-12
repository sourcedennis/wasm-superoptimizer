
-- | Synthesise linear instruction sequence for some input functions.
module TestSynthesis where

import Melude
-- Stdlib imports
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
-- External library imports
import qualified Data.IdList as IdList
import           Data.Attoparsec.ByteString ( parseOnly )
-- Local library imports
import qualified Algorithm.Graph as AG
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast
  ( PVal (..), Module (..) )
import qualified Lang.Wasm.BinaryParser as WP
-- Local imports
import qualified Lang.Wasm.Symbolic.Configuration as Cfg
import qualified Lang.Wasm.Process as P
import qualified Lang.Wasm.Dataflow.Liveness as L
import           Lang.Wasm.Dataflow.Liveness
  ( LivenessLattice (..), LivenessState (..) )
import qualified Algorithm.Dataflow as Dataflow
import           Helpers ( FuncDesc (..), buildFuncDescs )
import qualified Lang.Wasm.Synthesis as LinearSynth
import           Binding.Z3.Z3Solver ( runZ3SolverTimeout )
import           Lang.Wasm.Drive ( drive )
import qualified Helpers as H
import qualified Lang.Wasm.Superopt.MonadSuperopt as SU

import TestEquivalences ( checkFuncEquivalence )


testSynthesis :: IO ()
testSynthesis =
  do
    putStrLn "--------------------------------------"
    putStrLn "- - - - - - SYNTHESIS TEST - - - - - -"
    putStrLn "--------------------------------------"
    forM_ ["micro/transitive_break","micro/popcount","micro/idgcd","eqcheck/mulshl"
          ,"eqcheck/leqand","eqcheck/popcount64"] $ \name ->
    -- forM_ ["eqcheck/mulshl","micro/leqand"] $ \name ->
      do
        content <- BS.readFile ("../data/" ++ name ++ ".wasm")
        startTime <- getTimeMs
        putStrLn (ß"--- Module \"" . ß name . ß"\" ---" $ "")
        tParseS <- getTimeMs
        case parseOnly WP.wasmP content of
          Left _err -> putStrLn "Failed to parse"
          Right m  ->
            forM_ [True, False] $ \useLiveness ->
              do
                if useLiveness then
                  putStrLn "Using liveness!"
                else
                  putStrLn "Not using liveness!"

                tParseE <- getTimeMs
                putStrLn (ß"Parsed in " . shows (tParseE - tParseS) . ß"ms" $ "")

                let fs = mFuncsDefined m
                putStrLn (ß"#Functions: " . shows (Vector.length fs) $ "")

                putStrLn "Building graphs"

                tGraphS <- getTimeMs
                d <- fmap head $ evaluate $ force =<< buildFuncDescs m
                tGraphE <- getTimeMs
    
                putStrLn $ ß"Done building graphs (" . shows (tGraphE - tGraphS) . ß"ms)" $ ""

                let sfc = H.fgSfc d
                let a = H.fgFunc d
                gA@(rootA,graphA,_) <- P.buildGraph sfc (Ast.fBody a)

                let cfg =
                      Cfg.init
                        (map Ast.globalType $ Ast.globals m)
                        (Vector.toList $ Ast.fParams $ H.fgFunc d)
                        (Vector.toList $ Ast.fLocals $ H.fgFunc d)
                        (H.mem0 m)

                let timeExpandMs = 60 * 1000 -- 30 * 60 * 1000
                    timeSynthesize = 120 * 1000
                    timeVerify = 10 * 1000
                    bound = 300000
                let rt = Ast.ftResults (Ast.fType $ H.fgFunc d)
                mBoundedTree <- liftIO $ runZ3SolverTimeout (drive (fgSfc d) rt bound cfg (fgGraph d)) timeExpandMs

                -- Make sure the tree is fully expanded first, so the used
                -- instructions can be used by the synthesizer.
                case mBoundedTree of
                  Nothing -> putStrLn "Tree invalid"
                  Just tree ->
                    do
                      -- putStrLn $ intercalateShow (ß"\n") (map ß $ P.showTreeLines $ P.mapTree (const ()) tree) ""

                      -- Use a simple liveness description of the final state. All globals and
                      -- stack values are live. Locals are all dead.
                      let l =
                            if useLiveness then
                              L.terminalState sfc (Ast.fResults $ H.fgFunc d)
                            else
                              L.terminalStateUseless sfc (Ast.fResults $ H.fgFunc d)

                      timeS <- getTimeMs
                      (mRes, stats) <- SU.runSuperUtil
                                (runMaybeT $ SU.softTimeout timeSynthesize $ LinearSynth.synthesize timeVerify sfc cfg tree l)
                                timeSynthesize Nothing True
                                
                      timeE <- getTimeMs

                      putStrLn (showString "Time taken: ". shows (timeE - timeS) $ "")
                      putStrLn $ showString "Z3 invocations: "  . shows (SU.ssZ3Verified stats) $ ""

                      case mRes of
                        Just (t, res) ->
                          do
                            let oldFunc = H.fgFunc d
                            let newFunc = oldFunc { Ast.fBody = map LinearSynth.ncfToInstr res }
                            putStrLn (showString "Sequence synthesized: " . shows res $ "")
                            mRes2 <- runMaybeT $ checkFuncEquivalence m oldFunc newFunc
                            case mRes2 of
                              Nothing -> putStrLn "Failed to verify correctness again (weird)"
                              Just (t, False) -> putStrLn "INVALID! ERROR" -- cannot happen
                              Just (t, True) ->
                                putStrLn $ showString "Verified afterward in " . shows t . showString "ms" $ ""
                        _ -> putStrLn "Failed to synthesise an alternative sequence"
