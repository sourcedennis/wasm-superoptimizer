
-- | Performs the liveness analysis on the graphs, and reports the percentage of
-- dead variables in the programs.
module TestLiveness
  ( testLiveness
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
-- External library imports
import qualified Data.IdList as IdList
import           Data.Attoparsec.ByteString ( parseOnly )
-- Local library imports
import qualified Algorithm.Graph as AG
import qualified Lang.Wasm.Data as WD
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast
  ( PVal (..), Module (..) )
import qualified Lang.Wasm.BinaryParser as WP
-- Local imports
import Lang.Wasm.Process
import qualified Lang.Wasm.Dataflow.Liveness as L
import           Lang.Wasm.Dataflow.Liveness
  ( LivenessLattice (..), LivenessState (..) )
import qualified Algorithm.Dataflow as Dataflow
import           Helpers ( FuncDesc (..), buildFuncDescs )


testLiveness :: IO ()
testLiveness =
  do
    putStrLn "--------------------------------------"
    putStrLn "- - - - LIVENESS ANALYSIS TEST - - - -"
    putStrLn "--------------------------------------"
    -- forM_ ["micro_popcount","micro_gcd","sha256", "lua", "z3w"] $ \name ->
    forM_ [ "micro/idgcd", "micro/bubblesort4", "micro/popcount", "micro/babbage", "micro/transitive_break"
          , "slumps/bitwise_IO", "macro/lua_mini", "macro/sha256", "macro/raytracer"
          , "projects/wasm_lua/main", "projects/z3.wasm/z3w" ] $ \name ->
      do
        content <- BS.readFile ("../data/" ++ name ++ ".wasm")
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

              putStrLn "Building graphs"

              tGraphS <- getTimeMs
              funcDescs <- evaluate $ force =<< buildFuncDescs m
              tGraphE <- getTimeMs
  
              putStrLn $ ß"Done building graphs (" . shows (tGraphE - tGraphS) . ß"ms)" $ ""

              t3 <- getTimeMs

              funcLiveness <- mapM singleGraphBwdTypes funcDescs
              let mStats = foldr addStats emptyStats <$> mapM countLiveVars funcLiveness
              maybe (putStrLn "error") printStats mStats

              t4 <- getTimeMs

              putStrLn $ ß"Time: " . shows (t4-t3) . ß"ms" $ ""

              putStrLn $ ß"Liveness check done" ""
        putStrLn ""

type NumDead = Int
type NumLive = Int

-- | Some numbers on variable liveness in a program.
data LivenessStats =
  LivenessStats {
    statGlobals  :: (NumLive, NumDead)
  , statLocals   :: (NumLive, NumDead)
  , statStack    :: (NumLive, NumDead)
  }

printStats :: LivenessStats -> IO ()
printStats s =
  do
    let statTotal = statGlobals s `add2` statLocals s `add2` statStack s
    putStrLn $ ß"Globals: " . showsPctDead (statGlobals s) $ ""
    putStrLn $ ß"Locals:  " . showsPctDead (statLocals s)  $ ""
    putStrLn $ ß"Stack:   " . showsPctDead (statStack s)   $ ""
    putStrLn $ ß"Total:   " . showsPctDead statTotal       $ ""
  where
  showsPctDead :: (NumLive, NumDead) -> ShowS
  showsPctDead (0,0) = ß"N/A"
  showsPctDead (l,d) = ß(printf "%.3f" (fromIntegral d / fromIntegral ( l + d ) :: Double)) . ß" dead"

-- | Returns the number of live/dead variables
countLiveVars :: IntMap LivenessLattice -> Maybe LivenessStats
countLiveVars = foldM (fmap . addStats) emptyStats . map countLiveLattice . IntMap.elems
  where
  countLiveLattice :: LivenessLattice -> Maybe LivenessStats
  countLiveLattice LiveInvalid = Nothing
  countLiveLattice (LiveOk s)  = Just $ countLiveState s
  countLiveLattice LiveUnknown = Just $ LivenessStats (0, 0) (0, 0) (0, 0)
  
  countLiveState :: LivenessState -> LivenessStats
  countLiveState s =
    let a = s ^. L.lsActivation
        ls = IdList.toList (WD.aParams a) ++ IdList.toList (WD.aLocals a)
    in
    LivenessStats
      (foldr (add2 . countLiveVal . WD.gVal) (0,0) $ IdList.toList (s ^. L.lsGlobals))
      (foldr (add2 . countLiveVal) (0,0) ls)
      (foldr (add2 . countLiveVal) (0,0) $ concat (s ^. L.lsStack))

  countLiveVal :: L.LiveVal -> (NumLive, NumDead)
  countLiveVal v
    | val v == L.Live  = (1, 0)
    | otherwise        = (0, 1)

  val :: Ast.PVal a a a a -> a
  val (VI32 x) = x
  val (VI64 x) = x
  val (VF32 x) = x
  val (VF64 x) = x

emptyStats :: LivenessStats
emptyStats = LivenessStats (0, 0) (0, 0) (0, 0)

addStats :: LivenessStats -> LivenessStats -> LivenessStats
addStats a b =
  LivenessStats
    (add2 (statGlobals a) (statGlobals b))
    (add2 (statLocals a) (statLocals b))
    (add2 (statStack a) (statStack b))

add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (a, b) (c, d) = (a+c, b+d)

-- | Performs the type checking on a deterministic process graph.
singleGraphBwdTypes :: MonadFail m => FuncDesc -> m (IntMap LivenessLattice)
singleGraphBwdTypes d =
  do
    -- sfc rt g@(rootI,_,terminalI)
    let (rootI, _, terminalI) = fgGraph d
        rt = Ast.fResults $ fgFunc d
    flow <- graphFlow (fgSfc d) rt (fgGraph d)
    let graphInvFlow  = AG.invertFlow flow rootI
        graphTransfer = Dataflow.mapFlow (L.transfer $ fgSfc d) graphInvFlow
        terminalState = L.terminalState (fgSfc d) rt
        graphOrder = AG.quasiTopologicalOrder (IntSet.fromList . map snd . graphInvFlow) (IntSet.singleton terminalI)
  -- Definitely use the ordered fix-point algorithm. It takes about 1 minute for the Z3 program.
  -- The unordered algorithm takes about 20 minutes.
    return $ Dataflow.fixOrder L.confluence (initValBwd terminalI terminalState) graphTransfer (IntSet.singleton terminalI) graphOrder

type Visited = IntSet

initValBwd :: Int -> LivenessState -> ( Int -> LivenessLattice )
initValBwd terminalI s nodeI
  | terminalI == nodeI  = LiveOk s
  | otherwise           = LiveUnknown -- bottom
