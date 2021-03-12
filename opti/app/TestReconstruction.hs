{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

-- | Tests whether ASTS and files can be re-obtained from the processs graphs.
module TestReconstruction
  ( testReconstruction
  ) where

import Melude
-- Stdlib imports
import           Control.Monad ( forM_ )
import           Control.Exception ( evaluate )
import           Control.DeepSeq ( force, NFData )
import           System.Timeout ( timeout )
-- Extra stdlib imports
import qualified Data.ByteString.Builder as LBS
import qualified Data.ByteString.Lazy as LBS
import qualified Lang.Wasm.BinaryParser as WP
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
-- External library imports
import           Data.Attoparsec.ByteString ( parseOnly )
-- Local library imports
import qualified Data.ByteString as BS
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast ( Module (..) )
import qualified Lang.Wasm.Process as P
import qualified Lang.Wasm.BinaryUnparser as WUP
import           Helpers ( FuncDesc (..), buildFuncDescs, toAst )

testReconstruction :: IO ()
testReconstruction =
  do
    putStrLn "------------------------------------"
    putStrLn "- - - - GRAPH RECONSTRUCTION - - - -"
    putStrLn "------------------------------------"
    -- forM_ ["micro_popcount","sha256", "lua","z3w"] $ \name ->
    forM_ ["micro_popcount","sha256", "lua"] $ \name ->
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

              let fs = Vector.toList $ mFuncsDefined m
                  numImportedFuncs = Vector.length $ mFuncsImported m
              putStrLn (ß"#Functions: " . shows (length fs) $ "")

              putStrLn "Building graphs"

              tGraphS <- getTimeMs
              funcDescs <- evaluate $ force =<< buildFuncDescs m
              tGraphE <- getTimeMs
  
              putStrLn $ ß"Done building graphs (" . shows (tGraphE - tGraphS) . ß"ms)" $ ""

              -- print $ map (Vector.length . fgGraph) funcDescs

              tAstS <- getTimeMs
              definedFuncs <- evaluate $ force $ Vector.fromList =<< mapM toAst funcDescs
              tAstE <- getTimeMs
              
              putStrLn $ ß"Done reconstructing AST: " . shows (tAstE - tAstS) . ß"ms" $ ""
              
              tParseS <- getTimeMs
              b <- evaluate $ force $ LBS.toLazyByteString (WUP.wasmW $ m { mFuncsDefined = definedFuncs })
              tParseE <- getTimeMs

              putStrLn (ß"Done unparsing: " . shows (tParseE - tParseS) . ß"ms" $ "")
              LBS.writeFile ("../data/macro/" ++ name ++ ".out.wasm") b

        putStrLn ""
