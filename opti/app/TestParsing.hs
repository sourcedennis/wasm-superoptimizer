{-# LANGUAGE MultiParamTypeClasses #-}


module TestParsing
  ( testParsing
  ) where

import Melude
-- Extra stdlib imports
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Control.Monad ( forM_ )
import           Control.Exception ( evaluate )
-- External library imports
import qualified Data.ByteString.Builder as LBS
import           Data.Attoparsec.ByteString ( parseOnly )
-- Local library imports
import qualified Lang.Wasm.BinaryParser as WP
import qualified Lang.Wasm.BinaryUnparser as WUP


testParsing :: IO ()
testParsing =
  forM_ ["sha256","lua","z3w"] $ \name ->
    do
      content <- BS.readFile ("../data/macro/" ++ name ++ ".wasm")
      startTime <- getTimeMs
      case parseOnly WP.wasmP content of
        Left _err -> putStrLn (showString "Failed to parse: " . showString name $ "")
        Right m  ->
          do
            midTime <- getTimeMs
            let parseTime = midTime - startTime
            evaluate $ show m
            putStrLn (showString "Parsed       : " . showString name . showString " (" . shows parseTime . showString "ms)" $ "")

            let b = LBS.toLazyByteString (WUP.wasmW m)
            evaluate $ LBS.length b
            midTime4 <- getTimeMs
            let unparseTime = midTime4 - midTime
            putStrLn (showString "Unparsed     : " . showString name . showString " (" . shows unparseTime . showString "ms)" $ "")
            LBS.writeFile ("../data/macro/" ++ name ++ ".out.wasm") b
