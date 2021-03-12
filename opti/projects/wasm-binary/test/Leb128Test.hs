module Leb128Test (tests) where

-- Stdlib imports
import Prelude hiding (sequence, error, repeat)
import Data.Word (Word8)
-- External imports
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (assertEqual, assertBool, testCase)
-- Project imports
import Data.Format.Leb128 (leb128P, sleb128P, toLeb128, toSleb128)


fromSleb128 :: [Word8] -> Maybe Integer
fromSleb128 w =
  case P.parseOnly (sleb128P 32) (BS.pack w) of
    Left _   -> Nothing
    Right v  -> Just v

fromLeb128 :: [Word8] -> Maybe Integer
fromLeb128 w =
  case P.parseOnly (leb128P 32) (BS.pack w) of
    Left _   -> Nothing
    Right v  -> Just v

tests :: TestTree
tests =
  testGroup "Unit Tests"
    [ testGroup "toSleb128"
        [ testCase "-0x3F" $ assertEqual [] [0x41] (toSleb128 (-0x3F))
        , testCase " 0x3F" $ assertEqual [] [0x3F] (toSleb128 0x3F)
        , testCase "-0x40" $ assertEqual [] [0x40] (toSleb128 (-0x40))
        , testCase " 0x40" $ assertEqual [] [0xC0,0x00] (toSleb128 0x40)

        , testCase "-0x41" $ assertEqual [] [0xBF,0x7F] (toSleb128 (-0x41))
        , testCase " 0x41" $ assertEqual [] [0xC1,0x00] (toSleb128 0x41)

        , testCase " 0x00" $ assertEqual [] [0x00] (toSleb128 0x00)
        , testCase "-0x01" $ assertEqual [] [0x7F] (toSleb128 (-0x01))
        , testCase " 0x01" $ assertEqual [] [0x01] (toSleb128 0x01)
        , testCase "-0x64" $ assertEqual [] [0x9C,0x7F] (toSleb128 (-0x64))
        , testCase "-0x60" $ assertEqual [] [0xA0,0x7F] (toSleb128 (-0x60))
        , testCase " 0x3FFF" $ assertEqual [] [0xFF,0xFF,0x00] (toSleb128 0x3FFF)
        , testCase "-0x3FFF" $ assertEqual [] [0x81,0x80,0x7F] (toSleb128 (-0x3FFF))
        ]
    , testGroup "fromSleb128"
        [ testCase "-0x3F" $ assertEqual [] (Just (-0x3F)) (fromSleb128 $ toSleb128 (-0x3F))
        , testCase " 0x3F" $ assertEqual [] (Just 0x3F) (fromSleb128 $ toSleb128 0x3F)
        , testCase "-0x40" $ assertEqual [] (Just (-0x40)) (fromSleb128 $ toSleb128 (-0x40))
        , testCase " 0x40" $ assertEqual [] (Just 0x40) (fromSleb128 $ toSleb128 0x40)
        , testCase " 0x00" $ assertEqual [] (Just 0x00) (fromSleb128 $ toSleb128 0x00)
        , testCase "-0x01" $ assertEqual [] (Just (-0x01)) (fromSleb128 $ toSleb128 (-0x01))
        , testCase " 0x01" $ assertEqual [] (Just 0x01) (fromSleb128 $ toSleb128 0x01)
        , testCase " 0x3FFF" $ assertEqual [] (Just 0x3FFF) (fromSleb128 $ toSleb128 0x3FFF)
        , testCase "-0x3FFF" $ assertEqual [] (Just (-0x3FFF)) (fromSleb128 $ toSleb128 (-0x3FFF))
        ]
    , testGroup "toLeb128"
        [ testCase " 0x3F" $ assertEqual [] [0x3F] (toLeb128 0x3F)
        , testCase " 0x40" $ assertEqual [] [0x40] (toLeb128 0x40)
        , testCase " 0x7F" $ assertEqual [] [0x7F] (toLeb128 0x7F)
        , testCase " 0x80" $ assertEqual [] [0x80,0x01] (toLeb128 0x80)
        , testCase " 0x00" $ assertEqual [] [0x00] (toLeb128 0x00)
        , testCase " 0x01" $ assertEqual [] [0x01] (toLeb128 0x01)
        , testCase " 0x3FFF" $ assertEqual [] [0xFF,0x7F] (toLeb128 0x3FFF)
        , testCase " 0x4000" $ assertEqual [] [0x80,0x80,0x01] (toLeb128 0x4000)
        ]
    , testGroup "fromLeb128"
        [ testCase " 0x3F" $ assertEqual [] (Just 0x3F) (fromLeb128 $ toLeb128 0x3F)
        , testCase " 0x40" $ assertEqual [] (Just 0x40) (fromLeb128 $ toLeb128 0x40)
        , testCase " 0x7F" $ assertEqual [] (Just 0x7F) (fromLeb128 $ toLeb128 0x7F)
        , testCase " 0x80" $ assertEqual [] (Just 0x80) (fromLeb128 $ toLeb128 0x80)
        , testCase " 0x00" $ assertEqual [] (Just 0x00) (fromLeb128 $ toLeb128 0x00)
        , testCase " 0x01" $ assertEqual [] (Just 0x01) (fromLeb128 $ toLeb128 0x01)
        , testCase " 0x3FFF" $ assertEqual [] (Just 0x3FFF) (fromLeb128 $ toLeb128 0x3FFF)
        , testCase " 0x4000" $ assertEqual [] (Just 0x4000) (fromLeb128 $ toLeb128 0x4000)

        , testCase " 0x01/1" $ assertEqual [] (Just 0x01) (fromLeb128 [0x01])
        , testCase " 0x01/2" $ assertEqual [] (Just 0x01) (fromLeb128 [0x81,0x00])
        , testCase " 0x01/3" $ assertEqual [] (Just 0x01) (fromLeb128 [0x81,0x80,0x00])
        , testCase " 0x01/4" $ assertEqual [] (Just 0x01) (fromLeb128 [0x81,0x80,0x80,0x00])
        , testCase " 0x01/5" $ assertEqual [] (Just 0x01) (fromLeb128 [0x81,0x80,0x80,0x80,0x00])
        , testCase " 0x01/Malformed" $ assertEqual [] Nothing (fromLeb128 [0x81,0x80,0x80,0x80,0x80,0x00])
        ]
    ]
