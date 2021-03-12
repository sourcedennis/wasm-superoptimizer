-- External imports
import Test.Tasty (defaultMain, TestTree, testGroup)
-- Local imports
import qualified Leb128Test

main :: IO ()
main = defaultMain $
  testGroup "Tests"
    [ Leb128Test.tests
    ]
