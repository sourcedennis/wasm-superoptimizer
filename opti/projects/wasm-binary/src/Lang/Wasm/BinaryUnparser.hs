module Lang.Wasm.BinaryUnparser where

-- External library imports
import qualified Data.ByteString.Builder as B
import           Data.ByteString.Builder ( Builder )
-- Local library imports
import           Lang.Wasm.Ast ( Module )
-- Local imports
import Lang.Wasm.BinaryUnparser.Sections ( modW )

wasmW :: Module -> Builder
wasmW = modW
