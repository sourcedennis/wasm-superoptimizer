module Lang.Wasm.BinaryParser where

-- External library imports
import Data.Attoparsec.ByteString ( Parser )
-- Local library imports
import Lang.Wasm.Ast ( Module )
-- Local imports
import Lang.Wasm.BinaryParser.Sections ( modP )

wasmP :: Parser Module
wasmP = modP
