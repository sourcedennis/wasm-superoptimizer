{-# LANGUAGE DeriveGeneric #-}

module Lang.Wasm.Process.Reconstruct.Instr where

-- Stdlib imports
import           GHC.Generics ( Generic )
import           Control.DeepSeq ( force, NFData )
-- Extra stdlib imports
import           Data.IntMap ( IntMap )
import qualified Control.Monad.State as S
import           Control.Monad.State ( State )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( FuncIdx, FuncType, LabelIdx (..), SimpleInstr, InstrCtx, ValType (..) )
-- Local imports
import           Lang.Wasm.Process.Structures ( NodeId )


-- data Instr
--   = InstrNop
--   | InstrUnreachable
--   | InstrSimple !SimpleInstr
--   | InstrBlock FuncType ![Instr]
--   | InstrLoop FuncType ![Instr]
--   | InstrIf FuncType ![Instr] ![Instr]
--   | InstrBr !(Either NodeId LabelIdx)
--   | InstrBrIf !(Either NodeId LabelIdx)
--   | InstrBrTable ![Either NodeId LabelIdx] !(Either NodeId LabelIdx)
--   | InstrReturn
--   | InstrCall !FuncIdx
--   | InstrCallIndirect !FuncType
--   | InstrDropKeep FuncType
--     -- | When the type is unknown (e.g., the result of an infinite loops can
--     -- have /any/ type).
--   | InstrIdkType
--   deriving (Eq, Show, Generic)

-- instance NFData Instr

-- -- | Replaces any occurrence of the node id by a label to the first surrounding
-- -- label. This obeys the "De Bruijn indexing" as demanded by WebAssembly.
-- --
-- -- >>> fixLabel 42 (InstrBlock [InstrBr (Left 42)])
-- -- InstrBlock [InstrBr (Right label1)]
-- fixLabel :: SingleId -> Instr -> Instr
-- fixLabel = fixLabel' 0
--   where
--   fixLabel' :: Int -> SingleId -> Instr -> Instr
--   fixLabel' i n InstrNop               = InstrNop
--   fixLabel' i n InstrUnreachable       = InstrUnreachable
--   fixLabel' i n (InstrSimple x)        = InstrSimple x
--   fixLabel' i n (InstrBlock ft xs)     = InstrBlock ft (map (fixLabel' (i+1) n) xs)
--   fixLabel' i n (InstrLoop ft xs)      = InstrLoop ft (map (fixLabel' (i+1) n) xs)
--   fixLabel' i n (InstrIf ft xs ys)     = InstrIf ft (map (fixLabel' (i+1) n) xs) (map (fixLabel' (i+1) n) ys)
--   fixLabel' i n (InstrBr x)            = InstrBr (updateLabel n (LabelIdx i) x)
--   fixLabel' i n (InstrBrIf x)          = InstrBrIf (updateLabel n (LabelIdx i) x)
--   fixLabel' i n (InstrBrTable xs x)    = InstrBrTable (map (updateLabel n (LabelIdx i)) xs) (updateLabel n (LabelIdx i) x)
--   fixLabel' i n InstrReturn            = InstrReturn
--   fixLabel' i n (InstrCall fIdx)       = InstrCall fIdx
--   fixLabel' i n (InstrCallIndirect ft) = InstrCallIndirect ft
--   fixLabel' i n InstrIdkType           = InstrIdkType

--   updateLabel :: SingleId -> LabelIdx -> Either SingleId LabelIdx -> Either SingleId LabelIdx
--   updateLabel i lIdx (Left j)
--     | i == j     = Right lIdx
--     | otherwise  = Left j
--   updateLabel _ _ (Right lIdx) = Right lIdx
