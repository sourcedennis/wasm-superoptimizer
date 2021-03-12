module Lang.Wasm.BinaryUnparser.Sections where

import Melude
-- Stdlib imports
import qualified Data.List as L
-- External library imports
import qualified Data.ByteString as BS
import           Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as B
import           Data.ByteString.Builder ( Builder )
import qualified Data.HashSet as HashSet
import           Data.HashSet ( HashSet )
import qualified Data.IdHashSet as IdHashSet
import           Data.IdHashSet ( IdHashSet )
import qualified Data.Vector as Vector
import           Data.Vector ( Vector )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( Module (..), FuncType (..), TypeIdx (..), Table (..), Mem (..)
  , Global (..), Export (..), Func (..)
  , ValType (..), Expr, ExportDesc (..)
  , Import (..)
  , ImportedFunc (..), ImportedMem (..), ImportedTable (..), ImportedGlobal (..)
  , FuncIdx
  , DefinedFunc (..), DefinedTable (..), DefinedMem (..), DefinedGlobal (..)
  , Instr (..)
  , ResultType, TableIdx (..), MemIdx (..), ImportInfo (..), TableType (..)
  , ElemType (..)
  )
-- Local imports
import Lang.Wasm.BinaryUnparser.General
  ( vecW, vecBytesW, memIdxW, funcIdxW, tableIdxW, typeIdxW, globalIdxW, u32W
  , nameW
  )
import Lang.Wasm.BinaryUnparser.Instrs ( exprW, funcTypeIdxW )
import Lang.Wasm.BinaryUnparser.Types ( valTypeW, funcTypeW, globalTypeW, tableTypeW, limitsW )


modW :: Module -> Builder
modW m =
  let ts = allTypes $ Ast.funcs m
  in
  mconcat
    [ -- Magic
      B.byteString $ BS.pack [0x00, 0x61, 0x73, 0x6D]
    , -- Version
      B.byteString $ BS.pack [0x01, 0x00, 0x00, 0x00]
    , typeSecW (IdHashSet.elems ts)
    , importSecW ts (Ast.imports m)
    , funcSecW ts (map fType $ Vector.toList $ mFuncsDefined m)
    , tableSecW (Vector.toList $ mTablesDefined m)
    , memSecW (Vector.toList $ mMemsDefined m)
    , globalSecW (Vector.toList $ mGlobalsDefined m)
    , exportSecW (Vector.toList $ mExports m)
    , startSecW (mStart m)
    , elemSecW (Ast.tables m)
    , codeSecW ts (Vector.toList $ mFuncsDefined m)
    , dataSecW (Ast.mems m)
    ]

typeSecW :: [FuncType] -> Builder
typeSecW [] = mempty
typeSecW xs = sectionW 1 (vecW funcTypeW xs)

importSecW :: IdHashSet FuncType -> [Import] -> Builder
importSecW _   [] = mempty
importSecW fts xs = sectionW 2 (vecW importW xs)
  where
  importW :: Import -> Builder
  importW (ImportDescFunc (ImportedFunc info t))     =
    nameW (imModule info) <> nameW (imName info) <> B.word8 0x00 <> funcTypeIdxW fts t
  importW (ImportDescTable (ImportedTable info t))   =
    nameW (imModule info) <> nameW (imName info) <> B.word8 0x01 <> tableTypeW (TableType (tType t) FuncRef)
  importW (ImportDescMem (ImportedMem info t))       =
    nameW (imModule info) <> nameW (imName info) <> B.word8 0x02 <> limitsW (dmLimits t)
  importW (ImportDescGlobal (ImportedGlobal info t)) =
    nameW (imModule info) <> nameW (imName info) <> B.word8 0x03 <> globalTypeW t


-- # Imports #

funcSecW :: IdHashSet FuncType -> [FuncType] -> Builder
funcSecW fts [] = mempty
funcSecW fts xs = sectionW 3 (vecW id $ map (funcTypeIdxW fts) xs)

tableSecW :: [DefinedTable] -> Builder
tableSecW [] = mempty
tableSecW xs = sectionW 4 (vecW tableW xs)
  where
  tableW :: DefinedTable -> Builder
  tableW t = tableTypeW (TableType (tType t) FuncRef)

memSecW :: [DefinedMem] -> Builder
memSecW [] = mempty
memSecW xs = sectionW 5 (vecW memW xs)
  where
  memW :: DefinedMem -> Builder
  memW = limitsW . dmLimits

globalSecW :: [DefinedGlobal] -> Builder
globalSecW [] = mempty
globalSecW xs = sectionW 6 (vecW globalW xs)
  where
  globalW :: DefinedGlobal -> Builder
  globalW g = globalTypeW (gType g) <> exprW IdHashSet.empty (gInit g)

exportSecW :: [Export] -> Builder
exportSecW [] = mempty
exportSecW xs = sectionW 7 (vecW exportW xs)
  where
  exportW :: Export -> Builder
  exportW e = nameW (exName e) <> descW (exDesc e)
  descW :: ExportDesc -> Builder
  descW (ExportDescFunc fIdx)   = B.word8 0x00 <> funcIdxW fIdx
  descW (ExportDescTable tIdx)  = B.word8 0x01 <> tableIdxW tIdx
  descW (ExportDescMem mIdx)    = B.word8 0x02 <> memIdxW mIdx
  descW (ExportDescGlobal gIdx) = B.word8 0x03 <> globalIdxW gIdx

startSecW :: Maybe FuncIdx -> Builder
startSecW Nothing     = mempty
startSecW (Just fIdx) = sectionW 8 (funcIdxW fIdx)

elemSecW :: [Table] -> Builder
elemSecW [] = mempty
elemSecW xs =
  sectionW 9 $ vecW elemW $ concatMap flattenTable $ zip (map TableIdx [0..]) xs
  where
  flattenTable :: (TableIdx, Table) -> [(TableIdx, Expr, [FuncIdx])]
  flattenTable (idx, t) = [(idx,off,fIdxs) | (off,fIdxs) <- Ast.tableElems t]
  elemW :: (TableIdx,Expr,[FuncIdx]) -> Builder
  elemW (idx, eOff, eFuncs) =
    tableIdxW idx <> exprW IdHashSet.empty eOff <> vecW funcIdxW eFuncs

codeSecW :: IdHashSet FuncType -> [DefinedFunc] -> Builder
codeSecW fts [] = mempty
codeSecW fts xs = sectionW 10 (vecW codeW xs)
  where
  codeW :: DefinedFunc -> Builder
  codeW c =
    let (len, b) = subW (codeW' c)
    in u32W len <> b
  codeW' :: DefinedFunc -> Builder
  codeW' c =
    vecW localsW (groupVals $ Vector.toList $ fLocals c) <> exprW fts (fBody c)
  localsW :: (ValType, Integer) -> Builder
  localsW (vt, n) = u32W n <> valTypeW vt
  groupVals :: [ValType] -> [(ValType, Integer)]
  groupVals = map (\x -> (head x, toInteger (length x))) . L.group

dataSecW :: [Mem] -> Builder
dataSecW [] = mempty
dataSecW xs =
  sectionW 11 $ vecW dataW $ concatMap flattenMem $ zip (map MemIdx [0..]) xs
  where
  flattenMem :: (memIdx, Mem) -> [(memIdx, Expr, ByteString)]
  flattenMem (idx, m) = [(idx,off,xs) | (off,xs) <- Ast.memDatas m]
  dataW :: (MemIdx,Expr,ByteString) -> Builder
  dataW (idx, dOff, xs) =
    memIdxW idx <> exprW IdHashSet.empty dOff <> vecBytesW xs

sectionW :: Integer -> Builder -> Builder
sectionW n b =
  let (len, b') = subW b
  in
  B.word8 (fromInteger n) <> u32W len <> b'

-- | Returns the length of the built bytestring.
subW :: Builder -> (Integer, Builder)
subW b =
  let bs  = B.toLazyByteString b
      len = toInteger (LBS.length bs)
  in
  ( len, B.lazyByteString bs )



-- # Helpers #

allTypes :: [Func] -> IdHashSet FuncType
allTypes xs =
  let fts = HashSet.toList $ foldr (HashSet.union . allFuncTypes) HashSet.empty xs
  in snd $ IdHashSet.fromList $ L.sortBy cmpFuncType fts

cmpFuncType :: FuncType -> FuncType -> Ordering
cmpFuncType (FuncType xs ys) (FuncType zs ws) =
  cmpTuple cmpResultType cmpResultType (xs,ys) (zs,ws)
  where
  -- |
  -- >>> cmpResultType [] [TI32,TF32]
  cmpResultType :: ResultType -> ResultType -> Ordering
  cmpResultType xs ys = cmpLexicographical cmpValType (Vector.toList xs) (Vector.toList ys)
  cmpValType :: ValType -> ValType -> Ordering
  cmpValType = cmpIndex [TI32,TI64,TF32,TF64]

-- | Returns the type of the function and the function types stored within the
-- instructions of the function (e.g., indirect call).
allFuncTypes :: Func -> HashSet FuncType
allFuncTypes (FuncImported (ImportedFunc _ ft)) = HashSet.singleton ft
allFuncTypes (FuncDefined f) =
  foldr (HashSet.union . instrFuncType) (HashSet.singleton $ Ast.fType f) (fBody f)

instrFuncType :: Instr -> HashSet FuncType
instrFuncType (InstrBlock ft xs)      =
  if isPrimitiveFuncType ft then
    foldr (HashSet.union . instrFuncType) (HashSet.singleton ft) xs
  else
    foldr (HashSet.union . instrFuncType) HashSet.empty xs
instrFuncType (InstrLoop ft xs)       =
  if isPrimitiveFuncType ft then
    foldr (HashSet.union . instrFuncType) (HashSet.singleton ft) xs
  else
    foldr (HashSet.union . instrFuncType) HashSet.empty xs
instrFuncType (InstrIf ft xs ys)       =
  if isPrimitiveFuncType ft then
    foldr (HashSet.union . instrFuncType) (HashSet.singleton ft) (xs ++ ys)
  else
    foldr (HashSet.union . instrFuncType) HashSet.empty (xs ++ ys)
instrFuncType (InstrCallIndirect ft) = HashSet.singleton ft
instrFuncType _                      = HashSet.empty

isPrimitiveFuncType :: FuncType -> Bool
isPrimitiveFuncType (FuncType xs ys)
  = Vector.length xs == 0 && Vector.length ys <= 1
