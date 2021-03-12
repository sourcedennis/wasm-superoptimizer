module Lang.Wasm.Ast
  ( -- * Indices
    TypeIdx (..)
  , FuncIdx (..)
  , TableIdx (..)
  , MemIdx (..)
  , GlobalIdx (..)
  , LocalIdx (..)
  , LabelIdx (..)
    -- * Instructions
  , Instr (..)
  , Sat (..)
  , Sx (..)
  , IUnop (..)
  , IBinop (..)
  , FUnop (..)
  , FBinop (..)
  , ITestop (..)
  , IRelop (..)
  , FRelop (..)
  , Cvtop (..)
  , PrmInstr (..)
  , VarInstr (..)
  , MemInstr (..)
  , SimpleInstr (..)
  , MemArg (..)
  , BlockType (..)
  , Expr (..)
    -- * Module & Sections
  , Module (..)
  , Func (..)
  , Table (..)
  , Mem (..)
  , Global (..)
  , Export (..)
  , ExportDesc (..)
  , DefinedFunc (..)
  , DefinedTable (..)
  , DefinedMem (..)
  , DefinedGlobal (..)
  , Import (..)
  , ImportInfo (..)
  , ImportedFunc (..)
  , ImportedTable (..)
  , ImportedMem (..)
  , ImportedGlobal (..)
  -- * Types
  , ValType (..)
  , TI32
  , TI64
  , TF32
  , TF64
  , ResultType (..)
  , FuncType (..)
  , Limits (..)
  -- , MemType (..)
  , TableType (..)
  , ElemType (..)
  , GlobalType (..)
  , Mut (..)
  -- , ExternType (..)
    -- * Values
  , WName
  , WI32 (..)
  , WI64 (..)
  , WF32 (..)
  , WF64 (..)
  , PVal (..)
  , Val
  , _VI32
  , _VI64
  , _VF32
  , _VF64
    -- * Helpers
  , InstrCtx (..)
  , TypeStack
    -- * Useful functions
  , instrEffect
  , applyEffect
  , applyEffectFull
  , applyFuncType
  , combineEffects
  , simpleInstrEffect
  , eqValType
  , newFuncType
  , zipValM
  , applyValM
  , itxLocal
  , itxGlobal
  , itxFuncType
  , valType
    -- ** Values
  , wi32s
  , wi32u
  , wi64s
  , wi64u
    -- ** Wasm Functions
  , funcs
  , lookupFunc
  , funcType
  , fParams
  -- , fParamsAndLocals
  , fResults
  , lookupLocal
  , funcCtx
    -- ** Memories
  , mems
  , lookupMem
  , memLimits
  , memDatas
    -- ** Tables
  , tables
  , tableElems
    -- ** Globals
  , globals
  , lookupGlobal
  , globalType
    -- ** Imports
  , imports
  , importInfo
  , importFunc
  , importTable
  , importMem
  , importGlobal
  ) where

import Melude
-- Stdlib imports
import           Control.Monad.Fail ( MonadFail )
-- External library imports
import qualified Data.ByteString as BS
import           Data.ByteString ( ByteString )
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
import           Data.List.NonEmpty ( NonEmpty ((:|)) )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- Local imports
import Lang.Wasm.Ast.Indices
import Lang.Wasm.Ast.Instrs
import Lang.Wasm.Ast.Sections
import Lang.Wasm.Ast.Types
import Lang.Wasm.Ast.Values
import Lang.Wasm.Ast.Effect
import Lang.Wasm.Data ( valType )


-- # Useful functions #

eqValType :: PVal a b c d -> PVal e f g h -> Bool
eqValType a b = valType a == valType b

newFuncType :: [ValType] -> [ValType] -> FuncType
newFuncType xs ys = FuncType (Vector.fromList xs) (Vector.fromList ys)


-- # Functions #

funcs :: Module -> [Func]
funcs m =
  let xs = map FuncImported $ Vector.toList $ mFuncsImported m
      ys = map FuncDefined $ Vector.toList $ mFuncsDefined m
  in xs ++ ys

lookupFunc :: FuncIdx -> Module -> Maybe Func
lookupFunc (FuncIdx i) m =
  let numImported = Vector.length $ mFuncsImported m
  in
  if i < numImported then
    FuncImported <$> ( mFuncsImported m !? i )
  else
    FuncDefined <$> ( mFuncsDefined m !? ( i - numImported ) )

funcType :: Func -> FuncType
funcType (FuncImported (ImportedFunc _ t)) = t
funcType (FuncDefined d) = fType d

fParams :: DefinedFunc -> Vector ValType
fParams = ftParams . fType

fResults :: DefinedFunc -> Vector ValType
fResults = ftResults . fType

-- fParamsAndLocals :: DefinedFunc -> Vector ValType
-- fParamsAndLocals f = (Vector.++) (fParams f) (fLocals f)

lookupLocal :: LocalIdx -> DefinedFunc -> Maybe ValType
lookupLocal (LocalIdx i) f =
  let numParams = length $ ftParams $ fType f
  in
  if i < numParams then
    ftParams (fType f) !? i
  else
    fLocals f !? (i - numParams) 

funcCtx :: Module -> DefinedFunc -> InstrCtx
funcCtx m f =
  let gs  = Vector.fromList $ map globalType $ globals m
      fts = Vector.fromList $ map funcType $ funcs m
  in InstrCtx gs (fParams f) (fLocals f) fts


-- # Memories #

mems :: Module -> [Mem]
mems m =
  let xs = map MemImported $ Vector.toList $ mMemsImported m
      ys = map MemDefined $ Vector.toList $ mMemsDefined m
  in xs ++ ys

lookupMem :: MemIdx -> Module -> Maybe Mem
lookupMem (MemIdx i) m =
  let numImported = Vector.length $ mMemsImported m
  in
  if i < numImported then
    MemImported <$> ( mMemsImported m !? i )
  else
    MemDefined <$> ( mMemsDefined m !? ( i - numImported ) )

memLimits :: Mem -> Limits
memLimits (MemImported (ImportedMem _ ls)) = dmLimits ls
memLimits (MemDefined dm) = dmLimits dm

memDatas :: Mem -> [(Expr,ByteString)]
memDatas (MemImported (ImportedMem _ m)) = dmDatas m
memDatas (MemDefined m) = dmDatas m


-- # Tables #

tables :: Module -> [Table]
tables m =
  let xs = map TableImported $ Vector.toList $ mTablesImported m
      ys = map TableDefined $ Vector.toList $ mTablesDefined m
  in xs ++ ys

tableElems :: Table -> [(Expr,[FuncIdx])]
tableElems (TableImported (ImportedTable _ t)) = definedTableElems t
tableElems (TableDefined t) = definedTableElems t

definedTableElems :: DefinedTable -> [(Expr,[FuncIdx])]
definedTableElems = tElems


-- # Globals #

globals :: Module -> [Global]
globals m =
  let xs = map GlobalImported $ Vector.toList $ mGlobalsImported m
      ys = map GlobalDefined $ Vector.toList $ mGlobalsDefined m
  in xs ++ ys

lookupGlobal :: GlobalIdx -> Module -> Maybe Global
lookupGlobal (GlobalIdx i) m =
  let numImported = Vector.length $ mGlobalsImported m
  in
  if i < numImported then
    GlobalImported <$> ( mGlobalsImported m !? i )
  else
    GlobalDefined <$> ( mGlobalsDefined m !? ( i - numImported ) )

globalType :: Global -> GlobalType
globalType (GlobalImported (ImportedGlobal i t)) = t
globalType (GlobalDefined g) = gType g


-- # Imports #

imports :: Module -> [Import]
imports m =
  map ImportDescFunc (Vector.toList (mFuncsImported m))
  ++ map ImportDescMem (Vector.toList (mMemsImported m))
  ++ map ImportDescTable (Vector.toList (mTablesImported m))
  ++ map ImportDescGlobal (Vector.toList (mGlobalsImported m))

importInfo :: Import -> ImportInfo
importInfo (ImportDescFunc (ImportedFunc info _))     = info
importInfo (ImportDescMem (ImportedMem info _))       = info
importInfo (ImportDescTable (ImportedTable info _))   = info
importInfo (ImportDescGlobal (ImportedGlobal info _)) = info

importFunc :: Import -> Maybe ImportedFunc
importFunc (ImportDescFunc f) = Just f
importFunc _ = Nothing

importTable :: Import -> Maybe ImportedTable
importTable (ImportDescTable t) = Just t
importTable _ = Nothing

importMem :: Import -> Maybe ImportedMem
importMem (ImportDescMem m) = Just m
importMem _ = Nothing

importGlobal :: Import -> Maybe ImportedGlobal
importGlobal (ImportDescGlobal g) = Just g
importGlobal _ = Nothing


-- # Helpers #

groupAdjacent :: IntMap a -> [(Int, [a])]
groupAdjacent =
  map combineGroup . groupSliding (\(i,_) (j,_) -> i + 1 == j) . IntMap.assocs
  where
  combineGroup :: NonEmpty (Int, a) -> (Int, [a])
  combineGroup ((i,x) :| xs) = (i, x : map snd xs)

i32Expr :: Integral i => i -> Expr
i32Expr i = [InstrSimple $ SConstI32 (WI32 $ fromIntegral i)]
