{-# LANGUAGE Strict #-}

module Lang.Wasm.BinaryParser.Sections
  ( modP
    -- Exports for testing purposes
  , customSecP
  , typeSecP
  , importSecP
  , funcSecP
  , tableSecP
  , memSecP
  , globalSecP
  , exportSecP
  , startSecP
  , elemSecP
  , codeSecP
  , dataSecP
  ) where

import Melude
-- Stdlib imports
import qualified Data.List as L
-- External library imports
import           Data.Word ( Word8, Word32 )
import qualified Data.ByteString as BS
import           Data.ByteString ( ByteString )
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!), (!?) )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( Module (..), Instr, FuncType, Import (..), TypeIdx (..)
  , Table (..), Global (..), Mem (..), Export (..), ExportDesc (..)
  , ValType (..), Expr (..), Func (..), Import (..), ImportInfo (..)
  , FuncIdx (..), TableIdx (..), MemIdx (..), DefinedGlobal (..)
  , DefinedMem (..), DefinedTable (..), DefinedFunc (..)
  , ImportedFunc (..), ImportedMem (..), ImportedTable (..), ImportedGlobal (..)
  , Instr (..), SimpleInstr (..), WI32 (..), InstrCtx (..)
  , GlobalIdx (..)
  )
-- Local imports
import Data.Format.Leb128 (leb128P, sleb128P)
import Lang.Wasm.BinaryParser.General
  ( u32P, u64P, i32P, i64P, typeIdxP, funcIdxP, tableIdxP, memIdxP, globalIdxP
  , localIdxP, labelIdxP, nameP, vecP, vecBytesP
  )
import Lang.Wasm.BinaryParser.Helpers (parseSub, catch)
import Lang.Wasm.BinaryParser.Types (funcTypeP, globalTypeP, limitsP, tableTypeP, valTypeP)
import Lang.Wasm.BinaryParser.Instrs ( funcTypeIdxP, exprP )
import Lang.Wasm.BinaryParser.InstrTypes ( fixModuleTypes )


modP :: Parser Module
modP =
  do
    -- Magic
    P.string (BS.pack [0x00, 0x61, 0x73, 0x6D])
    -- Version
    P.string (BS.pack [0x01, 0x00, 0x00, 0x00])
    P.many' customSecP -- ignore
    mTypes <- typeSecP
    let allTypes = Vector.fromList mTypes
    P.many' customSecP -- ignore
    mImports <- importSecP allTypes
    P.many' customSecP -- ignore
    funcTypeIdxs <- funcSecP
    P.many' customSecP -- ignore
    mTables <- tableSecP
    P.many' customSecP -- ignore
    mMems <- memSecP
    P.many' customSecP -- ignore
    mGlobals <- globalSecP
    P.many' customSecP -- ignore
    mExports <- exportSecP
    P.many' customSecP -- ignore
    mStart <- startSecP
    P.many' customSecP -- ignore
    mElems <- elemSecP
    P.many' customSecP -- ignore
    code <- codeSecP allTypes
    P.many' customSecP -- ignore
    mDatas <- dataSecP
    P.many' customSecP -- ignore
    P.endOfInput

    -- Combine function declarations with their code
    let mFuncs =
          if length funcTypeIdxs == length code then
            zipWith (combineDeclarationCode allTypes) funcTypeIdxs code
          else
            fail "Malformed function declarations"
        (importedMems, definedMems) =
          fillMems (mapMaybe Ast.importMem mImports, mMems) mDatas
        (importedTables, definedTables) =
          fillTables (mapMaybe Ast.importTable mImports, mTables) mElems
    fixModuleTypes $
      Module {
        mFuncsImported   = Vector.fromList $ mapMaybe Ast.importFunc mImports
      , mFuncsDefined    = Vector.fromList mFuncs
      , mMemsImported    = Vector.fromList importedMems
      , mMemsDefined     = Vector.fromList definedMems
      , mTablesImported  = Vector.fromList importedTables
      , mTablesDefined   = Vector.fromList definedTables
      , mGlobalsImported = Vector.fromList $ mapMaybe Ast.importGlobal mImports
      , mGlobalsDefined  = Vector.fromList mGlobals
      , mExports         = Vector.fromList mExports
      , mStart           = mStart
      }
  where
  -- | The function declaration section (3) and the function code section (10)
  -- are kept disjoint. However, their content is coupled in the AST. This
  -- function combines them again.
  combineDeclarationCode :: Vector FuncType -> TypeIdx -> Code -> DefinedFunc
  combineDeclarationCode fts (TypeIdx tIdx) c =
    DefinedFunc {
      fType   = fromMaybe (error $ "No type at TypeIdx " ++ show tIdx) (fts !? tIdx)
    , fLocals = Vector.fromList $ cLocals c
    , fBody   = cCode c
    }

data Data = Data MemIdx Expr ByteString

fillMems :: ([ImportedMem], [DefinedMem]) -> [Data] -> ([ImportedMem], [DefinedMem])
fillMems = L.foldr fillMem
  where
  fillMem :: Data -> ([ImportedMem], [DefinedMem]) -> ([ImportedMem], [DefinedMem])
  fillMem (Data (MemIdx i) off zs) (xs, ys)
    | i < length xs   = (mapAt (insertImported off zs) i xs, ys)
    | otherwise       = (xs, mapAt (insertDefined off zs) (i - length xs) ys)
  insertImported :: Expr -> ByteString -> ImportedMem -> ImportedMem
  insertImported e xs (ImportedMem info m) = ImportedMem info $ insertDefined e xs m
  insertDefined :: Expr -> ByteString -> DefinedMem -> DefinedMem
  insertDefined e xs m = m { dmDatas = (e,xs) : dmDatas m }

data Elem = Elem TableIdx Expr [FuncIdx]

fillTables :: ([ImportedTable], [DefinedTable]) -> [Elem] -> ([ImportedTable], [DefinedTable])
fillTables = L.foldr fillTable
  where
  fillTable :: Elem -> ([ImportedTable], [DefinedTable]) -> ([ImportedTable], [DefinedTable])
  fillTable (Elem (TableIdx i) off zs) (xs, ys)
    | i < length xs   = (mapAt (insertImported off zs) i xs, ys)
    | otherwise       = (xs, mapAt (insertDefined off zs) (i - length xs) ys)
  insertImported :: Expr -> [FuncIdx] -> ImportedTable -> ImportedTable
  insertImported e xs (ImportedTable info t) = ImportedTable info $ insertDefined e xs t
  insertDefined :: Expr -> [FuncIdx] -> DefinedTable -> DefinedTable
  insertDefined e xs m = m { tElems  = (e, xs) : tElems m }

customSecP :: Parser ()
customSecP =
  do
    res <- sectionP 0 P.takeByteString -- Consume all
    case res of
      Nothing -> fail "No custom section"
      Just _  -> return ()

-- | The type section decodes into a vector of function types that represent the
-- types component of a module.
typeSecP :: Parser [FuncType]
typeSecP = fromMaybe [] <$> sectionP 1 (vecP funcTypeP)

-- | The import section decodes into a vector of imports that represent the
-- `imports` component of a module
importSecP :: Vector FuncType -> Parser [Import]
importSecP fts = fromMaybe [] <$> sectionP 2 (vecP importP)
  where
  importP :: Parser Import
  importP =
    do
      info <- importInfoP
      P.choice
        [ P.word8 0x00 >> (ImportDescFunc   . ImportedFunc   info <$> funcTypeIdxP fts)
        , P.word8 0x01 >> (ImportDescTable  . ImportedTable  info <$> importTableP)
        , P.word8 0x02 >> (ImportDescMem    . ImportedMem    info <$> importMemP)
        , P.word8 0x03 >> (ImportDescGlobal . ImportedGlobal info <$> globalTypeP)
        ]
  importInfoP :: Parser ImportInfo
  importInfoP = ImportInfo <$> nameP <*> nameP
  importTableP :: Parser DefinedTable
  importTableP = DefinedTable <$> tableTypeP <*> pure []
  importMemP :: Parser DefinedMem
  importMemP = DefinedMem <$> limitsP <*> pure []

-- | The functions section decodes into a vector that represents the `type`
-- fields of the functions in the `funcs` component of a module. The `locals`
-- and `body` fields of the respective functions are encoded separately in the
-- code section
funcSecP :: Parser [TypeIdx]
funcSecP = fromMaybe [] <$> sectionP 3 (vecP typeIdxP)

-- | The table sections decodes into a vector of tables that represent the
-- tables component of a module.
tableSecP :: Parser [DefinedTable]
tableSecP = fromMaybe [] <$> sectionP 4 (vecP tableP)
  where
  tableP :: Parser DefinedTable
  tableP = DefinedTable <$> tableTypeP <*> pure []

-- | The memory section decodes into a vector of memories that represent the
-- `mems` component of a module.
memSecP :: Parser [DefinedMem]
memSecP = fromMaybe [] <$> sectionP 5 (vecP memP)
  where
  memP :: Parser DefinedMem
  memP =  DefinedMem <$> limitsP <*> pure []

-- | The global sections decodes into a vector of globals that represent the
-- `globals` component of a module
globalSecP :: Parser [DefinedGlobal]
globalSecP = fromMaybe [] <$> sectionP 6 (vecP globalP)
  where
  globalP :: Parser DefinedGlobal
  globalP = DefinedGlobal <$> globalTypeP <*> exprP Vector.empty

emptyCtx :: (TypeIdx -> Parser FuncType, Ast.InstrCtx)
emptyCtx =
  (\_ -> fail "Missing Type"
  , Ast.InstrCtx {
      Ast.itxGlobals   = Vector.empty
    , Ast.itxParams    = Vector.empty
    , Ast.itxLocals    = Vector.empty
    , Ast.itxFuncTypes = Vector.empty
    }
  )

-- |
exportSecP :: Parser [Export]
exportSecP = fromMaybe [] <$> sectionP 7 (vecP exportP)
  where
  exportP :: Parser Export
  exportP = Export <$> nameP <*> descP
  descP :: Parser ExportDesc
  descP =
    P.choice
      [ P.word8 0x00 >> (ExportDescFunc <$> funcIdxP)
      , P.word8 0x01 >> (ExportDescTable <$> tableIdxP)
      , P.word8 0x02 >> (ExportDescMem <$> memIdxP)
      , P.word8 0x03 >> (ExportDescGlobal <$> globalIdxP)
      ]

-- |
startSecP :: Parser (Maybe FuncIdx)
startSecP = sectionP 8 funcIdxP

-- |
elemSecP :: Parser [Elem]
elemSecP = fromMaybe [] <$> sectionP 9 (vecP elemP)
  where
  elemP :: Parser Elem
  elemP = Elem <$> tableIdxP <*> exprP Vector.empty <*> vecP funcIdxP

-- | Internal representation of a function's code block. This is later
-- eliminated and replaced by a `Func` structure.
data Code =
  Code {
    cLocals :: [ValType]
  , cCode   :: Expr
  }

-- |
codeSecP :: Vector FuncType -> Parser [Code]
codeSecP fts = fromMaybe [] <$> sectionP 10 (vecP codeP)
  where
  codeP :: Parser Code
  codeP =
    do
      size <- fromInteger <$> u32P
      parseSub size funcP
  funcP :: Parser Code
  funcP = Code <$> (concat <$> vecP localsP) <*> exprP fts
  localsP :: Parser [ValType]
  localsP = replicate <$> (fromInteger <$> u32P) <*> valTypeP

-- |
dataSecP :: Parser [Data]
dataSecP = fromMaybe [] <$> sectionP 11 (vecP dataP)
  where
  dataP :: Parser Data
  dataP =
    Data <$> memIdxP <*> exprP Vector.empty <*> vecBytesP

sectionP :: Int -> Parser a -> Parser (Maybe a)
sectionP n payloadParser =
  catch $
    do
      P.word8 (fromInteger $ toInteger n)
      size <- fromInteger <$> u32P
      parseSub size payloadParser
