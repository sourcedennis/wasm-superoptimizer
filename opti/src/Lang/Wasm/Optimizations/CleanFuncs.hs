
-- | Removes functions that are never called from the module.
--
-- Occasionally, this really saves on superoptimization time, as unused
-- functions need not be superoptimized.
module Lang.Wasm.Optimizations.CleanFuncs
  ( removeUnusedFuncs
  ) where

import Melude
-- Stdlib imports
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
import qualified Data.Vector as Vector
import           Data.Vector ( Vector )
-- Local library imports
import Lang.Wasm.Ast

removeUnusedFuncs :: MonadFail m => Module -> m Module
removeUnusedFuncs m =
  do
    imps <- importantFuncs m
    let (t, impFuncs)  = foldl (keepImportantImported imps) (IntMap.empty, IdList.empty) $ zip [0..] (Vector.toList $ mFuncsImported m)
        numImported = length impFuncs
        (t', defFuncs) = foldl (keepImportantDefined imps numImported) (t, IdList.empty) $ zip [length (mFuncsImported m)..] (Vector.toList $ mFuncsDefined m)
        fIdxMap = \(FuncIdx i) -> failMaybeMsg "Missing function" (FuncIdx <$> IntMap.lookup i t')
    let funcsImported = IdList.toVector impFuncs
    -- trace (show $ initialImportant m) $ return ()
    -- trace (show imps) $ return ()
    -- trace (show impFuncs) $ return ()
    -- trace (show defFuncs) $ return ()
    -- trace (show t') $ return ()
    funcsDefined <- IdList.toVector <$> mapM (mapIdxDefFunc fIdxMap) defFuncs
    memsImported <- mapM (mapIdxImpMem fIdxMap) (mMemsImported m)
    memsDefined <- mapM (mapIdxDefMem fIdxMap) (mMemsDefined m)
    tablesImported <- mapM (mapIdxImpTable fIdxMap) (mTablesImported m)
    tablesDefined <- mapM (mapIdxDefTable fIdxMap) (mTablesDefined m)
    globalsDefined <- mapM (mapIdxDefGlobal fIdxMap) (mGlobalsDefined m)
    exports <- mapM (mapIdxExport fIdxMap) (mExports m)
    start <- mapIdxStart fIdxMap (mStart m)
    return $
      m {
        mFuncsImported = funcsImported
      , mFuncsDefined = funcsDefined
      , mMemsImported = memsImported
      , mMemsDefined  = memsDefined
      , mTablesImported = tablesImported
      , mTablesDefined = tablesDefined
      , mGlobalsDefined = globalsDefined
      , mExports = exports
      , mStart = start
      }
  where
  keepImportantImported :: IntSet -> (IntMap Int, IdList ImportedFunc) -> (Int, ImportedFunc) -> (IntMap Int, IdList ImportedFunc)
  keepImportantImported imps (m, r) (i,f)
    | i `IntSet.member` imps  =
        let (newI, r') = IdList.append f r
        in (IntMap.insert i newI m, r')
    | otherwise = (m, r)
  keepImportantDefined :: IntSet -> Int -> (IntMap Int, IdList DefinedFunc) -> (Int, DefinedFunc) -> (IntMap Int, IdList DefinedFunc)
  keepImportantDefined imps numImported (m, r) (i, f)
    | i `IntSet.member` imps  =
        let (newI, r') = IdList.append f r
        in (IntMap.insert i (numImported+newI) m, r')
    | otherwise = (m, r)

mapIdxDefFunc :: Monad m => ( FuncIdx -> m FuncIdx ) -> DefinedFunc -> m DefinedFunc
mapIdxDefFunc f (DefinedFunc ft l xs) = DefinedFunc ft l <$> mapM (mapIdxInstr f) xs

mapIdxDefMem :: Monad m => ( FuncIdx -> m FuncIdx ) -> DefinedMem -> m DefinedMem
mapIdxDefMem f (DefinedMem l xs) = DefinedMem l <$> mapM (mapFstM (mapIdxExpr f)) xs

mapIdxImpMem :: Monad m => ( FuncIdx -> m FuncIdx ) -> ImportedMem -> m ImportedMem
mapIdxImpMem f (ImportedMem info m) = ImportedMem info <$> mapIdxDefMem f m

mapIdxDefTable :: Monad m => ( FuncIdx -> m FuncIdx ) -> DefinedTable -> m DefinedTable
mapIdxDefTable f (DefinedTable ls xs) = DefinedTable ls <$> mapM (mapFstM (mapIdxExpr f)) xs

mapIdxImpTable :: Monad m => ( FuncIdx -> m FuncIdx ) -> ImportedTable -> m ImportedTable
mapIdxImpTable f (ImportedTable info t) = ImportedTable info <$> mapIdxDefTable f t

mapIdxDefGlobal :: Monad m => ( FuncIdx -> m FuncIdx ) -> DefinedGlobal -> m DefinedGlobal
mapIdxDefGlobal f (DefinedGlobal gt x) = DefinedGlobal gt <$> mapIdxExpr f x

mapIdxExport :: Monad m => ( FuncIdx -> m FuncIdx ) -> Export -> m Export
mapIdxExport f (Export name (ExportDescFunc idx)) = Export name <$> (ExportDescFunc <$> f idx)
mapIdxExport f e = return e

mapIdxStart :: Monad m => ( FuncIdx -> m FuncIdx ) -> Maybe FuncIdx -> m (Maybe FuncIdx)
mapIdxStart f (Just idx) = Just <$> f idx
mapIdxStart f Nothing    = return Nothing

mapIdxExpr :: Monad m => ( FuncIdx -> m FuncIdx ) -> Expr -> m Expr
mapIdxExpr = mapM . mapIdxInstr

mapIdxInstr :: Monad m => ( FuncIdx -> m FuncIdx ) -> Instr -> m Instr
mapIdxInstr f (InstrCall i) = InstrCall <$> f i
mapIdxInstr f (InstrBlock ft xs) = InstrBlock ft <$> mapIdxExpr f xs
mapIdxInstr f (InstrLoop ft xs)  = InstrLoop ft <$> mapIdxExpr f xs
mapIdxInstr f (InstrIf ft xs ys) = InstrIf ft <$> mapIdxExpr f xs <*> mapIdxExpr f ys
mapIdxInstr _ x = return x

importantFuncs :: MonadFail m => Module -> m IntSet
importantFuncs m =
  let ip = initialImportant m
  in foldM (flip visit) IntSet.empty (IntSet.toList ip)
  where
  visit :: MonadFail m => Int -> IntSet -> m IntSet
  visit i s
    | i `IntSet.member` s  =
        return s -- previously visited
    | otherwise  =
        case lookupFunc (FuncIdx i) m of
          Nothing -> fail "Missing function"
          Just (FuncImported _) ->
            return $ IntSet.insert i s
          Just (FuncDefined d)  ->
            foldM (flip visit) (IntSet.insert i s) (IntSet.toList $ calledFuncs d)

calledFuncs :: DefinedFunc -> IntSet
calledFuncs = foldr (IntSet.union . callFuncInstrs) IntSet.empty . fBody
  where
  callFuncInstrs :: Instr -> IntSet
  callFuncInstrs (InstrCall i) = IntSet.singleton $ unFuncIdx i
  callFuncInstrs (InstrBlock _ xs) = foldr (IntSet.union . callFuncInstrs) IntSet.empty xs
  callFuncInstrs (InstrLoop _ xs)  = foldr (IntSet.union . callFuncInstrs) IntSet.empty xs
  callFuncInstrs (InstrIf _ xs ys) = foldr (IntSet.union . callFuncInstrs) IntSet.empty (xs ++ ys)
  callFuncInstrs _ = IntSet.empty

-- |
--
-- Initially, only the exported functions, the start function, and the table
-- functions are important. All functions those recursively depend on are
-- also important.
initialImportant :: Module -> IntSet
initialImportant m =
  foldr IntSet.union IntSet.empty
    (startFunc (mStart m) : map exportedFuncs (Vector.toList $ mExports m) ++ map tableFuncs (tables m))
  where
  defTableFuncs :: DefinedTable -> IntSet
  defTableFuncs = IntSet.fromList . concatMap (map unFuncIdx . snd) . tElems
  impTableFuncs :: ImportedTable -> IntSet
  impTableFuncs (ImportedTable _ x) = defTableFuncs x
  tableFuncs :: Table -> IntSet
  tableFuncs (TableDefined x)  = defTableFuncs x
  tableFuncs (TableImported x) = impTableFuncs x

  exportedFuncs :: Export -> IntSet
  exportedFuncs (Export _ (ExportDescFunc i)) = IntSet.singleton $ unFuncIdx i
  exportedFuncs _ = IntSet.empty

  startFunc :: Maybe FuncIdx -> IntSet
  startFunc (Just i) = IntSet.singleton $ unFuncIdx i
  startFunc Nothing  = IntSet.empty
