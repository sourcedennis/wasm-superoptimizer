{-# LANGUAGE Strict #-}

-- | The parser does not perform type checking. The types of some instructions
-- (e.g., `PDrop`) cannot be determined from the syntax. These are parsed as
-- their I32 version. The `fixTypes` function does perform type checking, and
-- replaces these parametric instructions by their correctly-typed variant.
module Lang.Wasm.BinaryParser.InstrTypes
  ( fixModuleTypes
  ) where

import Melude
-- Stdlib imports
import Control.Monad.Fail ( MonadFail )
-- Extra stdlib imports
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
-- Local library imports
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast hiding ( Stack )

fixModuleTypes :: MonadFail m => Module -> m Module
fixModuleTypes m =
  do
    let gs  = Vector.fromList $ map Ast.globalType $ Ast.globals m
        fts = Vector.fromList $ map Ast.funcType $ Ast.funcs m
        -- Simple context used for anything other than a function
        gCtx =
          InstrCtx {
            itxGlobals   = gs
          , itxParams    = Vector.empty
          , itxLocals    = Vector.empty
          , itxFuncTypes = fts
          }
    Module (mFuncsImported m)
      <$> (Vector.fromList <$> zipWithM (fixFunc gs fts) [length (Ast.mFuncsImported m)..] (Vector.toList $ mFuncsDefined m))
      <*> Vector.mapM (fixImpMem gCtx) (mMemsImported m)
      <*> Vector.mapM (fixDefMem gCtx) (mMemsDefined m)
      <*> Vector.mapM (fixImpTable gCtx) (mTablesImported m)
      <*> Vector.mapM (fixDefTable gCtx) (mTablesDefined m)
      <*> pure (mGlobalsImported m)
      <*> Vector.mapM (fixDefGlobal gCtx) (mGlobalsDefined m)
      <*> pure (mExports m)
      <*> pure (mStart m)

fixFunc :: MonadFail m
        => Vector GlobalType
        -> Vector FuncType
        -> Int
        -> DefinedFunc
        -> m DefinedFunc
fixFunc gs fts i f =
  do
    let ctx =
          InstrCtx {
            itxGlobals   = gs
          , itxParams    = Ast.fParams f
          , itxLocals    = Ast.fLocals f
          , itxFuncTypes = fts
          }
    body <- fixExpr ctx $ Ast.fBody f
    return $ f { Ast.fBody = body }

fixImpMem :: MonadFail m => InstrCtx -> ImportedMem -> m ImportedMem
fixImpMem ctx (ImportedMem info m) = ImportedMem info <$> fixDefMem ctx m

fixDefMem :: MonadFail m => InstrCtx -> DefinedMem -> m DefinedMem
fixDefMem ctx (DefinedMem l d) = DefinedMem l <$> mapM (mapFstM $ fixExpr ctx) d

fixImpTable :: MonadFail m => InstrCtx -> ImportedTable -> m ImportedTable
fixImpTable ctx (ImportedTable info t) = ImportedTable info <$> fixDefTable ctx t

fixDefTable :: MonadFail m => InstrCtx -> DefinedTable -> m DefinedTable
fixDefTable ctx (DefinedTable l d) = DefinedTable l <$> mapM (mapFstM $ fixExpr ctx) d

fixDefGlobal :: MonadFail m => InstrCtx -> DefinedGlobal -> m DefinedGlobal
fixDefGlobal ctx (DefinedGlobal gt expr) = DefinedGlobal gt <$> fixExpr ctx expr 

type Stack = [ValType]

fixExpr :: MonadFail m => InstrCtx -> [Instr] -> m [Instr]
fixExpr ctx = fixInstrs ctx []

-- 
fixInstrs :: MonadFail m => InstrCtx -> Stack -> [Instr] -> m [Instr]
fixInstrs _ _ [] = return []
fixInstrs ctx s (InstrNop:xs)        = fixInstrs ctx s xs
fixInstrs ctx s (InstrUnreachable:_) = return [InstrUnreachable]
fixInstrs ctx s (InstrSimple x:xs)   =
  do
    -- trace (showString "Fix simple " . shows x . showString " " . shows s $ "") $ return () 
    x' <- fixSimple s x
    ft <- simpleInstrEffect ctx x'
    s' <- applyEffect ft s
    (InstrSimple x':) <$> fixInstrs ctx s' xs
fixInstrs ctx s (InstrBlock bt@(FuncType bti _) ys:xs) =
  do
    -- trace (showString "Fix block " . shows bt . showString " " . shows s $ "") $ return ()
    s' <- applyFuncType bt s
    ys' <- fixInstrs ctx (reverse $ Vector.toList bti) ys
    (InstrBlock bt ys':) <$> fixInstrs ctx s' xs
fixInstrs ctx s (InstrLoop bt@(FuncType bti _) ys:xs) =
  do
    -- trace (showString "Fix loop " . shows bt . showString " " . shows s $ "") $ return ()
    s' <- applyFuncType bt s
    ys' <- fixInstrs ctx (reverse $ Vector.toList bti) ys
    (InstrLoop bt ys':) <$> fixInstrs ctx s' xs
fixInstrs ctx s (InstrIf bt@(FuncType bti _) ys zs:xs) =
  do
    -- trace (showString "Fix if " . shows bt . showString " " . shows s $ "") $ return ()
    s' <- applyFuncType bt =<< applyEffect ([TI32],[]) s
    ys' <- fixInstrs ctx (reverse $ Vector.toList bti) ys
    zs' <- fixInstrs ctx (reverse $ Vector.toList bti) zs
    (InstrIf bt ys' zs':) <$> fixInstrs ctx s' xs
fixInstrs ctx s (InstrBrIf lIdx:xs) =
  do
    -- trace (showString "Fix brif " . shows s $ "") $ return ()
    s' <- applyEffect ([TI32],[]) s
    (InstrBrIf lIdx:) <$> fixInstrs ctx s' xs
fixInstrs ctx s (InstrBrTable lIdx lIdxs:_) =
  do
    -- trace (showString "Fix brtable " . shows s $ "") $ return ()
    s' <- applyEffect ([TI32],[]) s
    return [InstrBrTable lIdx lIdxs]
fixInstrs ctx s (InstrBr lIdx:_) =
  do
    -- trace (showString "br " . shows s $ "") $ return ()
    return [InstrBr lIdx]
fixInstrs ctx s (InstrReturn:_) =
  do
    -- trace (showString "return " . shows s $ "") $ return ()
    return [InstrReturn]
fixInstrs ctx s (InstrCall fIdx:xs) =
  do
    -- trace (showString "call " . shows fIdx . showString " " . shows s $ "") $ return ()
    ft <- Ast.itxFuncType ctx fIdx
    s' <- applyFuncType ft s
    (InstrCall fIdx:) <$> fixInstrs ctx s' xs
fixInstrs ctx s (InstrCallIndirect ft:xs) =
  do
    s' <- applyFuncType ft =<< applyEffect ([TI32],[]) s
    (InstrCallIndirect ft:) <$> fixInstrs ctx s' xs

fixSimple :: MonadFail m => Stack -> SimpleInstr -> m SimpleInstr
fixSimple (TI32:_)           (SPrmInstrI32 PDrop)   = return $ SPrmInstrI32 PDrop
fixSimple (TI64:_)           (SPrmInstrI32 PDrop)   = return $ SPrmInstrI64 PDrop
fixSimple (TF32:_)           (SPrmInstrI32 PDrop)   = return $ SPrmInstrF32 PDrop
fixSimple (TF64:_)           (SPrmInstrI32 PDrop)   = return $ SPrmInstrF64 PDrop
fixSimple _                  (SPrmInstrI32 PDrop)   = fail "Type-incorrect drop instruction"
fixSimple (TI32:TI32:TI32:_) (SPrmInstrI32 PSelect) = return $ SPrmInstrI32 PSelect
fixSimple (TI32:TI64:TI64:_) (SPrmInstrI32 PSelect) = return $ SPrmInstrI64 PSelect
fixSimple (TI32:TF32:TF32:_) (SPrmInstrI32 PSelect) = return $ SPrmInstrF32 PSelect
fixSimple (TI32:TF64:TF64:_) (SPrmInstrI32 PSelect) = return $ SPrmInstrF64 PSelect
fixSimple _                  (SPrmInstrI32 PSelect) = fail "Type-incorrect select instruction"
fixSimple _ instr = return instr

-- applyFuncType :: MonadFail m => FuncType -> Stack -> m Stack
-- applyFuncType (FuncType xs ys) = applyFuncType' (Vector.toList xs, Vector.toList ys)

-- applyFuncType' :: MonadFail m => ([ValType], [ValType]) -> Stack -> m Stack
-- applyFuncType' (xs,ys) =
--   fmap (reverse ys ++) . dropEqPrefix (reverse xs)
