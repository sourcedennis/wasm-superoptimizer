{-# LANGUAGE KindSignatures, DeriveGeneric, StrictData #-}

module Lang.Wasm.Ast.Effect where

import Melude
-- Stdlib imports
import Control.Monad.Fail ( MonadFail )
import GHC.Generics (Generic)
import Control.DeepSeq ( NFData )
-- Extra stdlib imports
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
-- Local imports
import Lang.Wasm.Ast.Indices ( GlobalIdx (..), LocalIdx (..), FuncIdx (..) )
import Lang.Wasm.Ast.Types ( ValType (..), GlobalType (..), FuncType (..) )
import Lang.Wasm.Ast.Instrs ( Instr (..), SimpleInstr (..), PrmInstr (..),
  VarInstr (..), Cvtop (..), MemInstr (..) )

-- | A helper structure describing the module + function context in which an
-- instruction occurs.
data InstrCtx =
  InstrCtx {
    itxGlobals    :: Vector GlobalType
  , itxParams     :: Vector ValType
  , itxLocals     :: Vector ValType
    -- Not the /types/ section! Returns the the type of function; so this is
    -- indexed by `FuncIdx`.
  , itxFuncTypes  :: Vector FuncType
  }
  deriving (Show, Generic)

itxGlobal :: MonadFail m => InstrCtx -> GlobalIdx -> m GlobalType
itxGlobal ctx (GlobalIdx i) = failMaybeMsg "Missing global" (itxGlobals ctx !? i)

itxLocal :: MonadFail m => InstrCtx -> LocalIdx -> m ValType
itxLocal ctx (LocalIdx i) =
  let numParams = Vector.length $ itxParams ctx
  in
  if i < numParams then
    failMaybeMsg "Missing param" (itxParams ctx !? i)
  else
    failMaybeMsg "Missing local" (itxLocals ctx !? (i - numParams))

itxFuncType :: MonadFail m => InstrCtx -> FuncIdx -> m FuncType
itxFuncType ctx (FuncIdx i) = failMaybeMsg "Missing func" (itxFuncTypes ctx !? i)

instance NFData InstrCtx

-- | Stack of types. Grows to the /left/.
type TypeStack = [ValType]

applyEffect :: MonadFail m => ([ValType],[ValType]) -> TypeStack -> m TypeStack
applyEffect (xs, ys) = fmap (reverse ys ++) . dropEqPrefix (reverse xs)

applyEffectFull :: MonadFail m => ([ValType],[ValType]) -> NonEmpty TypeStack -> m (NonEmpty TypeStack)
applyEffectFull (xs, ys) (zs:|zss) = (:| zss) <$> fmap (reverse ys ++) (dropEqPrefix (reverse xs) zs)

applyFuncType :: MonadFail m => FuncType -> TypeStack -> m TypeStack
applyFuncType (FuncType xs ys) = applyEffect (Vector.toList xs, Vector.toList ys)

-- | 
-- >>> combineEffects ([TI64,TI64],[TI32]) ([TI32,TI32],[TF32])
-- Just ([TI32,TI64,TI64],[TF32])
--
-- >>> combineEffects ([TI32],[TI64,TF32,TF64]) ([TF32,TF64],[TI32])
-- Just ([TI32],[TI64,TI32])
combineEffects :: ([ValType],[ValType]) -> ([ValType],[ValType]) -> Maybe ([ValType],[ValType])
combineEffects (xs,ys) (zs,ws) =
  do
    (ps, qs) <- combineEffects' (reverse xs, reverse ys) (reverse zs, reverse ws)
    return (reverse ps, reverse qs)
  where
  combineEffects' :: ([ValType],[ValType]) -> ([ValType],[ValType]) -> Maybe ([ValType],[ValType])
  combineEffects' (xs,y:ys) (z:zs,ws)
    | y == z     = combineEffects' (xs, ys) (zs, ws)
    | otherwise  = Nothing
  combineEffects' (xs,[]) (zs,ws) = Just (xs++zs,ws)
  combineEffects' (xs,ys) ([],ws) = Just (xs,ws++ys)

instrEffect :: MonadFail m => InstrCtx -> Instr -> m (Maybe ([ValType], [ValType]))
instrEffect ctx InstrUnreachable          = return Nothing
instrEffect ctx InstrNop                  = return $ Just ([], [])
instrEffect ctx (InstrSimple x)           = Just <$> simpleInstrEffect ctx x
instrEffect ctx (InstrBlock t _)          = return $ Just $ unft t
instrEffect ctx (InstrLoop t _)           = return $ Just $ unft t
instrEffect ctx (InstrIf t _ _)           = return $ Just (Vector.toList (ftParams t) ++ [TI32], Vector.toList $ ftResults t)
instrEffect ctx (InstrBr lIdx)            = return Nothing
instrEffect ctx (InstrBrIf lIdx)          = return $ Just ([TI32],[])
instrEffect ctx (InstrBrTable lIdxs lIdx) = return Nothing
instrEffect ctx InstrReturn               = return Nothing
instrEffect ctx (InstrCall fIdx)          = Just . unft <$> itxFuncType ctx fIdx
instrEffect ctx (InstrCallIndirect t)     = return $ Just $ unft t

simpleInstrEffect :: MonadFail m => InstrCtx -> SimpleInstr -> m ([ValType],[ValType])
simpleInstrEffect ctx (SConstI32 _)          = return ([],[TI32])
simpleInstrEffect ctx (SConstI64 _)          = return ([],[TI64])
simpleInstrEffect ctx (SConstF32 _)          = return ([],[TF32])
simpleInstrEffect ctx (SConstF64 _)          = return ([],[TF64])
simpleInstrEffect ctx (SUnopI32 _)           = return ([TI32],[TI32])
simpleInstrEffect ctx (SUnopI64 _)           = return ([TI64],[TI64])
simpleInstrEffect ctx (SBinopI32 _)          = return ([TI32,TI32],[TI32])
simpleInstrEffect ctx (SBinopI64 _)          = return ([TI64,TI64],[TI64])
simpleInstrEffect ctx (SUnopF32 _)           = return ([TF32],[TF32])
simpleInstrEffect ctx (SUnopF64 _)           = return ([TF64],[TF64])
simpleInstrEffect ctx (SBinopF32 _)          = return ([TF32,TF32],[TF32])
simpleInstrEffect ctx (SBinopF64 _)          = return ([TF64,TF64],[TF64])
simpleInstrEffect ctx (STestopI32 _)         = return ([TI32],[TI32])
simpleInstrEffect ctx (STestopI64 _)         = return ([TI64],[TI32])
simpleInstrEffect ctx (SRelopI32 _)          = return ([TI32,TI32],[TI32])
simpleInstrEffect ctx (SRelopI64 _)          = return ([TI64,TI64],[TI32])
simpleInstrEffect ctx (SRelopF32 _)          = return ([TF32,TF32],[TI32])
simpleInstrEffect ctx (SRelopF64 _)          = return ([TF64,TF64],[TI32])
simpleInstrEffect ctx (SCvtop op)            = return $ cvtInstrEffect op
simpleInstrEffect ctx (SPrmInstrI32 PDrop)   = return ([TI32],[])
simpleInstrEffect ctx (SPrmInstrI64 PDrop)   = return ([TI64],[])
simpleInstrEffect ctx (SPrmInstrF32 PDrop)   = return ([TF32],[])
simpleInstrEffect ctx (SPrmInstrF64 PDrop)   = return ([TF64],[])
simpleInstrEffect ctx (SPrmInstrI32 PSelect) = return ([TI32,TI32,TI32],[TI32])
simpleInstrEffect ctx (SPrmInstrI64 PSelect) = return ([TI64,TI64,TI32],[TI64])
simpleInstrEffect ctx (SPrmInstrF32 PSelect) = return ([TF32,TF32,TI32],[TF32])
simpleInstrEffect ctx (SPrmInstrF64 PSelect) = return ([TF64,TF64,TI32],[TF64])
simpleInstrEffect ctx (SVarInstr instr)      = varInstrEffect ctx instr
simpleInstrEffect ctx (SMemInstr instr)      = return $ memInstrEffect instr

cvtInstrEffect :: Cvtop -> ([ValType],[ValType])
cvtInstrEffect CI32Extend8S       = ([TI32],[TI32])
cvtInstrEffect CI32Extend16S      = ([TI32],[TI32])
cvtInstrEffect CI64Extend8S       = ([TI64],[TI64])
cvtInstrEffect CI64Extend16S      = ([TI64],[TI64])
cvtInstrEffect CI64Extend32S      = ([TI64],[TI64])
cvtInstrEffect CI32WrapI64        = ([TI64],[TI32])
cvtInstrEffect (CI64ExtendI32 _)  = ([TI32],[TI64])
cvtInstrEffect (CI32TruncF32 _ _) = ([TF32],[TI32])
cvtInstrEffect (CI32TruncF64 _ _) = ([TF64],[TI32])
cvtInstrEffect (CI64TruncF32 _ _) = ([TF32],[TI64])
cvtInstrEffect (CI64TruncF64 _ _) = ([TF64],[TI64])
cvtInstrEffect CF32DemoteF64      = ([TF64],[TF32])
cvtInstrEffect CF64PromoteF32     = ([TF32],[TF64])
cvtInstrEffect (CF32ConvertI32 _) = ([TI32],[TF32])
cvtInstrEffect (CF32ConvertI64 _) = ([TI64],[TF32])
cvtInstrEffect (CF64ConvertI32 _) = ([TI32],[TF64])
cvtInstrEffect (CF64ConvertI64 _) = ([TI64],[TF64])
cvtInstrEffect CI32ReinterpretF32 = ([TF32],[TI32])
cvtInstrEffect CI64ReinterpretF64 = ([TF64],[TI64])
cvtInstrEffect CF32ReinterpretI32 = ([TI32],[TF32])
cvtInstrEffect CF64ReinterpretI64 = ([TI64],[TF64])

varInstrEffect :: MonadFail m => InstrCtx -> VarInstr -> m ([ValType],[ValType])
varInstrEffect ctx (VLocalGet lIdx)  = itxLocal  ctx lIdx >>= \t -> return ([],[t])
varInstrEffect ctx (VLocalSet lIdx)  = itxLocal  ctx lIdx >>= \t -> return ([t],[])
varInstrEffect ctx (VLocalTee lIdx)  = itxLocal  ctx lIdx >>= \t -> return ([t],[t])
varInstrEffect ctx (VGlobalGet gIdx) = itxGlobal ctx gIdx >>= \t -> return ([],[gtType t])
varInstrEffect ctx (VGlobalSet gIdx) = itxGlobal ctx gIdx >>= \t -> return ([gtType t],[])

memInstrEffect :: MemInstr -> ([ValType],[ValType])
memInstrEffect (MI32Load _)     = ([TI32],[TI32])
memInstrEffect (MI64Load _)     = ([TI32],[TI64])
memInstrEffect (MF32Load _)     = ([TI32],[TF32])
memInstrEffect (MF64Load _)     = ([TI32],[TF64])
memInstrEffect (MI32Store _)    = ([TI32,TI32],[])
memInstrEffect (MI64Store _)    = ([TI32,TI64],[])
memInstrEffect (MF32Store _)    = ([TI32,TF32],[])
memInstrEffect (MF64Store _)    = ([TI32,TF64],[])
memInstrEffect (MI32Load8 _ _)  = ([TI32],[TI32])
memInstrEffect (MI64Load8 _ _)  = ([TI32],[TI64])
memInstrEffect (MI32Load16 _ _) = ([TI32],[TI32])
memInstrEffect (MI64Load16 _ _) = ([TI32],[TI64])
memInstrEffect (MI64Load32 _ _) = ([TI32],[TI64])
memInstrEffect (MI32Store8 _)   = ([TI32,TI32],[])
memInstrEffect (MI64Store8 _)   = ([TI32,TI64],[])
memInstrEffect (MI32Store16 _)  = ([TI32,TI32],[])
memInstrEffect (MI64Store16 _)  = ([TI32,TI64],[])
memInstrEffect (MI64Store32 _)  = ([TI32,TI64],[])
memInstrEffect MMemorySize      = ([],[TI32])
memInstrEffect MMemoryGrow      = ([TI32],[TI32])

unft :: FuncType -> ([ValType], [ValType])
unft t = (Vector.toList $ ftParams t, Vector.toList $ ftResults t)
