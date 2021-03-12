{-# LANGUAGE KindSignatures, FunctionalDependencies #-}

-- | General definition for `SimpleInstr` operations. This detaches operator
-- semantics from program-state transformations (e.g., stack push/pops).
--
-- This may be instantiated for concrete execution, symbolic execution, etc.
module Lang.Wasm.Algebra
  ( -- * Data Structures
    MonadWasmState (..)
  , SimpleAlgebra (..)
  , Address (..)
    -- * Functions
  , execAlgebra
  , popStackI32
  , popStackI64
  , popStackF32
  , popStackF64
  , pushStackI32
  , pushStackI64
  , pushStackF32
  , pushStackF64
  , magic
  , typeVal
  , setGlobals
  ) where

import Melude
-- Stdlib imports
import Control.Monad ( void )
import Control.Monad.Fail ( MonadFail )
import Data.Word ( Word32 )
-- Local library imports
import qualified Lang.Wasm.Data as WD
import Lang.Wasm.Ast
  ( LocalIdx, GlobalIdx (..)
  , WI32 (..), WI64, WF32, WF64
  , IUnop, IBinop, FUnop, FBinop, ITestop, IRelop, FRelop, Cvtop (..)
  , PrmInstr (..), VarInstr (..), MemInstr (..)
  , SimpleInstr (..), ValType (..)
  , Sx (..), Sat (..), MemArg (..), PVal (..)
  )
import Lang.Wasm.Data ( PGlobal (..) )


data Address i32 = Address i32 Word32

class MonadFail m => MonadWasmState i32 i64 f32 f64 mem m | m -> i32 i64 f32 f64 mem where
  -- isTrapped :: m ()
  popStack   :: m (PVal i32 i64 f32 f64)
  peekStack  :: m (PVal i32 i64 f32 f64)
  pushStack  :: PVal i32 i64 f32 f64 -> m ()
  pushLabel  :: m ()
  popLabel   :: m ()

  getLocal  :: LocalIdx -> m (PVal i32 i64 f32 f64)
  setLocal  :: LocalIdx -> PVal i32 i64 f32 f64 -> m ()

  getGlobal  :: GlobalIdx -> m (PVal i32 i64 f32 f64)
  setGlobal  :: GlobalIdx -> PVal i32 i64 f32 f64 -> m ()

  getGlobals :: m [PVal i32 i64 f32 f64]

  getMemory :: m mem
  setMemory :: mem -> m ()

  -- TODO: Maybe implement memory operations externally, and only use
  --       `getMemory` and `setMemory`.

  storeI32    :: Address i32 -> i32 -> m ()
  storeI64    :: Address i32 -> i64 -> m ()
  storeF32    :: Address i32 -> f32 -> m ()
  storeF64    :: Address i32 -> f64 -> m ()
  store8I32   :: Address i32 -> i32 -> m ()
  store16I32  :: Address i32 -> i32 -> m ()
  store8I64   :: Address i32 -> i64 -> m ()
  store16I64  :: Address i32 -> i64 -> m ()
  store32I64  :: Address i32 -> i64 -> m ()

  loadI32    :: Address i32 -> m i32
  loadI64    :: Address i32 -> m i64
  loadF32    :: Address i32 -> m f32
  loadF64    :: Address i32 -> m f64
  load8I32   :: Sx -> Address i32 -> m i32
  load16I32  :: Sx -> Address i32 -> m i32
  load8I64   :: Sx -> Address i32 -> m i64
  load16I64  :: Sx -> Address i32 -> m i64
  load32I64  :: Sx -> Address i32 -> m i64

  memGrow   :: i32 -> m i32
  memSize   :: m i32

setGlobals :: MonadWasmState i32 i64 f32 f64 mem m => [PVal i32 i64 f32 f64] -> m ()
setGlobals = mapM_ (uncurry setGlobal) . zip (map GlobalIdx [0..])

-- | Program-algebra-like definition for `SimpleInstr`s. This detaches operator
-- semantics from program-state transformations (e.g., stack push/pops).
--
-- Conversion operators are simplified for an easier (and more consistent)
-- interface.
--
-- Is there a better name for this than "algebra"? 'cuz I'm quite sure it's not
-- entirely accurate.
data SimpleAlgebra i32 i64 f32 f64 ( m :: * -> * ) =
  SimpleAlgebra {
    constI32  :: WI32 -> m i32
  , constI64  :: WI64 -> m i64
  , constF32  :: WF32 -> m f32
  , constF64  :: WF64 -> m f64
  
  , magicI32  :: m i32
  , magicI64  :: m i64
  , magicF32  :: m f32
  , magicF64  :: m f64

  , unopI32   :: IUnop  WI32 -> i32 -> m i32
  , unopI64   :: IUnop  WI64 -> i64 -> m i64
  , binopI32  :: IBinop WI32 -> i32 -> i32 -> m i32
  , binopI64  :: IBinop WI64 -> i64 -> i64 -> m i64

  , unopF32   :: FUnop  WF32 -> f32 -> m f32
  , unopF64   :: FUnop  WF64 -> f64 -> m f64
  , binopF32  :: FBinop WF32 -> f32 -> f32 -> m f32
  , binopF64  :: FBinop WF64 -> f64 -> f64 -> m f64

  , testopI32  :: ITestop WI32 -> i32 -> m i32
  , testopI64  :: ITestop WI64 -> i64 -> m i32

  , relopI32  :: IRelop WI32 -> i32 -> i32 -> m i32
  , relopI64  :: IRelop WI64 -> i64 -> i64 -> m i32
  , relopF32  :: FRelop WF32 -> f32 -> f32 -> m i32
  , relopF64  :: FRelop WF64 -> f64 -> f64 -> m i32

  , selectI32  :: i32 -> i32 -> i32 -> m i32 -- true false cond
  , selectI64  :: i64 -> i64 -> i32 -> m i64 -- true false cond
  , selectF32  :: f32 -> f32 -> i32 -> m f32 -- true false cond
  , selectF64  :: f64 -> f64 -> i32 -> m f64 -- true false cond

  , extend8toI32S    :: i32 -> m i32
  , extend16toI32S   :: i32 -> m i32
  , extend8toI64S    :: i64 -> m i64
  , extend16toI64S   :: i64 -> m i64
  , extend32toI64S   :: i64 -> m i64
  , extractI64toI32  :: i64 -> m i32
  , extendI32toI64   :: Sx -> i32 -> m i64

  , reinterpretF32toI32 :: f32 -> m i32
  , reinterpretF64toI64 :: f64 -> m i64
  , reinterpretI32toF32 :: i32 -> m f32
  , reinterpretI64toF64 :: i64 -> m f64

  , convertF32toF64 :: f32 -> m f64
  , convertF64toF32 :: f64 -> m f32
  , convertI32toF32 :: Sx -> i32 -> m f32
  , convertI32toF64 :: Sx -> i32 -> m f64
  , convertI64toF32 :: Sx -> i64 -> m f32
  , convertI64toF64 :: Sx -> i64 -> m f64

  , truncF32toI32  :: Sat -> Sx -> f32 -> m i32
  , truncF64toI32  :: Sat -> Sx -> f64 -> m i32
  , truncF32toI64  :: Sat -> Sx -> f32 -> m i64
  , truncF64toI64  :: Sat -> Sx -> f64 -> m i64
  }

execAlgebra :: MonadWasmState i32 i64 f32 f64 mem m
            => SimpleAlgebra i32 i64 f32 f64 m
            -> SimpleInstr
            -> m ()
execAlgebra a (SConstI32 v) = constI32 a v >>= pushStackI32
execAlgebra a (SConstI64 v) = constI64 a v >>= pushStackI64
execAlgebra a (SConstF32 v) = constF32 a v >>= pushStackF32
execAlgebra a (SConstF64 v) = constF64 a v >>= pushStackF64
execAlgebra a (SUnopI32 op) = unaryOp (unopI32 a op) popStackI32 pushStackI32
execAlgebra a (SUnopI64 op) = unaryOp (unopI64 a op) popStackI64 pushStackI64
execAlgebra a (SUnopF32 op) = unaryOp (unopF32 a op) popStackF32 pushStackF32
execAlgebra a (SUnopF64 op) = unaryOp (unopF64 a op) popStackF64 pushStackF64
execAlgebra a (STestopI32 op) = unaryOp (testopI32 a op) popStackI32 pushStackI32
execAlgebra a (STestopI64 op) = unaryOp (testopI64 a op) popStackI64 pushStackI32
execAlgebra a (SBinopI32 op) = binaryOp (binopI32 a op) popStackI32 popStackI32 pushStackI32
execAlgebra a (SBinopI64 op) = binaryOp (binopI64 a op) popStackI64 popStackI64 pushStackI64
execAlgebra a (SBinopF32 op) = binaryOp (binopF32 a op) popStackF32 popStackF32 pushStackF32
execAlgebra a (SBinopF64 op) = binaryOp (binopF64 a op) popStackF64 popStackF64 pushStackF64
execAlgebra a (SRelopI32 op) = binaryOp (relopI32 a op) popStackI32 popStackI32 pushStackI32
execAlgebra a (SRelopI64 op) = binaryOp (relopI64 a op) popStackI64 popStackI64 pushStackI32
execAlgebra a (SRelopF32 op) = binaryOp (relopF32 a op) popStackF32 popStackF32 pushStackI32
execAlgebra a (SRelopF64 op) = binaryOp (relopF64 a op) popStackF64 popStackF64 pushStackI32
execAlgebra a (SPrmInstrI32 PDrop) = void popStackI32
execAlgebra a (SPrmInstrI64 PDrop) = void popStackI64
execAlgebra a (SPrmInstrF32 PDrop) = void popStackF32
execAlgebra a (SPrmInstrF64 PDrop) = void popStackF64
execAlgebra a (SPrmInstrI32 PSelect) = execSelect (selectI32 a) popStackI32 pushStackI32
execAlgebra a (SPrmInstrI64 PSelect) = execSelect (selectI64 a) popStackI64 pushStackI64
execAlgebra a (SPrmInstrF32 PSelect) = execSelect (selectF32 a) popStackF32 pushStackF32
execAlgebra a (SPrmInstrF64 PSelect) = execSelect (selectF64 a) popStackF64 pushStackF64
execAlgebra a (SVarInstr (VLocalGet lIdx))  = getLocal lIdx >>= pushStack
execAlgebra a (SVarInstr (VLocalSet lIdx))  = popStack  >>= setLocal lIdx
execAlgebra a (SVarInstr (VLocalTee lIdx))  = peekStack >>= setLocal lIdx
execAlgebra a (SVarInstr (VGlobalGet gIdx)) = getGlobal gIdx >>= pushStack
execAlgebra a (SVarInstr (VGlobalSet gIdx)) = popStack >>= setGlobal gIdx

execAlgebra a (SMemInstr (MI32Load ma))      = loadInstr (loadI32   . toAddress ma) pushStackI32
execAlgebra a (SMemInstr (MI64Load ma))      = loadInstr (loadI64   . toAddress ma) pushStackI64
execAlgebra a (SMemInstr (MF32Load ma))      = loadInstr (loadF32   . toAddress ma) pushStackF32
execAlgebra a (SMemInstr (MF64Load ma))      = loadInstr (loadF64   . toAddress ma) pushStackF64
execAlgebra a (SMemInstr (MI32Load8 sx ma))  = loadInstr (load8I32  sx . toAddress ma) pushStackI32
execAlgebra a (SMemInstr (MI64Load8 sx ma))  = loadInstr (load8I64  sx . toAddress ma) pushStackI64
execAlgebra a (SMemInstr (MI32Load16 sx ma)) = loadInstr (load16I32 sx . toAddress ma) pushStackI32
execAlgebra a (SMemInstr (MI64Load16 sx ma)) = loadInstr (load16I64 sx . toAddress ma) pushStackI64
execAlgebra a (SMemInstr (MI64Load32 sx ma)) = loadInstr (load32I64 sx . toAddress ma) pushStackI64

execAlgebra a (SMemInstr (MI32Store ma)) = storeInstr popStackI32 (storeI32 . toAddress ma)
execAlgebra a (SMemInstr (MI64Store ma)) = storeInstr popStackI64 (storeI64 . toAddress ma)
execAlgebra a (SMemInstr (MF32Store ma)) = storeInstr popStackF32 (storeF32 . toAddress ma)
execAlgebra a (SMemInstr (MF64Store ma)) = storeInstr popStackF64 (storeF64 . toAddress ma)

execAlgebra a (SMemInstr (MI32Store8 ma))  = storeInstr popStackI32 (store8I32  . toAddress ma)
execAlgebra a (SMemInstr (MI32Store16 ma)) = storeInstr popStackI32 (store16I32 . toAddress ma)
execAlgebra a (SMemInstr (MI64Store8 ma))  = storeInstr popStackI64 (store8I64  . toAddress ma)
execAlgebra a (SMemInstr (MI64Store16 ma)) = storeInstr popStackI64 (store16I64 . toAddress ma)
execAlgebra a (SMemInstr (MI64Store32 ma)) = storeInstr popStackI64 (store16I64 . toAddress ma)

execAlgebra a (SMemInstr MMemorySize) = memSize >>= pushStackI32
execAlgebra a (SMemInstr MMemoryGrow) = unaryOp memGrow popStackI32 pushStackI32

execAlgebra a (SCvtop CI32Extend8S)          = unaryOp (extend8toI32S a) popStackI32 pushStackI32
execAlgebra a (SCvtop CI32Extend16S)         = unaryOp (extend16toI32S a) popStackI32 pushStackI32
execAlgebra a (SCvtop CI64Extend8S)          = unaryOp (extend8toI64S a) popStackI64 pushStackI64
execAlgebra a (SCvtop CI64Extend16S)         = unaryOp (extend16toI64S a) popStackI64 pushStackI64
execAlgebra a (SCvtop CI64Extend32S)         = unaryOp (extend32toI64S a) popStackI64 pushStackI64
execAlgebra a (SCvtop CI32WrapI64)           = unaryOp (extractI64toI32 a) popStackI64 pushStackI32
execAlgebra a (SCvtop (CI64ExtendI32 sx))    = unaryOp (extendI32toI64 a sx) popStackI32 pushStackI64
execAlgebra a (SCvtop (CI32TruncF32 sat sx)) = unaryOp (truncF32toI32 a sat sx) popStackF32 pushStackI32
execAlgebra a (SCvtop (CI32TruncF64 sat sx)) = unaryOp (truncF64toI32 a sat sx) popStackF64 pushStackI32

execAlgebra a (SCvtop (CI64TruncF32 sat sx)) = unaryOp (truncF32toI64 a sat sx) popStackF32 pushStackI64
execAlgebra a (SCvtop (CI64TruncF64 sat sx)) = unaryOp (truncF64toI64 a sat sx) popStackF64 pushStackI64
execAlgebra a (SCvtop CF32DemoteF64)         = unaryOp (convertF64toF32 a) popStackF64 pushStackF32
execAlgebra a (SCvtop CF64PromoteF32)        = unaryOp (convertF32toF64 a) popStackF32 pushStackF64
execAlgebra a (SCvtop (CF32ConvertI32 sx))   = unaryOp (convertI32toF32 a sx) popStackI32 pushStackF32
execAlgebra a (SCvtop (CF32ConvertI64 sx))   = unaryOp (convertI64toF32 a sx) popStackI64 pushStackF32
execAlgebra a (SCvtop (CF64ConvertI32 sx))   = unaryOp (convertI32toF64 a sx) popStackI32 pushStackF64
execAlgebra a (SCvtop (CF64ConvertI64 sx))   = unaryOp (convertI64toF64 a sx) popStackI64 pushStackF64
execAlgebra a (SCvtop CI32ReinterpretF32)    = unaryOp (reinterpretF32toI32 a) popStackF32 pushStackI32
execAlgebra a (SCvtop CI64ReinterpretF64)    = unaryOp (reinterpretF64toI64 a) popStackF64 pushStackI64
execAlgebra a (SCvtop CF32ReinterpretI32)    = unaryOp (reinterpretI32toF32 a) popStackI32 pushStackF32
execAlgebra a (SCvtop CF64ReinterpretI64)    = unaryOp (reinterpretI64toF64 a) popStackI64 pushStackF64

execSelect :: MonadWasmState i32 i64 f32 f64 mem m
           => ( a -> a -> i32 -> m a ) -> m a -> ( a -> m x ) -> m x
execSelect fSelect fPop fPush =
  do
    cond   <- popStackI32
    iFalse <- fPop
    iTrue  <- fPop
    fPush =<< fSelect iTrue iFalse cond

popStackI32 :: MonadWasmState i32 i64 f32 f64 mem m => m i32
popStackI32 = popStack >>= forceI32

popStackI64 :: MonadWasmState i32 i64 f32 f64 mem m => m i64
popStackI64 = popStack >>= forceI64

popStackF32 :: MonadWasmState i32 i64 f32 f64 mem m => m f32
popStackF32 = popStack >>= forceF32

popStackF64 :: MonadWasmState i32 i64 f32 f64 mem m => m f64
popStackF64 = popStack >>= forceF64

pushStackI32  :: MonadWasmState i32 i64 f32 f64 mem m => i32 -> m ()
pushStackI32 = pushStack . VI32

pushStackI64  :: MonadWasmState i32 i64 f32 f64 mem m => i64 -> m ()
pushStackI64 = pushStack . VI64

pushStackF32  :: MonadWasmState i32 i64 f32 f64 mem m => f32 -> m ()
pushStackF32 = pushStack . VF32

pushStackF64  :: MonadWasmState i32 i64 f32 f64 mem m => f64 -> m ()
pushStackF64 = pushStack . VF64

magic :: Monad m => SimpleAlgebra i32 i64 f32 f64 m -> ValType -> m (PVal i32 i64 f32 f64)
magic a TI32 = VI32 <$> magicI32 a
magic a TI64 = VI64 <$> magicI64 a
magic a TF32 = VF32 <$> magicF32 a
magic a TF64 = VF64 <$> magicF64 a

typeVal :: (Monoid m1, Monoid m2, Monoid m3, Monoid m4) => ValType -> PVal m1 m2 m3 m4
typeVal TI32 = VI32 mempty
typeVal TI64 = VI64 mempty
typeVal TF32 = VF32 mempty
typeVal TF64 = VF64 mempty


-- # Internal Helpers

storeInstr :: MonadWasmState i32 i64 f32 f64 mem m
           => m val
           -> ( i32 -> val -> m x )
           -> m x
storeInstr fPopVal fStore =
  do
    val <- fPopVal
    addrBase <- popStackI32
    fStore addrBase val
    

loadInstr :: MonadWasmState i32 i64 f32 f64 mem m
          => ( i32 -> m val )
          -> (val -> m x)
          -> m x
loadInstr fLoadAll fPush =
  do
    val <- fLoadAll =<< popStackI32
    fPush val

toAddress :: MemArg -> i32 -> Address i32
toAddress a v = Address v (toNum $ maOffset a)

-- | Unary WASM operator, for which input popping and result pushing is handled.
unaryOp :: MonadWasmState i32 i64 f32 f64 mem m => ( a -> m c ) -> m a -> ( c -> m x ) -> m x
unaryOp fOp a fPush =
  do
    a' <- a
    c <- fOp a'
    fPush c

-- | Binary WASM operator, for which input popping and result pushing is
-- handled. Note that the right-most argument is on /top/ of the stack.
binaryOp :: MonadWasmState i32 i64 f32 f64 mem m => ( a -> b -> m c ) -> m a -> m b -> ( c -> m x ) -> m x
binaryOp fOp a b fPush =
  do
    b' <- b
    a' <- a
    c <- fOp a' b'
    fPush c

forceI32 :: MonadFail m => PVal i32 i64 f32 f64 -> m i32
forceI32 (VI32 i) = return i
forceI32 _        = fail "Mismatched stack type"

forceI64 :: MonadFail m => PVal i32 i64 f32 f64 -> m i64
forceI64 (VI64 i) = return i
forceI64 _        = fail "Mismatched stack type"

forceF32 :: MonadFail m => PVal i32 i64 f32 f64 -> m f32
forceF32 (VF32 i) = return i
forceF32 _        = fail "Mismatched stack type"

forceF64 :: MonadFail m => PVal i32 i64 f32 f64 -> m f64
forceF64 (VF64 i) = return i
forceF64 _        = fail "Mismatched stack type"
