{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances,
             TemplateHaskell
#-}

-- | 
--
-- Might as well implement type checking of the graph as a dataflow problem;
-- This avoids re-implementing of graph traversal.
module Lang.Wasm.Dataflow.TypeCheck
  ( -- * Data Structures
    TypeStack
  , NodeType (..)
  , FullStack
    -- * Dataflow Functions
  , confluence
  , transfer
  , transferBwd
    -- * Helpers Functions
  , isNodeTypeOk
  , terminalStack
  ) where

import Melude
-- Stdlib imports
import           Data.Word ( Word32 )
import           Control.Monad ( void )
import           Control.Monad.Fail ( MonadFail )
-- Extra stdlib imports
import qualified Control.Monad.State as S
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.HashSet as HashSet
import           Data.HashSet ( HashSet )
import qualified Data.Vector as Vector
import           Data.Vector ( Vector, (!?) )
-- Local library imports
import qualified Lang.Wasm.Data as WD
import qualified Lang.Wasm.Ast as Ast
import Lang.Wasm.Ast
  ( LocalIdx(..), GlobalIdx(..), FuncIdx(..), ValType(..), GlobalType(..)
  , FuncType(..), SimpleInstr(..), WI32, WI64, WF32, WF64, IUnop(..), IBinop(..)
  , FUnop(..), FBinop(..), ITestop(..), IRelop(..), FRelop(..), PrmInstr(..)
  , VarInstr(..), MemInstr(..), Sx(..), MemArg(..), Mut(..), PVal(..)
  , InstrCtx(..), TypeStack, instrEffect, valType
  )
-- Local imports
import qualified Algorithm.Dataflow as Dataflow
import qualified Lang.Wasm.Algebra as A
import           Lang.Wasm.Algebra
  ( SimpleAlgebra (..), MonadWasmState (..), execAlgebra )
import qualified Lang.Wasm.AlgebraBwd as AB
import           Lang.Wasm.AlgebraBwd
  ( SimpleBwdAlgebra (..), execBwdAlgebra )
import qualified Lang.Wasm.Dataflow as D
import           Lang.Wasm.Dataflow
  ( FlowAlgebra (..), FlowBwdAlgebra (..)
  )
import           Lang.Wasm.Process


-- # Data Structures #

-- | A stack where only element /types/ are known
type FullStack = WD.PStack () () () ()

-- | Node in the lattice of graph-node types.
data NodeType
  = TypeOk FullStack
  | TypeUnknown -- bottom
  | TypeInvalid (HashSet FullStack) -- top. value is for debugging only, and value not part of lattice
  deriving Eq

instance Show NodeType where
  showsPrec d (TypeOk s)       = showParen (d>10) (ß"TypeOk " . shows s)
  showsPrec d TypeUnknown      = ß"TypeUnknown"
  showsPrec d (TypeInvalid xs) = showParen (d>10) (ß"TypeInvalid " . shows (HashSet.toList xs))


-- ## Internal Data Structures ##

data TypeProgState =
  TypeProgState {
    _tpsGlobals  :: Vector GlobalType
  , _tpsLocals   :: LocalIdx -> Maybe ValType
  , _tpsStack    :: FullStack
  }

$(makeLenses ''TypeProgState)


-- # Dataflow Functions

confluence :: NodeType -> NodeType -> NodeType
confluence (TypeOk xs) (TypeOk ys)
  | xs == ys   = TypeOk xs
  | otherwise  = TypeInvalid (HashSet.fromList [xs,ys])
confluence TypeUnknown y = y
confluence x TypeUnknown = x
confluence (TypeInvalid xs) (TypeInvalid ys) = TypeInvalid (HashSet.union xs ys)
confluence (TypeOk x) (TypeInvalid ys) = TypeInvalid (HashSet.insert x ys)
confluence (TypeInvalid xs) (TypeOk y) = TypeInvalid (HashSet.insert y xs)

transfer :: InstrCtx -> FlowEdge -> ( NodeType -> NodeType )
transfer ctx edge (TypeOk stack)  = transferStack ctx edge stack
transfer _   edge TypeUnknown     = TypeUnknown
transfer _   edge (TypeInvalid _) = TypeInvalid HashSet.empty

transferBwd :: InstrCtx -> FlowEdge -> ( NodeType -> NodeType )
transferBwd ctx edge (TypeOk stack)  = transferBwdStack ctx edge stack
transferBwd _   edge TypeUnknown     = TypeUnknown
transferBwd _   edge (TypeInvalid _) = TypeInvalid HashSet.empty

-- | Internal.
transferStack :: InstrCtx -> FlowEdge -> ( FullStack -> NodeType )
transferStack ctx edge s =
  let fType = fromMaybe (error "Missing function") . Ast.itxFuncType ctx
      progState = TypeProgState (Ast.itxGlobals ctx) (Ast.itxLocal ctx) s
  in
  case execStateT (D.execFlowAlgebra fType flowAlgebra edge) progState of
    Nothing -> TypeInvalid HashSet.empty -- Crucially, /don't/ transfer the
    -- value (which is for debugging only), as it may cause non-termination
    -- of the dataflow algorithm on type-incorrect graphs.
    Just progState' -> TypeOk (progState' ^. tpsStack)

-- | Internal.
transferBwdStack :: InstrCtx -> FlowEdge -> ( FullStack -> NodeType )
transferBwdStack ctx edge s =
  let fType = fromMaybe (error "Missing function") . Ast.itxFuncType ctx
      progState = TypeProgState (Ast.itxGlobals ctx) (Ast.itxLocal ctx) s
  in
  case execStateT (D.execBwdFlowAlgebra fType flowBwdAlgebra edge) progState of
    Nothing -> TypeInvalid HashSet.empty -- Crucially, /don't/ transfer the
    -- value (which is for debugging only), as it may cause non-termination
    -- of the dataflow algorithm on type-incorrect graphs.
    Just progState' -> TypeOk (progState' ^. tpsStack)


-- # Exposed Helpers

isNodeTypeOk :: NodeType -> Bool
isNodeTypeOk (TypeOk _) = True
isNodeTypeOk _ = False

terminalStack :: Ast.ResultType -> FullStack
terminalStack rt = map (WD.val () () () ()) (reverse $ Vector.toList rt) :| []


-- # Internal Helper

instance MonadWasmState () () () () () (StateT TypeProgState Maybe) where
  popStack =
    withStateLens tpsStack $ StateT WD.popStackVal

  peekStack =
    withStateLens tpsStack $ getsM WD.peekStackVal

  pushStack =
    withStateLens tpsStack . S.modify . WD.pushStack
  
  pushLabel =
    withStateLens tpsStack $ S.modify $ WD.pushStackLabel []
  
  popLabel =
    withStateLens tpsStack $ modifyM WD.popStackLabel

  getLocal lIdx =
    do
      s <- S.get
      x <- failMaybe $ (s ^. tpsLocals) lIdx
      return (A.typeVal x)

  setLocal lIdx v =
    do
      s <- S.get
      x <- failMaybeMsg "Local out of bounds" ((s ^. tpsLocals) lIdx)
      failIf (x /= valType v) "Type incorrect local update"

  getGlobal (GlobalIdx gIdx) =
    do
      s <- S.get
      x <- failMaybeMsg "Global out of bounds" ((s ^. tpsGlobals) !? gIdx)
      return (A.typeVal $ gtType x)

  setGlobal (GlobalIdx gIdx) v =
    do
      s <- S.get
      x <- failMaybeMsg "Global out of bounds" ((s ^. tpsGlobals) !? gIdx)
      failIf (gtType x /= valType v) "Type incorrect global update"

  getGlobals =
    do
      s <- S.get
      let gs = s ^. tpsGlobals
      return $ map (WD.val () () () () . Ast.gtType) $ Vector.toList (s ^. tpsGlobals)
  
  getMemory = return ()
  setMemory _ = return ()

  -- These below are currenly unused by backward analysis
  storeI32   = const $ const $ return ()
  storeI64   = const $ const $ return ()
  storeF32   = const $ const $ return ()
  storeF64   = const $ const $ return ()
  store8I32  = const $ const $ return ()
  store16I32 = const $ const $ return ()
  store8I64  = const $ const $ return ()
  store16I64 = const $ const $ return ()
  store32I64 = const $ const $ return ()

  loadI32     _ = return ()
  loadI64     _ = return ()
  loadF32     _ = return ()
  loadF64     _ = return ()
  load8I32  _ _ = return ()
  load16I32 _ _ = return ()
  load8I64  _ _ = return ()
  load16I64 _ _ = return ()
  load32I64 _ _ = return ()

  memGrow = const $ return ()
  memSize = return ()

flowAlgebra :: Monad m
            => FlowAlgebra () () () () () m
flowAlgebra =
  FlowAlgebra {
    instrAlgebra = algebra
  , callExternal = \fIdx gs mem args resTypes -> return (gs, mem, map A.typeVal resTypes)
  , callIndirect = \fIdx gs mem args resTypes -> return (gs, mem, map A.typeVal resTypes)

  , assertTrue   = const $ return ()
  , assertConstI32 = const $ const $ return ()
  , assertGeq      = const $ const $ return ()
  }

algebra :: Monad m
        => SimpleAlgebra () () () () m
algebra =
  SimpleAlgebra {
    constI32 = const $ return ()
  , constI64 = const $ return ()
  , constF32 = const $ return ()
  , constF64 = const $ return ()
  
  , magicI32 = return ()
  , magicI64 = return ()
  , magicF32 = return ()
  , magicF64 = return ()

  , unopI32  = const $ const $ return ()
  , unopI64  = const $ const $ return ()
  , binopI32 = const $ const $ const $ return ()
  , binopI64 = const $ const $ const $ return ()

  , unopF32  = const $ const $ return ()
  , unopF64  = const $ const $ return ()
  , binopF32 = const $ const $ const $ return ()
  , binopF64 = const $ const $ const $ return ()

  , testopI32 = const $ const $ return ()
  , testopI64 = const $ const $ return ()

  , relopI32 = const $ const $ const $ return ()
  , relopI64 = const $ const $ const $ return ()
  , relopF32 = const $ const $ const $ return ()
  , relopF64 = const $ const $ const $ return ()

  , selectI32 = const $ const $ const $ return ()
  , selectI64 = const $ const $ const $ return ()
  , selectF32 = const $ const $ const $ return ()
  , selectF64 = const $ const $ const $ return ()

  , extend8toI32S   = const $ return ()
  , extend16toI32S  = const $ return ()
  , extend8toI64S   = const $ return ()
  , extend16toI64S  = const $ return ()
  , extend32toI64S  = const $ return ()
  , extractI64toI32 = const $ return ()
  , extendI32toI64  = const $ const $ return ()

  , reinterpretF32toI32 = const $ return ()
  , reinterpretF64toI64 = const $ return ()
  , reinterpretI32toF32 = const $ return ()
  , reinterpretI64toF64 = const $ return ()

  , convertF32toF64 = const $ return ()
  , convertF64toF32 = const $ return ()
  , convertI32toF32 = const $ const $ return ()
  , convertI32toF64 = const $ const $ return ()
  , convertI64toF32 = const $ const $ return ()
  , convertI64toF64 = const $ const $ return ()

  , truncF32toI32 = const $ const $ const $ return ()
  , truncF64toI32 = const $ const $ const $ return ()
  , truncF32toI64 = const $ const $ const $ return ()
  , truncF64toI64 = const $ const $ const $ return ()
  }

flowBwdAlgebra :: Monad m
               => FlowBwdAlgebra () () () () () m
flowBwdAlgebra =
  FlowBwdAlgebra {
    bwdInstrAlgebra = bwdAlgebra
  , bwdCallExternal = \fIdx gs args res -> return (gs, map A.typeVal res)
  , bwdCallIndirect = \gs args xs -> return (gs, map A.typeVal xs, ())
  }

bwdAlgebra :: Monad m
           => SimpleBwdAlgebra () () () () () m
bwdAlgebra =
  SimpleBwdAlgebra {
    topI32 = return ()
  , topI64 = return ()
  , topF32 = return ()
  , topF64 = return ()

  , bottomI32 = return ()
  , bottomI64 = return ()
  , bottomF32 = return ()
  , bottomF64 = return ()

  , confluenceI32 = const $ const $ return ()
  , confluenceI64 = const $ const $ return ()
  , confluenceF32 = const $ const $ return ()
  , confluenceF64 = const $ const $ return ()

  , bwdUnopI32  = const $ const $ return ()
  , bwdUnopI64  = const $ const $ return ()
  , bwdBinopI32 = const $ const $ return ((), ())
  , bwdBinopI64 = const $ const $ return ((), ())

  , bwdUnopF32  = const $ const $ return ()
  , bwdUnopF64  = const $ const $ return ()
  , bwdBinopF32 = const $ const $ return ((), ())
  , bwdBinopF64 = const $ const $ return ((), ())

  , bwdTestopI32 = const $ const $ return ()
  , bwdTestopI64 = const $ const $ return ()

  , bwdRelopI32 = const $ const $ return ((), ())
  , bwdRelopI64 = const $ const $ return ((), ())
  , bwdRelopF32 = const $ const $ return ((), ())
  , bwdRelopF64 = const $ const $ return ((), ())

  , bwdSelectI32 = const $ return ((), (), ())
  , bwdSelectI64 = const $ return ((), (), ())
  , bwdSelectF32 = const $ return ((), (), ())
  , bwdSelectF64 = const $ return ((), (), ())

  , bwdExtend1I8toI32 = const $ const $ return ()
  , bwdExtend2I8toI32 = const $ const $ return ((),())
  , bwdExtend1I8toI64 = const $ const $ return ()
  , bwdExtend2I8toI64 = const $ const $ return ((),())
  , bwdExtend4I8toI64 = const $ const $ return ((),(),(),())

  , bwdExtractI32to1I8 = const $ return ()
  , bwdExtractI32to2I8 = const $ return ()

  , bwdExtractI64to1I8 = const $ return ()
  , bwdExtractI64to2I8 = const $ return ()
  , bwdExtractI64to4I8 = const $ return ()

  , bwdReinterpretI32to4I8 = const $ return ()
  , bwdReinterpretI64to8I8 = const $ return ()
  , bwdReinterpret4I8toI32 = const $ return ((),(),(),())
  , bwdReinterpret8I8toI64 = const $ return ((),(),(),(),(),(),(),())
  , bwdReinterpretI32toF32 = const $ return ()
  , bwdReinterpretI64toF64 = const $ return ()
  , bwdReinterpretF32toI32 = const $ return ()
  , bwdReinterpretF64toI64 = const $ return ()

  , bwdTruncF32toI32 = const $ const $ const $ return ()
  , bwdTruncF64toI32 = const $ const $ const $ return ()
  , bwdTruncF32toI64 = const $ const $ const $ return ()
  , bwdTruncF64toI64 = const $ const $ const $ return ()

  , bwdConvertF32toF64 = const $ return ()
  , bwdConvertF64toF32 = const $ return ()
  , bwdConvertI32toF32 = const $ const $ return ()
  , bwdConvertI32toF64 = const $ const $ return ()
  , bwdConvertI64toF32 = const $ const $ return ()
  , bwdConvertI64toF64 = const $ const $ return ()
  }
