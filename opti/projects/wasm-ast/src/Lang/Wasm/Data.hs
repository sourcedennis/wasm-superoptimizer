{-# LANGUAGE DeriveGeneric #-}

-- | WebAssembly parametric state representation structures.
--
-- These data structures are parametric over the value types, which enables
-- different executions or analyses to be implemented over these same
-- structures (e.g., concrete execution, symbolic execution, liveness analysis,
-- constant propagation, ...). These different techniques usually use different
-- (abstract) representations for values.
--
-- (Not really part of the official AST, though)
module Lang.Wasm.Data
  ( -- ** Data Structures
    PScopeStack
  , PStack
  , PActivation (..)
  , PGlobal (..)
  , PGlobals (..)
    -- ** Values
  , val
  , valType
  , global
    -- ** Stack Functions
  , pushStack
  , pushStackI32
  , pushStackI64
  , pushStackF32
  , pushStackF64
  , pushStackLabel
  , enterStackLabel
  , leaveStackLabel
  , popStackVal
  , popStackI32
  , popStackI64
  , popStackF32
  , popStackF64
  , popStackLabel
  , peekStackVal
  , stackTypes
  , mapStackVal
  , zipStackVal
    -- ** Activation
  , getLocal
  , setLocal
  , mapActivationVal
  , zipActivationVal
    -- ** Globals
  , pGlobalType
  , getGlobal
  , setGlobal
  , setGlobalForce
  , mapGlobalVal
  , mapGlobalValM
  , zipGlobalVal
  ) where

import Melude
-- Stdlib imports
import           GHC.Generics ( Generic )
import           Control.DeepSeq ( NFData )
import           Control.Monad.Fail ( MonadFail )
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)) )
-- Extra stdlib imports
import qualified Data.Vector as Vector
import           Data.Vector ( Vector )
-- External library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )
-- Local imports
import           Lang.Wasm.Ast.Indices ( LocalIdx (..), GlobalIdx (..) )
import           Lang.Wasm.Ast.Types ( ValType (..), ResultType, Mut (..), GlobalType (..) )
import           Lang.Wasm.Ast.Values ( PVal (..), _VI32, _VI64, _VF32, _VF64 )


-- # Data Structures #

-- | The stack within a single block scope
type PScopeStack i32 i64 f32 f64 = [PVal i32 i64 f32 f64]

-- | The stack within a single function activation
type PStack i32 i64 f32 f64 = NonEmpty (PScopeStack i32 i64 f32 f64)

-- | A stack frame, containing parameters and locals.
--
-- As every stack lives within the context of a single function, the stack frame
-- need not actually live on the stack. Previously, the activation lived on the
-- stack, which took a toll on performance. (Every time a local is accessed, the
-- stack is traversed)
--
-- For most purposes, the params need not be distinguished from the locals. The
-- difference between those is that - upon function entry - params are assigned
-- externally-determined values, while locals are initialised to 0.
data PActivation i32 i64 f32 f64 =
  PActivation {
    aParams  :: !(IdList (PVal i32 i64 f32 f64))
  , aLocals  :: !(IdList (PVal i32 i64 f32 f64))
  }
  deriving (Eq, Show, Generic)

-- | A parametric global variable.
--
-- The mutability of globals varies, but is statically determined. For
-- convenience, this mutability is explicitly stored. (Particularly, this means
-- @set_global@ instructions have to be synthesized for immutable variables)
data PGlobal i32 i64 f32 f64 =
  PGlobal {
    gMut  :: !Mut
  , gVal  :: !(PVal i32 i64 f32 f64)
  }
  deriving (Eq, Show, Generic)

type PGlobals i32 i64 f32 f64 = IdList (PGlobal i32 i64 f32 f64)

mapStackVal :: Monad m
            => ( PVal a b c d -> m (PVal e f g h) )
            -> PStack a b c d
            -> m (PStack e f g h)
mapStackVal = mapM . mapM

mapActivationVal :: Monad m
                 => ( PVal a b c d -> m (PVal e f g h) )
                 -> PActivation a b c d
                 -> m (PActivation e f g h)
mapActivationVal f a =
  PActivation <$> mapM f (aParams a) <*> mapM f (aLocals a)

mapGlobalVal :: ( PVal a b c d -> PVal e f g h )
             -> PGlobal a b c d
             -> PGlobal e f g h
mapGlobalVal f g = PGlobal (gMut g) $ f (gVal g)

mapGlobalValM :: Monad m
              => ( PVal a b c d -> m (PVal e f g h) )
              -> PGlobal a b c d
              -> m (PGlobal e f g h)
mapGlobalValM f g = PGlobal (gMut g) <$> f (gVal g)

zipStackVal :: MonadFail m
            => ( PVal a b c d -> PVal e f g h -> m (PVal i j k l) )
            -> PStack a b c d
            -> PStack e f g h
            -> m (PStack i j k l)
zipStackVal = zipWithExactNEM . zipWithExactM

zipActivationVal :: MonadFail m
                 => ( PVal a b c d -> PVal e f g h -> m (PVal i j k l) )
                 -> PActivation a b c d
                 -> PActivation e f g h
                 -> m (PActivation i j k l)
zipActivationVal f a b =
  PActivation
    <$> IdList.zipWithExactM f (aParams a) (aParams b)
    <*> IdList.zipWithExactM f (aLocals a) (aLocals b)

zipGlobalVal :: MonadFail m
             => ( PVal a b c d -> PVal e f g h -> m (PVal i j k l) )
             -> PGlobal a b c d
             -> PGlobal e f g h
             -> m (PGlobal i j k l)
zipGlobalVal f a b =
  do
    failIf (gMut a /= gMut b) "Mismatching global mutability"
    PGlobal (gMut a) <$> f (gVal a) (gVal b)


-- # Stack #

pushStack :: PVal i32 i64 f32 f64 -> PStack i32 i64 f32 f64 -> PStack i32 i64 f32 f64
pushStack x (xs:|xss) = (x:xs):|xss

pushStackI32 :: i32 -> PStack i32 i64 f32 f64 -> PStack i32 i64 f32 f64
pushStackI32 = pushStack . VI32

pushStackI64 :: i64 -> PStack i32 i64 f32 f64 -> PStack i32 i64 f32 f64
pushStackI64 = pushStack . VI64

pushStackF32 :: f32 -> PStack i32 i64 f32 f64 -> PStack i32 i64 f32 f64
pushStackF32 = pushStack . VF32

pushStackF64 :: f64 -> PStack i32 i64 f32 f64 -> PStack i32 i64 f32 f64
pushStackF64 = pushStack . VF64

pushStackLabel :: PScopeStack i32 i64 f32 f64 -> PStack i32 i64 f32 f64 -> PStack i32 i64 f32 f64
pushStackLabel labelStack (x:|xs) = labelStack :| (x:xs)

-- |
-- >>> pushApplyStackLabel (Vector.fromList [TI32,TF32]) ([VF32 (), VI32 (),VI64 ()]:|[]) :: Maybe (Stack () () () ())
-- Just ([VF32 (),VI32 ()] :| [[VI64 ()]])
enterStackLabel :: MonadFail m
                => ResultType
                -> PStack i32 i64 f32 f64
                -> m (PStack i32 i64 f32 f64)
enterStackLabel rt (x:|xs) =
  do
    (z,x') <- splitPrefix (map (\v y -> v == valType y) (reverse $ Vector.toList rt)) x
    return (z:|(x':xs))

leaveStackLabel :: MonadFail m
                => ResultType
                -> PStack i32 i64 f32 f64
                -> m (PStack i32 i64 f32 f64)
leaveStackLabel rt (_:|[]) = fail "No label to pop"
leaveStackLabel rt (x:|(y:ys)) =
  if map valType x == reverse (Vector.toList rt) then
    return ((x++y):|ys)
  else
    fail "Invalid scope stack"

popStackVal :: MonadFail m
            => PStack i32 i64 f32 f64
            -> m (PVal i32 i64 f32 f64, PStack i32 i64 f32 f64)
popStackVal ((x:xs):|ys) = return (x, xs:|ys)
popStackVal ([]:|ys)     = fail "Empty scope stack"

popStackI32 :: MonadFail m => PStack i32 i64 f32 f64 -> m (i32, PStack i32 i64 f32 f64)
popStackI32 = popStackVal >=> mapFstM (failMaybeMsg "No I32 on stack" . preview _VI32)

popStackI64 :: MonadFail m => PStack i32 i64 f32 f64 -> m (i64, PStack i32 i64 f32 f64)
popStackI64 = popStackVal >=> mapFstM (failMaybeMsg "No I64 on stack" . preview _VI64)

popStackF32 :: MonadFail m => PStack i32 i64 f32 f64 -> m (f32, PStack i32 i64 f32 f64)
popStackF32 = popStackVal >=> mapFstM (failMaybeMsg "No F32 on stack" . preview _VF32)

popStackF64 :: MonadFail m => PStack i32 i64 f32 f64 -> m (f64, PStack i32 i64 f32 f64)
popStackF64 = popStackVal >=> mapFstM (failMaybeMsg "No F64 on stack" . preview _VF64)

popStackLabel :: MonadFail m => PStack i32 i64 f32 f64 -> m (PStack i32 i64 f32 f64)
popStackLabel ([]:|(x:xs)) = return (x:|xs)
popStackLabel (_:|_)       = fail "No label on stack"

peekStackVal :: MonadFail m
             => PStack i32 i64 f32 f64
             -> m (PVal i32 i64 f32 f64)
peekStackVal ((v:_):|_) = return v
peekStackVal ([]:|_)    = fail "Empty scope stack"

stackTypes :: PStack i32 i64 f32 f64 -> NonEmpty [ValType]
stackTypes = NE.map (map valType)


-- # Activation #

getLocal :: MonadFail m
         => LocalIdx
         -> PActivation i32 i64 f32 f64
         -> m (PVal i32 i64 f32 f64)
getLocal (LocalIdx i) a =
  let numParams = IdList.size (aParams a)
  in
  if i < numParams then
    failMaybeMsg "Missing param" $ IdList.lookup i (aParams a)
  else
    failMaybeMsg "Missing local" $ IdList.lookup ( i - numParams ) (aLocals a)

setLocal :: MonadFail m
         => LocalIdx
         -> PVal i32 i64 f32 f64
         -> PActivation i32 i64 f32 f64
         -> m (PActivation i32 i64 f32 f64)
setLocal (LocalIdx i) v a =
  let numParams = IdList.size (aParams a)
  in
  if i < numParams then
    do
      aParams' <- failMaybeMsg "Type mismatch" $ IdList.maybeUpdate (replace v) i (aParams a)
      return $ a { aParams = aParams' }
  else
    do
      aLocals' <- failMaybeMsg "Type mismatch" $ IdList.maybeUpdate (replace v) (i - numParams) (aLocals a)
      return $ a { aLocals = aLocals' }
  where
  replace :: PVal i32a i64a f32a f64a
          -> PVal i32b i64b f32b f64b
          -> Maybe (PVal i32a i64a f32a f64a)
  replace x y = toMaybe (valType x == valType y) x


-- # Globals #

pGlobalType :: PGlobal i32 i64 f32 f64 -> GlobalType
pGlobalType pg = GlobalType (gMut pg) (valType $ gVal pg)

getGlobal :: MonadFail m
          => GlobalIdx
          -> PGlobals i32 i64 f32 f64
          -> m (PVal i32 i64 f32 f64)
getGlobal (GlobalIdx i) =
  failMaybeMsg "Missing Global" . fmap gVal . IdList.lookup i

setGlobal :: MonadFail m
          => GlobalIdx
          -> PVal i32 i64 f32 f64
          -> PGlobals i32 i64 f32 f64
          -> m (PGlobals i32 i64 f32 f64)
setGlobal (GlobalIdx i) v =
  failMaybeMsg "Type mismatch" . IdList.maybeUpdate (replace v) i
  where
  replace :: PVal i32a i64a f32a f64a
          -> PGlobal i32b i64b f32b f64b
          -> Maybe (PGlobal i32a i64a f32a f64a)
  replace x y =
    toMaybe
      (gMut y == MVar && valType x == valType (gVal y))
      (PGlobal (gMut y) x)

-- | Set the global without checking for mutability.
--
-- This is necessary for some analyses (e.g. liveness), as those may "update"
-- the liveness state of an immutable global.
setGlobalForce :: MonadFail m
               => GlobalIdx
               -> PVal i32 i64 f32 f64
               -> PGlobals i32 i64 f32 f64
               -> m (PGlobals i32 i64 f32 f64)
setGlobalForce (GlobalIdx i) v =
  failMaybeMsg "Type mismatch" . IdList.maybeUpdate (replace v) i
  where
  replace :: PVal i32a i64a f32a f64a
          -> PGlobal i32b i64b f32b f64b
          -> Maybe (PGlobal i32a i64a f32a f64a)
  replace x y = Just (PGlobal (gMut y) x)

-- # Helpers #

val :: a -> b -> c -> d -> ValType -> PVal a b c d
val x _ _ _ TI32 = VI32 x
val _ x _ _ TI64 = VI64 x
val _ _ x _ TF32 = VF32 x
val _ _ _ x TF64 = VF64 x

global :: a -> b -> c -> d -> GlobalType -> PGlobal a b c d
global a b c d gt = PGlobal (gtMut gt) (val a b c d (gtType gt))

valType :: PVal a b c d -> ValType
valType (VI32 _) = TI32
valType (VI64 _) = TI64
valType (VF32 _) = TF32
valType (VF64 _) = TF64


-- # Instances #

instance (NFData i32, NFData i64, NFData f32, NFData f64) =>
           NFData (PActivation i32 i64 f32 f64)

instance (NFData i32, NFData i64, NFData f32, NFData f64) =>
           NFData (PGlobal i32 i64 f32 f64)

instance (Hashable i32, Hashable i64, Hashable f32, Hashable f64) =>
           Hashable (PActivation i32 i64 f32 f64)

instance (Hashable i32, Hashable i64, Hashable f32, Hashable f64) =>
           Hashable (PGlobal i32 i64 f32 f64)
