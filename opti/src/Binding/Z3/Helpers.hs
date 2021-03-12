{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module Binding.Z3.Helpers where

import Melude
-- Stdlib imports
import qualified Data.Map as Map
import           Data.Map ( Map )
-- External library imports
import qualified Z3.Monad as Z3
import           Z3.Monad ( MonadZ3 )


-- | Evaluates an array and extracts its interpretation from a Z3 model.
--
-- Extracting arrays from Z3 is quite tricky, as it cannot be done directly.
-- Instead, the array interpretation's internal structure has to be manually
-- traversed and collected.
--
-- Note that this may /not/ work for all Z3 array representations. This works
-- for a Z3_OP_CONST_ARRAY wrapped in several Z3_OP_STORE nodes.
--
-- This implementation is based on the StackOverflow answer (C++ Z3):
-- https://stackoverflow.com/questions/40903505/extract-value-from-const-array-in-z3
evalArr :: forall z3 k v . (MonadZ3 z3, Ord k, Eq v)
        => Z3.EvalAst z3 k
        -> Z3.EvalAst z3 v
        -> Z3.EvalAst z3 (v, Map k v)
evalArr fEvalKey fEvalVal m arr =
  maybe (return Nothing) evalAstArr =<< Z3.eval m arr
  where
  evalAstArr :: Z3.AST -> z3 (Maybe (v, Map k v))
  evalAstArr arr =
    do
      args <- Z3.getAppArgs =<< Z3.toApp arr

      case args of
        [arr2, addr, val] -> -- Assume it's a Z3_OP_STORE
          do
            mAddr <- fEvalKey m addr
            mVal  <- fEvalVal m val

            case (,) <$> mAddr <*> mVal of
              Just (addr', val') ->
                do
                  mRes <- evalAstArr arr2
                  return $ mRes >>= \(def, m) ->
                    if val' == def then
                      return (def, Map.delete addr' m)
                    else
                      return (def, Map.insert addr' val' m)
              Nothing -> return Nothing
        [def] -> -- Done unwrapping. Z3_OP_CONST_ARRAY reached
          do
            defVal <- fEvalVal m def
            return ( (Just . (,Map.empty)) =<< defVal )

-- | Produces the Z3 AST for the /not equal/ operation. Both input ASTs must be
-- of the same type (otherwise Z3 crashes).
mkNe :: MonadZ3 z3 => Z3.AST -> Z3.AST -> z3 Z3.AST
mkNe = Z3.mkNot <<=< Z3.mkEq
