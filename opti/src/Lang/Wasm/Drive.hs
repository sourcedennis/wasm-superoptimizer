{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

-- | Driving algorithm
module Lang.Wasm.Drive
  ( drive
  ) where

import Melude
-- Stdlib imports
import           Control.Monad.Fail ( MonadFail )
-- Local imports
import qualified Lang.Wasm.Symbolic.Configuration as Conf
import           Lang.Wasm.Symbolic.Configuration.ExecSimple ( execSimpleInstr )
import           Lang.Wasm.Symbolic.Configuration ( Configuration, cfgStack )
import qualified Lang.Wasm.Solver as Sv
import           Lang.Wasm.Solver ( MonadSolver )
import qualified Lang.Wasm.Process as P
import           Lang.Wasm.Process
import qualified Lang.Wasm.Symbolics as Y
import           Lang.Wasm.Symbolics
  ( MonadSymbolics, Symbolic )
-- import           Lang.Wasm.Symbolic.ExecSimple ( execSimpleInstr )
import qualified Lang.Wasm.Data as WD
import qualified Lang.Wasm.Ast as Ast
import           Lang.Wasm.Ast
  ( InstrCtx, ResultType, SimpleInstr, PVal (..), TI32, TI64, WI32 (..)
  , SimpleInstr (..), PrmInstr (..), ValType (..)
  )


type DepthBound = Int

drive :: forall m c a
      .  MonadSolver m
      => InstrCtx
      -> ResultType
      -> DepthBound
      -> Configuration c
      -> GraphFrozen a
      -> m (Maybe (Tree (Maybe (Maybe NodeId, Configuration c))))
drive sfc rt bound c g =
  do
    let stack = []
    let tree = buildTree sfc stack rt g
    t <- runMaybeT (driveTree sfc bound c tree)
    case t of
      Nothing -> return Nothing
      Just t' -> return $ resolveDrop sfc t'

driveTree :: MonadSolver m
          => InstrCtx
          -> DepthBound
          -> Configuration c
          -> Tree (Maybe NodeId)
          -> MaybeT m (Tree (Maybe NodeId, Configuration c))
driveTree sfc 0 c _ = fail "Driving bound exceeded"
driveTree sfc bound c (Node i edge) =
  Node (i, c) <$> driveEdge sfc bound c i edge
driveTree sfc bound c (NodeNaturalEnd i) =
  return $ NodeNaturalEnd (i, c)
driveTree sfc bound c (NodeReturn i) =
  return $ NodeReturn (i, c)
driveTree sfc bound c (NodeTrapped i) =
  return $ NodeTrapped (i, c)
driveTree sfc bound c (NodeTerminal i) =
  return $ NodeTerminal (i, c)
driveTree sfc bound c NodeStaticError =
  fail "Static error"

driveEdge :: forall m c
          .  MonadSolver m
          => InstrCtx
          -> DepthBound
          -> Configuration c
          -> Maybe NodeId
          -> TreeEdge (Maybe NodeId)
          -> MaybeT m (TreeEdge (Maybe NodeId, Configuration c))
driveEdge sfc bound c i (PteInstr instr nextT) =
  do
    isDone <- Sv.isEmpty c
    -- trace (show instr ++ " " ++ show isDone) $ return ()

    Just c' <- return $ execSimpleInstr c instr

    case WD.peekStackVal (c' ^. cfgStack) of
      Just (VI32 v) ->
        do
          res <- Sv.evalI32 (Conf.environment c') v
          case res of
            Sv.EvSat vVal ->
              do
                Just (_, cDrop) <- return $ Conf.popI32 c'
                let (symVal, cDrop') = runSymbolics (Y.vI32Const vVal) cDrop :: (Y.Symbolic c TI32, Configuration c)
                    c'' = Conf.pushI32 symVal cDrop' -- identical to c' (maybe smaller formula)
                afterT <- driveTree sfc (bound-1) c'' nextT
                let cleanT = Node (i, cDrop) $ PteInstr (SConstI32 vVal) afterT
                    dropT  = Node (Nothing, c'') $ PteInstr (SPrmInstrI32 PDrop) cleanT
                return $ PteInstr instr dropT
            _ -> PteInstr instr <$> driveTree sfc (bound-1) c' nextT
      Just (VI64 v) ->
        do
          res <- Sv.evalI64 (Conf.environment c') v
          case res of
            Sv.EvSat vVal ->
              do
                Just (_, cDrop) <- return $ Conf.popI64 c'
                let (symVal, cDrop') = runSymbolics (Y.vI64Const vVal) cDrop :: (Y.Symbolic c TI64, Configuration c)
                    c'' = Conf.pushI64 symVal cDrop' -- identical to c' (maybe smaller formula)
                afterT <- driveTree sfc (bound-1) c'' nextT
                let cleanT = Node (i, cDrop) $ PteInstr (SConstI64 vVal) afterT
                    dropT  = Node (Nothing, c'') $ PteInstr (SPrmInstrI64 PDrop) cleanT
                return $ PteInstr instr dropT
            _ -> PteInstr instr <$> driveTree sfc (bound-1) c' nextT
      _ -> PteInstr instr <$> driveTree sfc (bound-1) c' nextT
driveEdge sfc bound c i (PteIf ft ifT elseT) =
  do
    Just (ifCond, c') <- return $ Conf.popI32 c

    -- Don't enter a label, as all values continue forward anyway
    let ifC = assertConstraint (Y.vI32Ne ifCond =<< Y.vI32Const (WI32 0)) c'
    let elseC = assertConstraint (Y.vI32Eqz ifCond) c'

    isTrue <- Sv.isEmpty elseC
    isFalse <- Sv.isEmpty ifC

    -- trace ("if' " ++ show isTrue ++ " " ++ show isFalse) $ return ()

    if isTrue then
      PteInstr (SPrmInstrI32 PDrop) <$> driveTree sfc (bound-1) ifC ifT
    else if isFalse then
      PteInstr (SPrmInstrI32 PDrop) <$> driveTree sfc (bound-1) elseC elseT
    else
      PteIf ft <$> driveTree sfc (bound-1) ifC ifT <*> driveTree sfc (bound-1) elseC elseT
driveEdge _ _ _ _ _ = fail "Other edges not implemented"
-- driveEdge sfc bound c (PeTable kv xs x) =
--   do
--     -- trace "table" $ return ()

--     Just (lblIdx, c') <- return $ popI32 c

--     res <- lift $ Sv.evalI32 (Conf.environment c') lblIdx

--     case res of
--       Sv.EvSat (WI32 lblIdxVal) ->
--         case atIndex lblIdxVal xs of
--           Just (dv, nextI) ->
--             let c'' = assertConstraint (Y.vI32Eq lblIdx =<< Y.vI32Const (WI32 lblIdxVal)) c'
--             in PeInstr (SPrmInstrI32 PDrop) <$> driveTreeDk sfc (bound-1) c'' (P.newDropKeep kv dv) nextI
--           Nothing ->
--             let (dv, nextI) = x
--                 c'' = assertConstraint (Y.vI32Ge Ast.U lblIdx =<< Y.vI32Const (WI32 $ toNum $ length xs)) c'
--             in PeInstr (SPrmInstrI32 PDrop) <$> driveTreeDk sfc (bound-1) c'' (P.newDropKeep kv dv) nextI
--       _ ->
--         do
--           xs' <- zipWithM (\lblIdxVal (dv, nextI) ->
--                    let c'' = assertConstraint (Y.vI32Eq lblIdx =<< Y.vI32Const (WI32 lblIdxVal)) c'
--                    in (dv,) <$> driveTreeDk sfc (bound-1) c'' (P.newDropKeep kv dv) nextI
--                  ) [0..] xs
--           let (dv, nextI) = x
--               c'' = assertConstraint (Y.vI32Ge Ast.U lblIdx =<< Y.vI32Const (WI32 $ toNum $ length xs)) c'
--           x' <- driveTreeDk sfc (bound-1) c'' (P.newDropKeep kv dv) nextI
--           return $ PeTable kv xs' (dv, x')
-- driveEdge sfc bound c (PeCall fIdx nextI) =
--   do
--     -- trace "call" $ return ()

--     ft <- Ast.itxFuncType sfc fIdx
--     Just c' <- return $ applyFuncType ft c
--     PeCall fIdx <$> driveTree sfc (bound-1) c' nextI
-- driveEdge sfc bound c (PeCallIndirect ft nextI) =
--   do
--     -- trace "call indirect" $ return ()

--     Just (_, c') <- return $ popI32 c
--     Just c'' <- return $ applyFuncType ft c'
--     PeCallIndirect ft <$> driveTree sfc (bound-1) c'' nextI

assertConstraint :: ( forall m . MonadSymbolics env m => m (Y.Symbolic env Bool) ) -> Configuration env -> Configuration env
assertConstraint b c =
  let (a, c') = runSymbolics (Y.vBAnd (c ^. Conf.cfgConstraint) =<< b) c
  in c' & Conf.cfgConstraint .~ a

runSymbolics :: ( forall m . MonadSymbolics env m => m a ) -> Configuration env -> (a, Configuration env)
runSymbolics f c =
  let (a, sym) = Y.runSymbolicsState f (c ^. Conf.cfgSymbolics)
  in (a, c & Conf.cfgSymbolics .~ sym)

runSymbolicsMaybe :: ( forall m . MonadSymbolics env m => MaybeT m a ) -> Configuration env -> Maybe (a, Configuration env)
runSymbolicsMaybe f c =
  case Y.runSymbolicsState (runMaybeT f) (c ^. Conf.cfgSymbolics) of
    (Nothing, sym) -> Nothing
    (Just a, sym)  -> Just (a, c & Conf.cfgSymbolics .~ sym)

-- -- | Applies the `DropKeep` before driving the tree.
-- driveTreeDk :: MonadSolver m
--             => InstrCtx
--             -> DepthBound
--             -> Configuration c
--             -> DropKeep
--             -> Tree SingleId
--             -> MaybeT m (Tree (SingleId, Configuration c))
-- driveTreeDk sfc bound c dk t =
--   case applyDropKeep c dk of
--     Just c'' -> driveTree sfc bound c'' t
--     Nothing  -> fail "Statically invalid program"

-- applyDropKeep :: Configuration env -> DropKeep -> Maybe (Configuration env)
-- applyDropKeep c dk =
--   c & cfgStack (P.applyDropKeep dk)

-- applyFuncType :: Ast.FuncType -> Configuration env -> Maybe (Configuration env)
-- applyFuncType ft conf =
--   do
--     (stack, conf') <- runSymbolicsMaybe (applyFuncType' ft (conf ^. cfgStack)) conf
--     return $ conf' & cfgStack .~ stack
--   where
--   applyFuncType' :: MonadSymbolics env m => Ast.FuncType -> [Y.SymbolicVal env] -> MaybeT m [Y.SymbolicVal env]
--   applyFuncType' (Ast.FuncType xs ys) zs =
--     do
--       zs' <- dropPrefix (map P.isEqType (reverse $ Vector.toList xs)) zs
--       outs <- mapM Y.vUnknown (reverse $ Vector.toList ys)
--       return (outs ++ zs')

type NumDropped = Int

resolveDrop :: MonadFail m => InstrCtx -> Tree a -> m (Tree (Maybe a))
-- resolveDrop _ t = t
resolveDrop ctx = fmap snd . resolveDrop'
  where
  resolveDrop' :: MonadFail m => Tree a -> m ([ValType], Tree (Maybe a))
  resolveDrop' (NodeNaturalEnd a) = return ([], NodeNaturalEnd $ Just a)
  resolveDrop' (NodeReturn a)     = return ([], NodeReturn $ Just a)
  resolveDrop' (NodeTrapped a)    = return ([], NodeTrapped $ Just a)
  resolveDrop' (NodeTerminal a)   = return ([], NodeTerminal $ Just a)
  resolveDrop' NodeStaticError    = return ([], NodeStaticError)
  resolveDrop' (Node a (PteInstr x t))
    | affectsState x = 
        do
          t' <- dropExplicit <$> resolveDrop' t
          return ([], Node (Just a) (PteInstr x t'))
    | otherwise =
        do
          res@(dropped, t') <- resolveDrop' t
          (xs, ys) <- Ast.simpleInstrEffect ctx x
          case dropEqPrefix (reverse ys) dropped of
            Just dropped' -> -- The node can be eliminated
              return (reverse xs ++ dropped', t')
            Nothing ->
              return ([], Node (Just a) $ PteInstr x $ dropExplicit res)
  resolveDrop' (Node a (PteIf ft ifT elseT)) =
    do
      resI <- resolveDrop' ifT
      resE <- resolveDrop' elseT
      return ([], Node (Just a) (PteIf ft (dropExplicit resI) (dropExplicit resE)))
  -- resolveDrop' ctx (Node a (PeTable kv xs (dv, t))) =
  --   do
  --     xs' <- mapM (\(dv, t) -> (dv,) . dropExplicit <$> resolveDrop' ctx t) xs
  --     x' <- (dv,) . dropExplicit <$> resolveDrop' ctx t
  --     return ([], TreeNode (Just a) (PeTable kv xs' x'))
  resolveDrop' (Node a (PteCall fIdx t)) =
    do
      t' <- dropExplicit <$> resolveDrop' t
      return ([], Node (Just a) (PteCall fIdx t'))
  resolveDrop' (Node a (PteCallIndirect ft t)) =
    do
      t' <- dropExplicit <$> resolveDrop' t
      return ([], Node (Just a) (PteCallIndirect ft t'))
  -- resolveDrop' ctx (Node a (PteDropMany kv dv t)) =
  --   do
  --     t' <- dropExplicit <$> resolveDrop' ctx t
  --     return ([], TreeNode (Just a) (PeDropMany kv dv t'))
  
  dropExplicit :: ([ValType], Tree (Maybe a)) -> Tree (Maybe a)
  dropExplicit ([], t) = t
  dropExplicit (TI32:xs, t) = Node Nothing $ PteInstr (SPrmInstrI32 PDrop) (dropExplicit (xs, t))
  dropExplicit (TI64:xs, t) = Node Nothing $ PteInstr (SPrmInstrI64 PDrop) (dropExplicit (xs, t))
  dropExplicit (TF32:xs, t) = Node Nothing $ PteInstr (SPrmInstrF32 PDrop) (dropExplicit (xs, t))
  dropExplicit (TF64:xs, t) = Node Nothing $ PteInstr (SPrmInstrF64 PDrop) (dropExplicit (xs, t))

affectsState :: SimpleInstr -> Bool
affectsState (SVarInstr (Ast.VLocalSet _))  = True
affectsState (SVarInstr (Ast.VLocalTee _))  = True
affectsState (SVarInstr (Ast.VGlobalGet _)) = True
affectsState (SMemInstr _) = True
affectsState _ = False
