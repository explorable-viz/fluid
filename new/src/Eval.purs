module Eval where

import Prelude hiding (absurd, apply)
import Data.Either (Either(..))
import Data.List (List(..), (:), unzip)
import Data.Map (lookup, update)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Bindings ((:+:), (↦), ε, find)
import Expl (Def(..), Expl(..)) as T
import Expl (Expl, Match(..))
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDef(..), RecDefs, asExpr)
import Expr (Def(..), RawExpr(..)) as E
import Pretty (pretty, render)
import Primitive (applyBinary, applyUnary)
import Util (MayFail, T3(..), type (×), absurd, error)
import Val (Env, UnaryOp(..), Val(..), val)
import Val (RawVal(..)) as V

match2 :: Val -> Elim -> MayFail (T3 Env Cont Match)
match2 v (ElimVar x κ) = pure $ T3 (ε :+: x ↦ v) κ (MatchVar x)
match2 (Val _ (V.Constr c vs)) (ElimConstr κs) =
   case lookup c κs of
      Nothing -> Left $ "Constructor " <> show c <> " not found"
      Just κ -> do
         T3 ρ κ' ξs <- matchArgs vs κ
         T3 ρ κ' <$> pure (MatchConstr (Tuple c ξs) $ update (const Nothing) c κs)
match2 v _ = Left $ "Pattern mismatch for " <> render (pretty v)

matchArgs :: List Val -> Cont -> MayFail (T3 Env Cont (List Match))
matchArgs Nil κ               = pure $ T3 ε κ Nil
matchArgs (v : vs) (CElim σ)  = do
   T3 ρ κ' ξ <- match2 v σ
   T3 ρ' κ'' ξs <- matchArgs vs κ'
   pure $ T3 (ρ <> ρ') κ'' (ξ : ξs)
matchArgs (_ : _) _           = Left $ "Too many arguments"

-- Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
closeDefs :: Env -> RecDefs -> RecDefs -> Env
closeDefs _ _ Nil = ε
closeDefs ρ δ0 (RecDef f σ : δ) = closeDefs ρ δ0 δ :+: f ↦ (val $ V.Closure ρ δ0 σ)

eval :: Env -> Expr -> MayFail (Expl × Val)
eval ρ (Expr _ (E.Var x)) =
   Tuple (T.Var x) <$> find x ρ
eval ρ (Expr _ (E.Op op)) =
   Tuple (T.Op op) <$> find op ρ
eval ρ (Expr _ E.True) =
   pure $ Tuple T.True $ val V.True
eval ρ (Expr _ E.False) =
   pure $ Tuple T.False $ val V.False
eval ρ (Expr _ (E.Int n)) =
   pure $ Tuple (T.Int n) $ val $ V.Int n
eval ρ (Expr _ (E.Str str)) =
   pure $ Tuple (T.Str str) $ val $ V.Str str
eval ρ (Expr _ (E.Constr c es)) = do
   Tuple ts vs <- traverse (eval ρ) es <#> unzip
   pure $ Tuple (T.Constr c ts) $ val $ V.Constr c vs
eval ρ (Expr _ (E.Pair e e')) = do
   Tuple t v <- eval ρ e
   Tuple t' v' <- eval ρ e'
   pure $ Tuple (T.Pair t t') $ val $ V.Pair v v'
eval ρ (Expr _ E.Nil) =
   pure $ Tuple T.Nil $ val V.Nil
eval ρ (Expr _ (E.Cons e e')) = do
   Tuple t v <- eval ρ e
   Tuple t' v' <- eval ρ e'
   pure $ Tuple (T.Cons t t') $ val $ V.Cons v v'
eval ρ (Expr _ (E.LetRec δ e)) = do
   let ρ' = closeDefs ρ δ δ
   Tuple t v <- eval (ρ <> ρ') e
   pure $ Tuple (T.LetRec δ t) v
eval ρ (Expr _ (E.Lambda σ)) =
   pure $ Tuple (T.Lambda σ) $ val $ V.Closure ρ Nil σ
eval ρ (Expr _ (E.App e e')) = do
   Tuple t (Val _ u) <- eval ρ e
   Tuple t' v' <- eval ρ e'
   case u of
      V.Closure ρ1 δ σ -> do
         let ρ2 = closeDefs ρ1 δ δ
         T3 ρ3 e'' ξ <- match2 v' σ
         Tuple t'' v'' <- eval (ρ1 <> ρ2 <> ρ3) $ asExpr e''
         pure $ Tuple (T.App t t' ξ t'') v''
      V.Unary φ ->
         pure $ Tuple (T.AppOp t t') $ applyUnary φ v'
      V.Binary φ ->
         pure $ Tuple (T.AppOp t t') $ val $ V.Unary $ PartialApp φ v'
      _ -> Left "Expected closure or operator"
eval ρ (Expr _ (E.BinaryApp e op e')) = do
   Tuple t v <- eval ρ e
   Tuple t' v' <- eval ρ e'
   Val _ u <- find op ρ
   case u of
      V.Binary φ ->
         pure $ Tuple (T.BinaryApp t op t') (v `applyBinary φ` v')
      _ -> error absurd
eval ρ (Expr _ (E.Let (E.Def σ e) e')) = do
   Tuple t v <- eval ρ e
   T3 ρ' _ ξ <- match2 v σ
   Tuple t' v' <- eval (ρ <> ρ') e'
   pure $ Tuple (T.Let (T.Def ξ t) t') v'
eval ρ (Expr _ (E.MatchAs e σ)) = do
   Tuple t v <- eval ρ e
   T3 ρ' e' ξ <- match2 v σ
   Tuple t' v' <- eval (ρ <> ρ') (asExpr e')
   pure $ Tuple (T.MatchAs t ξ t') v'

defs :: Env -> Module -> MayFail Env
defs ρ (Module Nil) = pure ρ
defs ρ (Module (Left (E.Def σ e) : ds)) = do
   Tuple _ v <- eval ρ e
   T3 ρ' _ ξ <- match2 v σ
   defs (ρ <> ρ') (Module ds)
defs ρ (Module (Right δ : ds)) =
   defs (ρ <> closeDefs ρ δ δ) (Module ds)
