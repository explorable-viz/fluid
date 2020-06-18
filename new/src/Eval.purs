module Eval where

import Prelude hiding (absurd, apply)
import Data.Either (Either(..))
import Data.List (List(..), (:), unzip)
import Data.Map (lookup, update)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Bindings (Bindings(..), (:+:), (↦), find)
import Expl (Def(..), Expl(..)) as T
import Expl (Expl, Match(..))
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDef(..), RecDefs, body)
import Expr (Def(..), RawExpr(..)) as E
import Pretty (pretty, render)
import Primitive (applyBinary, applyUnary)
import Util (MayFail, type (×), (×), absurd, error)
import Val (Env, UnaryOp(..), Val(..), val)
import Val (RawVal(..)) as V

match :: Val -> Elim -> MayFail (Env × Cont × Match)
match v (ElimVar x κ) = pure $ (Empty :+: x ↦ v) × κ × (MatchVar x)
match (Val _ (V.Constr c vs)) (ElimConstr κs) =
   case lookup c κs of
      Nothing  -> Left $ "Pattern mismatch: no branch for " <> show c
      Just κ   -> do
         ρ × κ' × ξs <- matchArgs vs κ
         pure $ ρ × κ' × (MatchConstr (c × ξs) $ update (const Nothing) c κs)
match v _ = Left $ "Pattern mismatch: " <> render (pretty v) <> " is not a value"

matchArgs :: List Val -> Cont -> MayFail (Env × Cont × (List Match))
matchArgs Nil κ = pure $ Empty × κ × Nil
matchArgs (v : vs) (Arg _ σ)  = do
   ρ  × κ'  × ξ  <- match v σ
   ρ' × κ'' × ξs <- matchArgs vs κ'
   pure $ (ρ <> ρ') × κ'' × (ξ : ξs)
matchArgs (_ : _) _ = Left $ "Too many arguments"

-- Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
closeDefs :: Env -> RecDefs -> RecDefs -> Env
closeDefs _ _ Nil = Empty
closeDefs ρ δ0 (RecDef f σ : δ) = closeDefs ρ δ0 δ :+: f ↦ (val $ V.Closure ρ δ0 σ)

eval :: Env -> Expr -> MayFail (Expl × Val)
eval ρ (Expr _ (E.Var x)) =
   (T.Var x × _) <$> find x ρ
eval ρ (Expr _ (E.Op op)) =
   (T.Op op × _) <$> find op ρ
eval ρ (Expr _ (E.Int n)) =
   pure $ T.Int n × val (V.Int n)
eval ρ (Expr _ (E.Str str)) =
   pure $ (T.Str str) × val (V.Str str)
eval ρ (Expr _ (E.Constr c es)) = do
   ts × vs <- traverse (eval ρ) es <#> unzip
   pure $ (T.Constr c ts) × val (V.Constr c vs)
eval ρ (Expr _ (E.LetRec δ e)) = do
   let ρ' = closeDefs ρ δ δ
   t × v <- eval (ρ <> ρ') e
   pure $ (T.LetRec δ t) × v
eval ρ (Expr _ (E.Lambda σ)) =
   pure $ (T.Lambda σ) × val (V.Closure ρ Nil σ)
eval ρ (Expr _ (E.App e e')) = do
   t  × (Val _ u) <- eval ρ e
   t' × v'        <- eval ρ e'
   case u of
      V.Closure ρ1 δ σ -> do
         let ρ2 = closeDefs ρ1 δ δ
         ρ3 × e'' × ξ <- match v' σ
         t'' × v'' <- eval (ρ1 <> ρ2 <> ρ3) $ body e''
         pure $ (T.App t t' ξ t'') × v''
      V.Unary φ ->
         pure $ (T.AppOp t t') × applyUnary φ v'
      V.Binary φ ->
         pure $ (T.AppOp t t') × val (V.Unary $ PartialApp φ v')
      _ -> Left "Expected closure or operator"
eval ρ (Expr _ (E.BinaryApp e op e')) = do
   t  × v  <- eval ρ e
   t' × v' <- eval ρ e'
   Val _ u <- find op ρ
   case u of
      V.Binary φ ->
         pure $ (T.BinaryApp t op t') × (v `applyBinary φ` v')
      _ -> error absurd
eval ρ (Expr _ (E.Let (E.Def σ e) e')) = do
   t  × v      <- eval ρ e
   ρ' × _ × ξ  <- match v σ
   t' × v'     <- eval (ρ <> ρ') e'
   pure $ (T.Let (T.Def ξ t) t') × v'
eval ρ (Expr _ (E.MatchAs e σ)) = do
   t  × v      <- eval ρ e
   ρ' × e' × ξ <- match v σ
   t' × v'     <- eval (ρ <> ρ') (body e')
   pure $ (T.MatchAs t ξ t') × v'

-- desugar :: Expr -> Expr
-- desugar (Expr _ (E.Let (E.Def σ e) e'))
--  = E.Lambda σ


defs :: Env -> Module -> MayFail Env
defs ρ (Module Nil) = pure ρ
defs ρ (Module (Left (E.Def σ e) : ds)) = do
   _  × v      <- eval ρ e
   ρ' × _ × ξ  <- match v σ
   defs (ρ <> ρ') (Module ds)
defs ρ (Module (Right δ : ds)) =
   defs (ρ <> closeDefs ρ δ δ) (Module ds)
