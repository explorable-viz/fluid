module Eval where

import Prelude hiding (absurd, apply)
import Data.Either (Either(..), note)
import Data.List (List(..), (:), length, singleton, unzip)
import Data.Map (lookup, update)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Bindings (Bindings(..), (:+:), (↦), find, varAnon)
import DataType (Ctr, arity)
import Expl (Expl(..), VarDef(..)) as T
import Expl (Expl, Match(..))
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDef(..), RecDefs, body)
import Expr (RawExpr(..), VarDef(..)) as E
import Pretty (pretty, render)
import Primitive (apply)
import Util (MayFail, type (×), (×), absurd, check, error, report, successful)
import Val (Env, Val(..), val)
import Val (RawVal(..)) as V

match :: Val -> Elim -> MayFail (Env × Cont × Match)
match v (ElimVar x κ)
   | x == varAnon = pure $ Empty × κ × MatchVarAnon v
   | otherwise    = pure $ (Empty :+: x ↦ v) × κ × MatchVar x
match (Val _ (V.Constr c vs)) (ElimConstr κs) = do
   κ <- note ("Pattern mismatch: no branch for " <> show c) $ lookup c κs
   ρ × κ' × ξs <- matchArgs c vs κ
   pure $ ρ × κ' × (MatchConstr (c × ξs) $ update (const Nothing) c κs)
match v _ = report $ "Pattern mismatch: " <> render (pretty v) <> " is not a constructor value"

matchArgs :: Ctr -> List Val -> Cont -> MayFail (Env × Cont × (List Match))
matchArgs _ Nil κ                = pure $ Empty × κ × Nil
matchArgs c (v : vs) (Arg σ)     = do
   ρ  × κ'  × ξ  <- match v σ
   ρ' × κ'' × ξs <- matchArgs c vs κ'
   pure $ (ρ <> ρ') × κ'' × (ξ : ξs)
matchArgs c (_ : vs) (Body _)    = report $
   show (length vs + 1) <> " extra argument(s) to " <> show c <> "; did you forget parentheses in lambda pattern?"
matchArgs _ _ _                  = error absurd

-- Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
closeDefs :: Env -> RecDefs -> RecDefs -> Env
closeDefs _ _ Nil = Empty
closeDefs ρ δ0 (RecDef f σ : δ) = closeDefs ρ δ0 δ :+: f ↦ (val $ V.Closure ρ δ0 σ)

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) $ show c <> " got " <> show n <> " argument(s), expects at most " <> show n'

eval :: Env -> Expr -> MayFail (Expl × Val)
eval ρ (Expr _ (E.Var x)) =
   (T.Var x ρ × _) <$> find x ρ
eval ρ (Expr _ (E.Op op)) =
   (T.Op op ρ × _) <$> find op ρ
eval ρ (Expr _ (E.Int n)) =
   pure $ T.Int n ρ × val (V.Int n)
eval ρ (Expr _ (E.Str str)) =
   pure $ T.Str str ρ × val (V.Str str)
eval ρ (Expr _ (E.Constr c es)) = do
   checkArity c (length es)
   ts × vs <- traverse (eval ρ) es <#> unzip
   pure $ case es of Nil -> (T.NullConstr c ρ) × val (V.Constr c vs)
                     _   -> (T.Constr c ts) × val (V.Constr c vs)
eval ρ (Expr _ (E.LetRec δ e)) = do
   let ρ' = closeDefs ρ δ δ
   t × v <- eval (ρ <> ρ') e
   pure $ T.LetRec δ t × v
eval ρ (Expr _ (E.Lambda σ)) =
   pure $ T.Lambda σ × val (V.Closure ρ Nil σ)
eval ρ (Expr _ (E.App e e')) = do
   t  × v@(Val _ u) <- eval ρ e
   t' × v'          <- eval ρ e'
   case u of
      V.Closure ρ1 δ σ  -> do
         let ρ2 = closeDefs ρ1 δ δ
         ρ3 × e'' × ξ <- match v' σ
         t'' × v'' <- eval (ρ1 <> ρ2 <> ρ3) $ body e''
         pure $ T.App (t × v) t' ξ t'' × v''
      V.Primitive φ     -> pure $ T.AppOp (t × v) (t' × v') × apply φ v'
      V.Constr c vs     -> do
         check (successful (arity c) > length vs) $ "Too many arguments to " <> show c
         pure $ T.AppOp (t × v) (t' × v') × val (V.Constr c $ vs <> singleton v')
      _                 -> report "Expected closure, operator or unsaturated constructor"
eval ρ (Expr _ (E.BinaryApp e op e')) = do
   t  × v  <- eval ρ e
   t' × v' <- eval ρ e'
   Val _ u <- find op ρ
   case u of
      V.Primitive φ ->
         let Val _ u' = apply φ v in
         case u' of
            V.Primitive φ_v   -> pure $ T.BinaryApp (t × v) op (t' × v') × apply φ_v v'
            _                 -> report "Not a binary operator"
      _ -> report "Not an operator"
eval ρ (Expr _ (E.Let (E.VarDef σ e) e')) = do
   t  × v      <- eval ρ e
   ρ' × _ × ξ  <- match v σ
   t' × v'     <- eval (ρ <> ρ') e'
   pure $ T.Let (T.VarDef ξ t) t' × v'
eval ρ (Expr _ (E.MatchAs e σ)) = do
   t  × v      <- eval ρ e
   ρ' × e' × ξ <- match v σ
   t' × v'     <- eval (ρ <> ρ') (body e')
   pure $ T.MatchAs t ξ t' × v'

defs :: Env -> Module -> MayFail Env
defs ρ (Module Nil) = pure ρ
defs ρ (Module (Left (E.VarDef σ e) : ds)) = do
   _  × v      <- eval ρ e
   ρ' × _ × ξ  <- match v σ
   defs (ρ <> ρ') (Module ds)
defs ρ (Module (Right δ : ds)) =
   defs (ρ <> closeDefs ρ δ δ) (Module ds)
