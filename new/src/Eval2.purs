module Eval2 where

import Prelude hiding (absurd, apply)
import Data.Either (Either(..), note)
import Data.List (List(..), (:), length, singleton, unzip)
import Data.Map (lookup, update)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Bindings (varAnon)
import DataType (Ctr, arity)
import Expl2 (Expl, Expl'(..), VarDef(..)) as T
import Expl2 (Expl, Match, Match'(..))
import Expr (Cont2, Cont2'(..), Elim2, Elim2'(..), Expr2, Expr2'(..), Module2(..), RecDef2(..), RecDefs2, body2)
import Expr (RawExpr2(..), VarDef(..)) as E
import Pretty2 (pretty, render)
import Primitive (apply)
import Util (MayFail, type (×), (×), absurd, check, error, report, successful)
import Val (Env2, Env2'(..), Val2, Val2'(..), (:+:), (↦), find, val2)
import Val (RawVal2'(..)) as V

match :: Val2 -> Elim2 -> MayFail (Env2 × Cont2 × Match)
match v (ElimVar2 x κ)
   | x == varAnon = pure $ Empty2 × κ × MatchVarAnon v
   | otherwise    = pure $ (Empty2 :+: x ↦ Just v) × κ × MatchVar x
match (Val2' _ (V.Constr2 c vs)) (ElimConstr2 κs) = do
   κ <- note ("Pattern mismatch: no branch for " <> show c) $ lookup c κs
   ρ × κ' × ξs <- matchArgs c vs κ
   pure $ ρ × κ' × (MatchConstr (c × ξs) $ update (const Nothing) c κs)
match v _ = report $ "Pattern mismatch: " <> render (pretty v) <> " is not a constructor value"

matchArgs :: Ctr -> List Val2 -> Cont2 -> MayFail (Env2 × Cont2 × (List Match))
matchArgs _ Nil κ                = pure $ Empty2 × κ × Nil
matchArgs c (v : vs) (Arg2 σ)     = do
   ρ  × κ'  × ξ  <- match v σ
   ρ' × κ'' × ξs <- matchArgs c vs κ'
   pure $ (ρ <> ρ') × κ'' × (ξ : ξs)
matchArgs c (_ : vs) (Body2 _)    = report $
   show (length vs + 1) <> " extra argument(s) to " <> show c <> "; did you forget parentheses in lambda pattern?"
matchArgs _ _ _                  = error absurd

-- Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
closeDefs :: Env2 -> RecDefs2 -> RecDefs2 -> Env2
closeDefs _ _ Nil = Empty2
closeDefs ρ δ0 (RecDef2 f σ : δ) = closeDefs ρ δ0 δ :+: f ↦ Just (val2 $ V.Closure2 ρ δ0 σ)

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) $ show c <> " got " <> show n <> " argument(s), expects at most " <> show n'

eval :: Env2 -> Expr2 -> MayFail (Expl × Val2)
eval ρ (Expr2' _ (E.Var2 x)) =
   (T.Var x ρ × _) <$> find x ρ
eval ρ (Expr2' _ (E.Op2 op)) =
   (T.Op op ρ × _) <$> find op ρ
eval ρ (Expr2' _ (E.Int2 n)) =
   pure $ T.Int n ρ × val2 (V.Int2 n)
eval ρ (Expr2' _ (E.Str2 str)) =
   pure $ T.Str str ρ × val2 (V.Str2 str)
eval ρ (Expr2' _ (E.Constr2 c es)) = do
   checkArity c (length es)
   ts × vs <- traverse (eval ρ) es <#> unzip
   pure $ case es of Nil -> (T.NullConstr c ρ) × val2 (V.Constr2 c vs)
                     _   -> (T.Constr c ts) × val2 (V.Constr2 c vs)
eval ρ (Expr2' _ (E.LetRec2 δ e)) = do
   let ρ' = closeDefs ρ δ δ
   t × v <- eval (ρ <> ρ') e
   pure $ T.LetRec δ t × v
eval ρ (Expr2' _ (E.Lambda2 σ)) =
   pure $ T.Lambda σ × val2 (V.Closure2 ρ Nil σ)
eval ρ (Expr2' _ (E.App2 e e')) = do
   t  × v@(Val2' _ u) <- eval ρ e
   t' × v'          <- eval ρ e'
   case u of
      V.Closure2 ρ1 δ σ  -> do
         let ρ2 = closeDefs ρ1 δ δ
         ρ3 × e'' × ξ <- match v' σ
         t'' × v'' <- eval (ρ1 <> ρ2 <> ρ3) $ body2 e''
         pure $ T.App (t × v) t' ξ t'' × v''
      V.Primitive2 φ     -> pure $ T.AppOp (t × v) (t' × v') × apply φ v'
      V.Constr2 c vs     -> do
         check (successful (arity c) > length vs) $ "Too many arguments to " <> show c
         pure $ T.AppOp (t × v) (t' × v') × val2 (V.Constr2 c $ vs <> singleton v')
      _                 -> report "Expected closure, operator or unsaturated constructor"
eval ρ (Expr2' _ (E.BinaryApp2 e op e')) = do
   t  × v  <- eval ρ e
   t' × v' <- eval ρ e'
   Val2' _ u <- find op ρ
   case u of
      V.Primitive2 φ ->
         let Val2' _ u' = apply φ v in
         case u' of
            V.Primitive2 φ_v   -> pure $ T.BinaryApp (t × v) op (t' × v') × apply φ_v v'
            _                 -> report "Not a binary operator"
      _ -> report "Not an operator"
eval ρ (Expr2' _ (E.Let2 (E.VarDef σ e) e')) = do
   t  × v      <- eval ρ e
   ρ' × _ × ξ  <- match v σ
   t' × v'     <- eval (ρ <> ρ') e'
   pure $ T.Let (T.VarDef ξ t) t' × v'
eval ρ (Expr2' _ (E.MatchAs2 e σ)) = do
   t  × v      <- eval ρ e
   ρ' × e' × ξ <- match v σ
   t' × v'     <- eval (ρ <> ρ') (body2 e')
   pure $ T.MatchAs t ξ t' × v'

defs :: Env2 -> Module2 -> MayFail Env2
defs ρ (Module2 Nil) = pure ρ
defs ρ (Module2 (Left (E.VarDef σ e) : ds)) = do
   _  × v      <- eval ρ e
   ρ' × _ × ξ  <- match v σ
   defs (ρ <> ρ') (Module2 ds)
defs ρ (Module2 (Right δ : ds)) =
   defs (ρ <> closeDefs ρ δ δ) (Module2 ds)
