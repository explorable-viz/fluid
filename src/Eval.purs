module Eval where

import Prelude hiding (absurd, apply)
import Data.Array (fromFoldable)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), note)
import Data.List (List(..), (:), length, range, singleton, unzip, snoc)
import Data.Map (lookup, update)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Bindings (Bindings(..), (:+:), (↦), find)
import DataType (Ctr, arity, checkDataType, cPair, dataTypeForKeys)
import Expl (Expl(..), VarDef(..)) as T
import Expl (Expl, Match(..))
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDefs, VarDef(..), asExpr, varAnon)
import Lattice (𝔹)
import Pretty (pretty, render)
import Primitive (apply, to)
import Util (MayFail, type (×), (×), absurd, check, error, report, successful)
import Val (Env, Val)
import Val (Val(..)) as V

match :: Val 𝔹 -> Elim 𝔹 -> MayFail (Env 𝔹 × Cont 𝔹 × Match 𝔹)
match v (ElimVar x κ)
   | x == varAnon = pure $ Empty × κ × MatchVarAnon v
   | otherwise    = pure $ (Empty :+: x ↦ v) × κ × MatchVar x
match (V.Constr _ c vs) (ElimConstr κs) = do
   checkDataType "Pattern mismatch: " c κs
   κ <- note ("Incomplete pattern: no branch for " <> show c) $ lookup c κs
   ρ × κ' × ξs <- matchArgs c vs κ
   pure $ ρ × κ' × (MatchConstr (c × ξs) $ update (const Nothing) c κs)
match v (ElimConstr κs) = do
   d <- dataTypeForKeys $ keys κs
   report $ "Pattern mismatch: " <> render (pretty v) <>
            " is not a constructor value, expected " <> show d

matchArgs :: Ctr -> List (Val 𝔹) -> Cont 𝔹 -> MayFail (Env 𝔹 × Cont 𝔹 × List (Match 𝔹))
matchArgs _ Nil κ                = pure $ Empty × κ × Nil
matchArgs c (v : vs) (Arg σ)     = do
   ρ  × κ'  × ξ  <- match v σ
   ρ' × κ'' × ξs <- matchArgs c vs κ'
   pure $ (ρ <> ρ') × κ'' × (snoc ξs ξ)
matchArgs c (_ : vs) (Body _)    = report $
   show (length vs + 1) <> " extra argument(s) to " <> show c <> "; did you forget parentheses in lambda pattern?"
matchArgs _ _ _                  = error absurd

closeDefs :: Env 𝔹 -> RecDefs 𝔹 -> RecDefs 𝔹 -> Env 𝔹
closeDefs _ _ Empty           = Empty
closeDefs ρ δ0 (δ :+: f ↦ σ)  = closeDefs ρ δ0 δ :+: f ↦ V.Closure ρ δ0 σ

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) $ show c <> " got " <> show n <> " argument(s), expects at most " <> show n'

eval :: Env 𝔹 -> Expr 𝔹 -> MayFail (Expl 𝔹 × Val 𝔹)
eval ρ Hole = error absurd
eval ρ (Var x) =
   (T.Var ρ x × _) <$> find x ρ
eval ρ (Op op) =
   (T.Op ρ op × _) <$> find op ρ
eval ρ (Int _ n) =
   (T.Int ρ × _) <$> pure (V.Int false n)
eval ρ (Float _ n) =
   (T.Float ρ × _) <$> pure (V.Float false n)
eval ρ (Str _ str) =
   (T.Str ρ × _) <$> pure (V.Str false str)
eval ρ (Constr _ c es) = do
   checkArity c (length es)
   ts × vs <- traverse (eval ρ) es <#> unzip
   (T.Constr ρ c ts × _) <$> pure (V.Constr false c vs)
eval ρ (Matrix _ e (x × y) e') = do
   t × v <- eval ρ e'
   case v of
      V.Hole -> error absurd
      V.Constr _ c (v1 : v2 : Nil) | c == cPair -> do
         let i' × j' = to v1 × to v2
         check (i' × j' >= 1 × 1) $ "array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")"
         ts × vs <- unzipToArray <$> ((<$>) unzipToArray) <$> (sequence $ do
            i <- range 1 i'
            singleton $ sequence $ do
               j <- range 1 j'
               singleton $ eval ((ρ :+: x ↦ V.Int false i) :+: y ↦ V.Int false j) e)
         ((T.Matrix ts (x × y) t) × _) <$> pure (V.Matrix false vs (i' × j'))
      v' -> report $ "Array dimensions must be pair of ints; got " <> render (pretty v')
   where
   unzipToArray :: forall a b . List (a × b) -> Array a × Array b
   unzipToArray = unzip >>> bimap fromFoldable fromFoldable
eval ρ (LetRec δ e) = do
   let ρ' = closeDefs ρ δ δ
   t × v <- eval (ρ <> ρ') e
   (T.LetRec δ t × _) <$> pure v
eval ρ (Lambda σ) =
   (T.Lambda × _) <$> pure (V.Closure ρ Empty σ)
eval ρ (App e e') = do
   t × v <- eval ρ e
   t' × v' <- eval ρ e'
   case v of
      V.Hole -> error absurd
      V.Closure ρ1 δ σ -> do
         let ρ2 = closeDefs ρ1 δ δ
         ρ3 × e'' × ξ <- match v' σ
         t'' × v'' <- eval (ρ1 <> ρ2 <> ρ3) $ asExpr e''
         ((T.App (t × δ) t' ξ t'') × _) <$> pure v''
      V.Primitive _ φ ->
         ((T.AppOp (t × v) (t' × v')) × _) <$> pure (apply φ v')
      V.Constr _ c vs -> do
         check (successful (arity c) > length vs) $ "Too many arguments to " <> show c
         ((T.AppOp (t × v) (t' × v')) × _) <$> pure (V.Constr false c $ vs <> singleton v')
      _ -> report "Expected closure, operator or unsaturated constructor"
eval ρ (BinaryApp e op e') = do
   t  × v  <- eval ρ e
   t' × v' <- eval ρ e'
   v_φ <- find op ρ
   let t_app = (T.BinaryApp (t × v) (op × v_φ) (t' × v'))
   case v_φ of
      V.Hole -> error absurd
      V.Primitive _ φ ->
         case apply φ v of
            V.Hole -> error absurd
            V.Primitive _ φ_v -> pure $ t_app × apply φ_v v'
            _ -> report "Not a binary operator"
      _ -> report "Not an operator"
eval ρ (Let (VarDef σ e) e') = do
   t  × v      <- eval ρ e
   ρ' × κ × ξ  <- match v σ
   t' × v'     <- eval (ρ <> ρ') e'
   ((T.Let (T.VarDef ξ t) t') × _) <$> pure v'

eval_module :: Env 𝔹 -> Module 𝔹 -> MayFail (Env 𝔹)
eval_module ρ (Module Nil) = pure ρ
eval_module ρ (Module (Left (VarDef σ e) : ds)) = do
   _  × v      <- eval ρ e
   ρ' × _ × ξ  <- match v σ
   eval_module (ρ <> ρ') (Module ds)
eval_module ρ (Module (Right δ : ds)) =
   eval_module (ρ <> closeDefs ρ δ δ) (Module ds)
