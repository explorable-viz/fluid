module Eval where

import Prelude hiding (absurd)

import Data.Array (fromFoldable)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), note)
import Data.List (List(..), (:), length, range, reverse, singleton, unzip, zipWith)
import Data.Map (empty, lookup)
import Data.Map (singleton) as M
import Data.Map.Internal (keys)
import Data.Profunctor.Strong ((&&&), second)
import Data.Set (union)
import Data.Traversable (sequence, traverse)
import Bindings ((↦), asMap, find, key, val, varAnon)
import DataType (Ctr, arity, cPair, dataTypeFor)
import Expr (Cont(..), Elim(..), Expr(..), Module(..), VarDef(..), asExpr, fv)
import Lattice (𝔹, checkConsistent)
import Pretty (prettyP)
import Primitive (match) as P
import Trace (Trace(..), VarDef(..)) as T
import Trace (Trace, Match(..))
import Util (MayFail, type (×), (×), absurd, check, disjUnion, error, report, successful)
import Val (Env, FunEnv, PrimOp(..), (<+>), Val, dom, for, lookup', restrict)
import Val (Val(..)) as V

patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: Val 𝔹 -> Elim 𝔹 -> MayFail (Env 𝔹 × Cont 𝔹 × Match 𝔹)
match v (ElimVar x κ)  | x == varAnon    = pure (empty × κ × MatchVarAnon v)
                       | otherwise       = pure (M.singleton x v × κ × MatchVar x v)
match (V.Constr _ c vs) (ElimConstr m) = do
   checkConsistent "Pattern mismatch: " c (keys m)
   κ <- note ("Incomplete patterns: no branch for " <> show c) (lookup c m)
   second (MatchConstr c) <$> matchMany vs κ
match v (ElimConstr m) = do
   d <- dataTypeFor (keys m)
   report $ patternMismatch (prettyP v) (show d)
match (V.Record _ xvs) (ElimRecord xs κ)  = do
   check (xs == (xvs <#> key)) (patternMismatch (show $ xvs <#> key) (show xs))
   second (zipWith (↦) (reverse xs) >>> MatchRecord) <$> matchMany (reverse xvs <#> val) κ
match v (ElimRecord xs _) = report (patternMismatch (prettyP v) (show xs))

matchMany :: List (Val 𝔹) -> Cont 𝔹 -> MayFail (Env 𝔹 × Cont 𝔹 × List (Match 𝔹))
matchMany Nil κ = pure (empty × κ × Nil)
matchMany (v : vs) (ContElim σ) = do
   γ  × κ'  × w  <- match v σ
   γ' × κ'' × ws <- matchMany vs κ'
   pure $ γ `disjUnion` γ' × κ'' × (w : ws)
matchMany (_ : vs) (ContExpr _) = report $
   show (length vs + 1) <> " extra argument(s) to constructor; did you forget parentheses in lambda pattern?"
matchMany _ _ = error absurd

closeDefs :: Env 𝔹 -> FunEnv 𝔹 -> Env 𝔹
closeDefs γ ρ = ρ <#> \σ ->
   let xs = fv (ρ `for` σ) `union` fv σ
   in V.Closure false (γ `restrict` xs) ρ σ

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) (show c <> " got " <> show n <> " argument(s), expects at most " <> show n')

eval :: Env 𝔹 -> Expr 𝔹 -> MayFail (Trace 𝔹 × Val 𝔹)
eval γ (Var x)       = (T.Var x × _) <$> lookup' x γ
eval γ (Op op)       = (T.Op op × _) <$> lookup' op γ
eval _ (Int _ n)     = pure (T.Int n × V.Int false n)
eval _ (Float _ n)   = pure (T.Float n × V.Float false n)
eval _ (Str _ str)   = pure (T.Str str × V.Str false str)
eval γ (Record _ xes) = do
   let xs × es = xes <#> (key &&& val) # unzip
   ts × vs <- traverse (eval γ) es <#> unzip
   pure (T.Record γ (zipWith (↦) xs ts) × V.Record false (zipWith (↦) xs vs))
eval γ (Constr _ c es) = do
   checkArity c (length es)
   ts × vs <- traverse (eval γ) es <#> unzip
   pure (T.Constr γ c ts × V.Constr false c vs)
eval γ (Matrix _ e (x × y) e') = do
   t × v <- eval γ e'
   case v of
      V.Constr _ c (v1 : v2 : Nil) | c == cPair -> do
         let (i' × _) × (j' × _) = P.match v1 × P.match v2
         check (i' × j' >= 1 × 1) ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
         tss × vss <- unzipToArray <$> ((<$>) unzipToArray) <$> (sequence $ do
            i <- range 1 i'
            singleton $ sequence $ do
               j <- range 1 j'
               let γ' = M.singleton x (V.Int false i) `disjUnion` (M.singleton y (V.Int false j))
               singleton (eval (γ <+> γ') e))
         pure (T.Matrix tss (x × y) (i' × j') t × V.Matrix false (vss × (i' × false) × (j' × false)))
      v' -> report ("Array dimensions must be pair of ints; got " <> prettyP v')
   where
   unzipToArray :: forall a b . List (a × b) -> Array a × Array b
   unzipToArray = unzip >>> bimap fromFoldable fromFoldable
eval γ (Lambda σ) =
   pure (T.Lambda σ × V.Closure false (γ `restrict` fv σ) empty σ)
eval γ (Project e x) = do
   t × v <- eval γ e
   case v of
      V.Record _ xvs -> (T.Project t xvs x × _) <$> find x xvs
      _ -> report "Expected record"
eval γ (App e e') = do
   t × v <- eval γ e
   t' × v' <- eval γ e'
   case v of
      V.Closure _ γ1 ρ σ -> do
         let γ2 = closeDefs γ1 ρ
         γ3 × e'' × w <- match v' σ
         t'' × v'' <- eval (γ1 <+> γ2 <+> γ3) (asExpr e'')
         pure (T.App (t × dom ρ × σ) t' w t'' × v'')
      V.Primitive (PrimOp φ) vs ->
         let vs' = vs <> singleton v'
             v'' = if φ.arity > length vs' then V.Primitive (PrimOp φ) vs' else φ.op vs' in
         pure (T.AppPrim (t × PrimOp φ × vs) (t' × v') × v'')
      V.Constr _ c vs -> do
         check (successful (arity c) > length vs) ("Too many arguments to " <> show c)
         pure (T.AppConstr (t × c × length vs) t' × V.Constr false c (vs <> singleton v'))
      _ -> report "Expected closure, operator or unsaturated constructor"
eval γ (Let (VarDef σ e) e') = do
   t × v <- eval γ e
   γ' × _ × w <- match v σ -- terminal meta-type of eliminator is meta-unit
   t' × v' <- eval (γ <+> γ') e'
   pure (T.Let (T.VarDef w t) t' × v')
eval γ (LetRec xσs e) = do
   let γ' = closeDefs γ (asMap xσs)
   t × v <- eval (γ <+> γ') e
   pure (T.LetRec xσs t × v)

eval_module :: Env 𝔹 -> Module 𝔹 -> MayFail (Env 𝔹)
eval_module γ = go empty
   where
   go :: Env 𝔹 -> Module 𝔹 -> MayFail (Env 𝔹)
   go γ' (Module Nil) = pure γ'
   go y' (Module (Left (VarDef σ e) : ds)) = do
      _  × v <- eval (γ <+> y') e
      γ'' × _ × _  <- match v σ
      go (y' <+> γ'') (Module ds)
   go γ' (Module (Right xσs : ds)) =
      go (γ' <+> closeDefs (γ <+> γ') (asMap xσs)) (Module ds)
