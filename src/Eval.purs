module Eval where

import Prelude hiding (absurd)

import Data.Array (fromFoldable)
import Data.Bifunctor (bimap)
import Data.Either (Either(..), note)
import Data.List (List(..), (:), length, range, singleton, unzip)
import Data.List.NonEmpty (singleton) as NEL
import Data.Map (empty, filterKeys, insert, lookup)
import Data.Map (singleton) as M
import Data.Map.Internal (keys)
import Data.Profunctor.Strong ((&&&), second)
import Data.Set (union)
import Data.Traversable (sequence, traverse)
import Bindings (Bindings, (↦), find, key, val, varAnon, Var)
import DataType (Ctr, arity, cPair, dataTypeFor)
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDefs, VarDef(..), asExpr, asElim, for, fv)
import Lattice (𝔹, checkConsistent)
import Pretty (prettyP)
import Primitive (match) as P
import Trace (Trace(..), VarDef(..)) as T
import Trace (Trace, Match(..))
import Util (MayFail, type (×), (×), absurd, check, disjUnion, error, report, successful)
import Util.SnocList (SnocList(..), (:-), zipWith)
import Util.SnocList (unzip) as S
import Val (Env, PrimOp(..), SingletonEnv, Val, concat, lookup', restrict)
import Val (Val(..)) as V

patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: Val 𝔹 -> Elim 𝔹 -> MayFail (SingletonEnv 𝔹 × Cont 𝔹 × Match 𝔹)
match v (ElimVar x κ)  | x == varAnon    = pure (empty × κ × MatchVarAnon v)
                       | otherwise       = pure (M.singleton x v × κ × MatchVar x)
match (V.Constr _ c vs) (ElimConstr m) = do
   checkConsistent "Pattern mismatch: " c (keys m)
   κ <- note ("Incomplete patterns: no branch for " <> show c) (lookup c m)
   (second (\ws -> MatchConstr c ws (filterKeys ((/=) c) m))) <$> matchArgs c vs κ
match v (ElimConstr m)                    = (report <<< patternMismatch (prettyP v)) =<< show <$> dataTypeFor (keys m)
match (V.Record _ xvs) (ElimRecord xs κ)  = second MatchRecord <$> matchRecord xvs xs κ
match v (ElimRecord xs _)                 = report (patternMismatch (prettyP v) (show xs))

matchArgs :: Ctr -> List (Val 𝔹) -> Cont 𝔹 -> MayFail (SingletonEnv 𝔹 × Cont 𝔹 × List (Match 𝔹))
matchArgs _ Nil κ = pure (empty × κ × Nil)
matchArgs c (v : vs) (ContElim σ) = do
   γ  × κ'  × w  <- match v σ
   γ' × κ'' × ws <- matchArgs c vs κ'
   pure ((γ `disjUnion` γ') × κ'' × (w : ws))
matchArgs c (_ : vs) (ContExpr _) = report $
   show (length vs + 1) <> " extra argument(s) to " <> show c <> "; did you forget parentheses in lambda pattern?"
matchArgs _ _ _ = error absurd

matchRecord :: Bindings (Val 𝔹) -> SnocList Var -> Cont 𝔹 -> MayFail (SingletonEnv 𝔹 × Cont 𝔹 × Bindings (Match 𝔹))
matchRecord Lin Lin κ = pure (empty × κ × Lin)
matchRecord (xvs :- x ↦ v) (xs :- x') σ = do
   check (x == x') (patternMismatch (show x) (show x'))
   γ × σ' × xws <- matchRecord xvs xs σ
   γ' × κ × w <- match v (asElim σ')
   pure ((γ `disjUnion` γ') × κ × (xws :- x ↦ w))
matchRecord (_ :- x ↦ _) Lin _ = report (patternMismatch "end of record pattern" (show x))
matchRecord Lin (_ :- x) _ = report (patternMismatch "end of record" (show x))

closeDefs :: Env 𝔹 -> RecDefs 𝔹 -> RecDefs 𝔹 -> SingletonEnv 𝔹
closeDefs _ _ Lin = empty
closeDefs γ ρ0 (ρ :- f ↦ σ) =
   let xs = fv (ρ0 `for` σ) `union` fv σ
   in closeDefs γ ρ0 ρ # insert f (V.Closure false (γ `restrict` xs) ρ0 σ)

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) (show c <> " got " <> show n <> " argument(s), expects at most " <> show n')

eval :: Env 𝔹 -> Expr 𝔹 -> MayFail (Trace 𝔹 × Val 𝔹)
eval γ (Var x)       = (T.Var γ x × _) <$> lookup' x γ
eval γ (Op op)       = (T.Op γ op × _) <$> lookup' op γ
eval γ (Int _ n)     = pure (T.Int γ n × V.Int false n)
eval γ (Float _ n)   = pure (T.Float γ n × V.Float false n)
eval γ (Str _ str)   = pure (T.Str γ str × V.Str false str)
eval γ (Record _ xes) = do
   let xs × es = xes <#> (key &&& val) # S.unzip
   ts × vs <- traverse (eval γ) es <#> S.unzip
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
               singleton (eval (γ `concat` γ') e))
         pure (T.Matrix tss (x × y) (i' × j') t × V.Matrix false (vss × (i' × false) × (j' × false)))
      v' -> report ("Array dimensions must be pair of ints; got " <> prettyP v')
   where
   unzipToArray :: forall a b . List (a × b) -> Array a × Array b
   unzipToArray = unzip >>> bimap fromFoldable fromFoldable
eval γ (Lambda σ) =
   pure (T.Lambda γ σ × V.Closure false (γ `restrict` fv σ) Lin σ)
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
         let γ1' = γ1 <#> NEL.singleton
             γ2 = closeDefs γ1' ρ ρ
         γ3 × e'' × w <- match v' σ
         t'' × v'' <- eval ((γ1' `concat` γ2) `concat` γ3) (asExpr e'')
         pure (T.App (t × ρ × σ) t' w t'' × v'')
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
   t' × v' <- eval (γ `concat` γ') e'
   pure (T.Let (T.VarDef w t) t' × v')
eval γ (LetRec ρ e) = do
   let γ' = closeDefs γ ρ ρ
   t × v <- eval (γ `concat` γ') e
   pure (T.LetRec ρ t × v)

eval_module :: Env 𝔹 -> Module 𝔹 -> MayFail (Env 𝔹)
eval_module γ (Module Nil) = pure γ
eval_module γ (Module (Left (VarDef σ e) : ds)) = do
   _  × v <- eval γ e
   γ' × _ × _  <- match v σ
   eval_module (γ `concat` γ') (Module ds)
eval_module γ (Module (Right ρ : ds)) =
   eval_module (γ `concat` closeDefs γ ρ ρ) (Module ds)
