module Eval where

import Prelude hiding (absurd)

import Bindings (varAnon)
import Data.Array (fromFoldable) as A
import Data.Bifunctor (bimap)
import Data.Either (Either(..), note)
import Data.List (List(..), (:), length, range, singleton, unzip, zip)
import Data.Profunctor.Strong (second)
import Data.Set (fromFoldable, toUnfoldable, singleton) as S
import Data.Set (union, subset)
import Data.Traversable (sequence, traverse)
import DataType (Ctr, arity, consistentWith, dataTypeFor, showCtr)
import Dict (disjointUnion, get, empty, lookup, keys)
import Dict (fromFoldable, singleton, unzip) as D
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDefs, VarDef(..), asExpr, fv)
import Lattice (class BoundedJoinSemilattice, bot)
import Pretty (class Highlightable, prettyP)
import Primitive (unwrap)
import Trace (Trace(..), VarDef(..)) as T
import Trace (Trace, Match(..))
import Util (type (×), MayFail, absurd, both, check, error, report, successful, with, (×))
import Util.Pair (unzip, zip) as P
import Val (Env, PrimOp(..), (<+>), Val, for, lookup', restrict)
import Val (Val(..)) as V

patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: forall a. Highlightable a => Val a -> Elim a -> MayFail (Env a × Cont a × Match a)
match v (ElimVar x κ)
   | x == varAnon = pure (empty × κ × MatchVarAnon v)
   | otherwise = pure (D.singleton x v × κ × MatchVar x v)
match (V.Constr _ c vs) (ElimConstr m) = do
   with "Pattern mismatch" $ S.singleton c `consistentWith` keys m
   κ <- note ("Incomplete patterns: no branch for " <> showCtr c) (lookup c m)
   second (MatchConstr c) <$> matchMany vs κ
match v (ElimConstr m) = do
   d <- dataTypeFor $ keys m
   report $ patternMismatch (prettyP v) (show d)
match (V.Record _ xvs) (ElimRecord xs κ) = do
   check (subset xs (S.fromFoldable $ keys xvs)) $ patternMismatch (show (keys xvs)) (show xs)
   let xs' = xs # S.toUnfoldable
   second (zip xs' >>> D.fromFoldable >>> MatchRecord) <$> matchMany (xs' <#> flip get xvs) κ
match v (ElimRecord xs _) = report (patternMismatch (prettyP v) (show xs))

matchMany :: forall a. Highlightable a => List (Val a) -> Cont a -> MayFail (Env a × Cont a × List (Match a))
matchMany Nil κ = pure (empty × κ × Nil)
matchMany (v : vs) (ContElim σ) = do
   γ × κ' × w <- match v σ
   γ' × κ'' × ws <- matchMany vs κ'
   pure $ γ `disjointUnion` γ' × κ'' × (w : ws)
matchMany (_ : vs) (ContExpr _) = report $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"
matchMany _ _ = error absurd

closeDefs :: forall a. BoundedJoinSemilattice a => Env a -> RecDefs a -> Env a
closeDefs γ ρ = ρ <#> \σ ->
   let
      ρ' = ρ `for` σ
   in
      V.Closure bot (γ `restrict` (fv ρ' `union` fv σ)) ρ' σ

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) (showCtr c <> " got " <> show n <> " argument(s), expects at most " <> show n')

eval :: forall a. BoundedJoinSemilattice a => Highlightable a => Env a -> Expr a -> MayFail (Trace a × Val a)
eval γ (Var x) = (T.Var x × _) <$> lookup' x γ
eval γ (Op op) = (T.Op op × _) <$> lookup' op γ
eval _ (Int _ n) = pure (T.Const × V.Int bot n)
eval _ (Float _ n) = pure (T.Const × V.Float bot n)
eval _ (Str _ str) = pure (T.Const × V.Str bot str)
eval γ (Record _ xes) = do
   xts × xvs <- traverse (eval γ) xes <#> D.unzip
   pure $ T.Record xts × V.Record bot xvs
eval γ (Dictionary _ ees) = do
   (ts × vs) × (ts' × us) <- traverse (traverse (eval γ)) ees <#> (P.unzip >>> (unzip # both))
   pure $ T.Dictionary (P.zip ts ts') × V.Dictionary bot (D.fromFoldable $ zip (vs <#> unwrap) us)
eval γ (Constr _ c es) = do
   checkArity c (length es)
   ts × vs <- traverse (eval γ) es <#> unzip
   pure (T.Constr c ts × V.Constr bot c vs)
eval γ (Matrix _ e (x × y) e') = do
   t × v <- eval γ e'
   let (i' × (_ :: a)) × (j' × (_ :: a)) = unwrap v
   check (i' × j' >= 1 × 1) ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
   tss × vss <- unzipToArray <$> ((<$>) unzipToArray) <$>
      ( sequence $ do
           i <- range 1 i'
           singleton $ sequence $ do
              j <- range 1 j'
              let γ' = D.singleton x (V.Int bot i) `disjointUnion` (D.singleton y (V.Int bot j))
              singleton (eval (γ <+> γ') e)
      )
   pure $ T.Matrix tss (x × y) (i' × j') t × V.Matrix bot (vss × (i' × bot) × (j' × bot))
   where
   unzipToArray :: forall b c. List (b × c) -> Array b × Array c
   unzipToArray = unzip >>> bimap A.fromFoldable A.fromFoldable
eval γ (Lambda σ) =
   pure $ T.Const × V.Closure bot (γ `restrict` fv σ) empty σ
eval γ (Project e x) = do
   t × v <- eval γ e
   case v of
      V.Record _ xvs -> (T.Project t x × _) <$> lookup' x xvs
      _ -> report "Expected record"
eval γ (App e e') = do
   t × v <- eval γ e
   t' × v' <- eval γ e'
   case v of
      V.Closure _ γ1 ρ σ -> do
         let γ2 = closeDefs γ1 ρ
         γ3 × e'' × w <- match v' σ
         t'' × v'' <- eval (γ1 <+> γ2 <+> γ3) (asExpr e'')
         pure $ T.App (t × S.fromFoldable (keys ρ)) t' w t'' × v''
      V.Primitive (PrimOp φ) vs ->
         let
            vs' = vs <> singleton v'
            v'' = if φ.arity > length vs' then V.Primitive (PrimOp φ) vs' else φ.op vs'
         in
            pure $ T.AppPrim (t × PrimOp φ × vs) (t' × v') × v''
      V.Constr _ c vs -> do
         check (successful (arity c) > length vs) ("Too many arguments to " <> showCtr c)
         pure $ T.AppConstr (t × c × length vs) t' × V.Constr bot c (vs <> singleton v')
      _ -> report "Expected closure, operator or unsaturated constructor"
eval γ (Let (VarDef σ e) e') = do
   t × v <- eval γ e
   γ' × _ × w <- match v σ -- terminal meta-type of eliminator is meta-unit
   t' × v' <- eval (γ <+> γ') e'
   pure $ T.Let (T.VarDef w t) t' × v'
eval γ (LetRec ρ e) = do
   let γ' = closeDefs γ ρ
   t × v <- eval (γ <+> γ') e
   pure $ T.LetRec ρ t × v

eval_module :: forall a. Highlightable a => BoundedJoinSemilattice a => Env a -> Module a -> MayFail (Env a)
eval_module γ = go empty
   where
   go :: Env a -> Module a -> MayFail (Env a)
   go γ' (Module Nil) = pure γ'
   go y' (Module (Left (VarDef σ e) : ds)) = do
      _ × v <- eval (γ <+> y') e
      γ'' × _ × _ <- match v σ
      go (y' <+> γ'') (Module ds)
   go γ' (Module (Right ρ : ds)) =
      go (γ' <+> closeDefs (γ <+> γ') ρ) (Module ds)
