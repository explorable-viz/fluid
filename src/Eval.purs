module Eval where

import Prelude hiding (absurd, top)

import Bindings (varAnon)
import Data.Array (fromFoldable) as A
import Data.Bifunctor (bimap)
import Data.Either (Either(..), note)
import Data.List (List(..), (:), length, range, singleton, unzip, zip)
import Data.Set (fromFoldable, toUnfoldable, singleton) as S
import Data.Set (union, subset)
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd)
import DataType (Ctr, arity, consistentWith, dataTypeFor, showCtr)
import Dict (disjointUnion, get, empty, lookup, keys)
import Dict (fromFoldable, singleton, unzip) as D
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDefs, VarDef(..), asExpr, fv)
import Lattice ((∧), erase, top)
import Pretty (prettyP)
import Primitive (intPair, string)
import Trace (AppTrace(..), Trace(..), VarDef(..)) as T
import Trace (AppTrace, Trace, Match(..))
import Util (type (×), MayFail, absurd, both, check, error, report, successful, with, (×))
import Util.Pair (unzip) as P
import Val (Val(..)) as V
import Val (class Ann, Env, PrimOp(..), (<+>), Val, for, lookup', restrict)

patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: forall a. Ann a => Val a -> Elim a -> MayFail (Env a × Cont a × a × Match)
match v (ElimVar x κ)
   | x == varAnon = pure (empty × κ × top × MatchVarAnon (erase v))
   | otherwise = pure (D.singleton x v × κ × top × MatchVar x (erase v))
match (V.Constr α c vs) (ElimConstr m) = do
   with "Pattern mismatch" $ S.singleton c `consistentWith` keys m
   κ <- note ("Incomplete patterns: no branch for " <> showCtr c) (lookup c m)
   γ × κ' × α' × ws <- matchMany vs κ
   pure (γ × κ' × (α ∧ α') × MatchConstr c ws)
match v (ElimConstr m) = do
   d <- dataTypeFor $ keys m
   report $ patternMismatch (prettyP v) (show d)
match (V.Record α xvs) (ElimRecord xs κ) = do
   check (subset xs (S.fromFoldable $ keys xvs)) $ patternMismatch (show (keys xvs)) (show xs)
   let xs' = xs # S.toUnfoldable
   γ × κ' × α' × ws <- matchMany (xs' <#> flip get xvs) κ
   pure (γ × κ' × (α ∧ α') × MatchRecord (D.fromFoldable (zip xs' ws)))
match v (ElimRecord xs _) = report (patternMismatch (prettyP v) (show xs))

matchMany :: forall a. Ann a => List (Val a) -> Cont a -> MayFail (Env a × Cont a × a × List Match)
matchMany Nil κ = pure (empty × κ × top × Nil)
matchMany (v : vs) (ContElim σ) = do
   γ × κ' × α × w <- match v σ
   γ' × κ'' × β × ws <- matchMany vs κ'
   pure $ γ `disjointUnion` γ' × κ'' × (α ∧ β) × (w : ws)
matchMany (_ : vs) (ContExpr _) = report $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"
matchMany _ _ = error absurd

closeDefs :: forall a. Env a -> RecDefs a -> a -> Env a
closeDefs γ ρ α = ρ <#> \σ ->
   let ρ' = ρ `for` σ in V.Closure α (γ `restrict` (fv ρ' `union` fv σ)) ρ' σ

checkArity :: Ctr -> Int -> MayFail Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) (showCtr c <> " got " <> show n <> " argument(s), expects at most " <> show n')

apply :: forall a. Ann a => Val a -> Val a -> MayFail (AppTrace × Val a)
apply (V.Closure β γ1 ρ σ) v = do
   let γ2 = closeDefs γ1 ρ β
   γ3 × e'' × β' × w <- match v σ
   t'' × v'' <- eval (γ1 <+> γ2 <+> γ3) (asExpr e'') (β ∧ β')
   pure $ T.AppClosure (S.fromFoldable (keys ρ)) w t'' × v''
apply (V.Primitive (PrimOp φ) vs) v =
   let
      vs' = vs <> singleton v
      v'' = if φ.arity > length vs' then V.Primitive (PrimOp φ) vs' else φ.op vs'
   in
      pure $ T.AppPrimitive (PrimOp φ × (erase <$> vs)) (erase v) × v''
apply _ _ = report "Expected closure, operator or unsaturated constructor"

eval :: forall a. Ann a => Env a -> Expr a -> a -> MayFail (Trace × Val a)
eval γ (Var x) _ = (T.Var x × _) <$> lookup' x γ
eval γ (Op op) _ = (T.Op op × _) <$> lookup' op γ
eval _ (Int α n) α' = pure (T.Const × V.Int (α ∧ α') n)
eval _ (Float α n) α' = pure (T.Const × V.Float (α ∧ α') n)
eval _ (Str α str) α' = pure (T.Const × V.Str (α ∧ α') str)
eval γ (Record α xes) α' = do
   xts × xvs <- traverse (flip (eval γ) α') xes <#> D.unzip
   pure $ T.Record xts × V.Record (α ∧ α') xvs
eval γ (Dictionary α ees) α' = do
   (ts × vs) × (ts' × us) <- traverse (traverse (flip (eval γ) α')) ees <#> (P.unzip >>> (unzip # both))
   let
      ss × αs = (vs <#> \u -> string.match u) # unzip
      d = D.fromFoldable $ zip ss (zip αs us)
   pure $ T.Dictionary (zip (zip ss ts) ts') (d <#> snd >>> erase) × V.Dictionary (α ∧ α') d
eval γ (Constr α c es) α' = do
   checkArity c (length es)
   ts × vs <- traverse (flip (eval γ) α') es <#> unzip
   pure (T.Constr c ts × V.Constr (α ∧ α') c vs)
eval γ (Matrix α e (x × y) e') α' = do
   t × v <- eval γ e' α'
   let (i' × β) × (j' × β') = fst (intPair.match v)
   check (i' × j' >= 1 × 1) ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
   tss × vss <- unzipToArray <$> ((<$>) unzipToArray) <$>
      ( sequence $ do
           i <- range 1 i'
           singleton $ sequence $ do
              j <- range 1 j'
              let γ' = D.singleton x (V.Int β i) `disjointUnion` (D.singleton y (V.Int β' j))
              singleton (eval (γ <+> γ') e α')
      )
   pure $ T.Matrix tss (x × y) (i' × j') t × V.Matrix (α ∧ α') (vss × (i' × β) × (j' × β'))
   where
   unzipToArray :: forall b c. List (b × c) -> Array b × Array c
   unzipToArray = unzip >>> bimap A.fromFoldable A.fromFoldable
eval γ (Lambda σ) α =
   pure $ T.Const × V.Closure α (γ `restrict` fv σ) empty σ
eval γ (Project e x) α = do
   t × v <- eval γ e α
   case v of
      V.Record _ xvs -> (T.Project t x × _) <$> lookup' x xvs
      _ -> report "Expected record"
eval γ (App e e') α = do
   t × v <- eval γ e α
   t' × v' <- eval γ e' α
   case v of
      V.Closure β γ1 ρ σ -> do
         let γ2 = closeDefs γ1 ρ β
         γ3 × e'' × β' × w <- match v' σ
         t'' × v'' <- eval (γ1 <+> γ2 <+> γ3) (asExpr e'') (β ∧ β')
         pure $ T.App t t' (T.AppClosure (S.fromFoldable (keys ρ)) w t'') × v''
      V.Primitive (PrimOp φ) vs ->
         let
            vs' = vs <> singleton v'
            v'' = if φ.arity > length vs' then V.Primitive (PrimOp φ) vs' else φ.op vs'
         in
            pure $ T.App t t' (T.AppPrimitive (PrimOp φ × (erase <$> vs)) (erase v')) × v''
      V.Constr α' c vs -> do
         check (successful (arity c) > length vs) ("Too many arguments to " <> showCtr c)
         pure $ T.App t t' (T.AppConstr (c × length vs)) × V.Constr (α ∧ α') c (vs <> singleton v')
      _ -> report "Expected closure, operator or unsaturated constructor"
eval γ (Let (VarDef σ e) e') α = do
   t × v <- eval γ e α
   γ' × _ × α' × w <- match v σ -- terminal meta-type of eliminator is meta-unit
   t' × v' <- eval (γ <+> γ') e' α' -- (α ∧ α') for consistency with functions? (similarly for module defs)
   pure $ T.Let (T.VarDef w t) t' × v'
eval γ (LetRec ρ e) α = do
   let γ' = closeDefs γ ρ α
   t × v <- eval (γ <+> γ') e α
   pure $ T.LetRec (erase <$> ρ) t × v

eval_module :: forall a. Ann a => Env a -> Module a -> a -> MayFail (Env a)
eval_module γ = go empty
   where
   go :: Env a -> Module a -> a -> MayFail (Env a)
   go γ' (Module Nil) _ = pure γ'
   go y' (Module (Left (VarDef σ e) : ds)) α = do
      _ × v <- eval (γ <+> y') e α
      γ'' × _ × α' × _ <- match v σ
      go (y' <+> γ'') (Module ds) α'
   go γ' (Module (Right ρ : ds)) α =
      go (γ' <+> closeDefs (γ <+> γ') ρ α) (Module ds) α
