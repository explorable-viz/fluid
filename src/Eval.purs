module Eval where

import Prelude hiding (absurd, apply, top)

import Bindings (varAnon)
import Data.Array (fromFoldable) as A
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Exists (mkExists, runExists)
import Data.List (List(..), (:), length, range, singleton, unzip, zip)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
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
import Trace (AppTrace, ForeignTrace, ForeignTrace'(..), Match(..), Trace)
import Util (type (×), MayFailT, absurd, both, check, error, report, successful, orElse, with, (×))
import Util.Pair (unzip) as P
import Val (Fun(..), Val(..)) as V
import Val (class Ann, DictRep(..), Env, ForeignOp'(..), MatrixRep(..), (<+>), Val, for, lookup', restrict)

patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: forall a m. Monad m => Ann a => Val a -> Elim a -> MayFailT m (Env a × Cont a × a × Match)
match v (ElimVar x κ)
   | x == varAnon = pure (empty × κ × top × MatchVarAnon (erase v))
   | otherwise = pure (D.singleton x v × κ × top × MatchVar x (erase v))
match (V.Constr α c vs) (ElimConstr m) = do
   with "Pattern mismatch" $ S.singleton c `consistentWith` keys m
   κ <- lookup c m # orElse ("Incomplete patterns: no branch for " <> showCtr c)
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

matchMany :: forall a m. Monad m => Ann a => List (Val a) -> Cont a -> MayFailT m (Env a × Cont a × a × List Match)
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
   let ρ' = ρ `for` σ in V.Fun α $ V.Closure (γ `restrict` (fv ρ' `union` fv σ)) ρ' σ

checkArity :: forall m. Monad m => Ctr -> Int -> MayFailT m Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) (showCtr c <> " got " <> show n <> " argument(s), expects at most " <> show n')

apply :: forall a m. Monad m => Ann a => Val a × Val a -> MayFailT m (AppTrace × Val a)
apply (V.Fun β (V.Closure γ1 ρ σ) × v) = do
   let γ2 = closeDefs γ1 ρ β
   γ3 × e'' × β' × w <- match v σ
   t'' × v'' <- eval (γ1 <+> γ2 <+> γ3) (asExpr e'') (β ∧ β')
   pure $ T.AppClosure (S.fromFoldable (keys ρ)) w t'' × v''
apply (V.Fun α (V.Foreign φ vs) × v) = do
   t × v'' <- runExists apply' φ
   pure $ T.AppForeign (length vs + 1) t × v''
   where
   vs' = vs <> singleton v

   apply' :: forall t. ForeignOp' t -> MayFailT m (ForeignTrace × Val _)
   apply' (ForeignOp' φ') = do
      t × v'' <- do
         if φ'.arity > length vs' then pure $ Nothing × V.Fun α (V.Foreign φ vs')
         else first Just <$> φ'.op vs'
      pure $ mkExists (ForeignTrace' (ForeignOp' φ') t) × v''
apply (V.Fun α (V.PartialConstr c vs) × v) = do
   check (length vs < n) ("Too many arguments to " <> showCtr c)
   pure $ T.AppConstr c × v'
   where
   n = successful (arity c)
   v' =
      if length vs < n - 1 then V.Fun α $ V.PartialConstr c (vs <> singleton v)
      else V.Constr α c (vs <> singleton v)
apply (_ × v) = report $ "Found " <> prettyP v <> ", expected function"

apply2 :: forall a m. Monad m => Ann a => Val a × Val a × Val a -> MayFailT m ((AppTrace × AppTrace) × Val a)
apply2 (u1 × v1 × v2) = do
   t1 × u2 <- apply (u1 × v1)
   t2 × v <- apply (u2 × v2)
   pure $ (t1 × t2) × v

eval :: forall a m. Monad m => Ann a => Env a -> Expr a -> a -> MayFailT m (Trace × Val a)
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
   pure $ T.Dictionary (zip ss (zip ts ts')) (d <#> snd >>> erase) × V.Dictionary (α ∧ α') (DictRep d)
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
   pure $ T.Matrix tss (x × y) (i' × j') t × V.Matrix (α ∧ α') (MatrixRep (vss × (i' × β) × (j' × β')))
   where
   unzipToArray :: forall b c. List (b × c) -> Array b × Array c
   unzipToArray = unzip >>> bimap A.fromFoldable A.fromFoldable
eval γ (Lambda σ) α =
   pure $ T.Const × V.Fun α (V.Closure (γ `restrict` fv σ) empty σ)
eval γ (Project e x) α = do
   t × v <- eval γ e α
   case v of
      V.Record _ xvs -> (T.Project t x × _) <$> lookup' x xvs
      _ -> report $ "Found " <> prettyP v <> ", expected record"
eval γ (App e e') α = do
   t × v <- eval γ e α
   t' × v' <- eval γ e' α
   t'' × v'' <- apply (v × v')
   pure $ T.App t t' t'' × v''
eval γ (Let (VarDef σ e) e') α = do
   t × v <- eval γ e α
   γ' × _ × α' × w <- match v σ -- terminal meta-type of eliminator is meta-unit
   t' × v' <- eval (γ <+> γ') e' α' -- (α ∧ α') for consistency with functions? (similarly for module defs)
   pure $ T.Let (T.VarDef w t) t' × v'
eval γ (LetRec ρ e) α = do
   let γ' = closeDefs γ ρ α
   t × v <- eval (γ <+> γ') e α
   pure $ T.LetRec (erase <$> ρ) t × v

eval_module :: forall a m. Monad m => Ann a => Env a -> Module a -> a -> MayFailT m (Env a)
eval_module γ = go empty
   where
   go :: Env a -> Module a -> a -> MayFailT m (Env a)
   go γ' (Module Nil) _ = pure γ'
   go y' (Module (Left (VarDef σ e) : ds)) α = do
      _ × v <- eval (γ <+> y') e α
      γ'' × _ × α' × _ <- match v σ
      go (y' <+> γ'') (Module ds) α'
   go γ' (Module (Right ρ : ds)) α =
      go (γ' <+> closeDefs (γ <+> γ') ρ α) (Module ds) α
