module Eval2 where

import Prelude hiding (absurd, apply)

import Ann (erase)
import Bindings (varAnon)
import BoolAlg (BoolAlg)
import Control.Monad.Error.Class (class MonadError)
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
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDefs, VarDef(..), asExpr, fv)
import Pretty2 (prettyP)
import Primitive2 (intPair, string)
import Trace2 (AppTrace(..), Trace(..), VarDef(..)) as T
import Trace2 (AppTrace, ForeignTrace, ForeignTrace'(..), Match(..), Trace)
import Util (type (×), absurd, both, check, error, orElse, successful, throw, with, (×))
import Util.Pair (unzip) as P
import Val2 (Fun(..), Val(..)) as V
import Val2 (class Highlightable, DictRep(..), Env, ForeignOp'(..), MatrixRep(..), Val, for, lookup', restrict, (<+>))

patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: forall a m. MonadError Error m => Highlightable a => BoolAlg a -> Val a -> Elim a -> m (Env a × Cont a × a × Match)
match 𝒶 v (ElimVar x κ)
   | x == varAnon = pure (empty × κ × 𝒶.top × MatchVarAnon (erase v))
   | otherwise = pure (D.singleton x v × κ × 𝒶.top × MatchVar x (erase v))
match 𝒶 (V.Constr α c vs) (ElimConstr m) = do
   with "Pattern mismatch" $ S.singleton c `consistentWith` keys m
   κ <- lookup c m # orElse ("Incomplete patterns: no branch for " <> showCtr c)
   γ × κ' × α' × ws <- matchMany 𝒶 vs κ
   pure (γ × κ' × (α `𝒶.meet` α') × MatchConstr c ws)
match _ v (ElimConstr m) = do
   d <- dataTypeFor $ keys m
   throw $ patternMismatch (prettyP v) (show d)
match 𝒶 (V.Record α xvs) (ElimRecord xs κ) = do
   check (subset xs (S.fromFoldable $ keys xvs)) $ patternMismatch (show (keys xvs)) (show xs)
   let xs' = xs # S.toUnfoldable
   γ × κ' × α' × ws <- matchMany 𝒶 (xs' <#> flip get xvs) κ
   pure (γ × κ' × (α `𝒶.meet` α') × MatchRecord (D.fromFoldable (zip xs' ws)))
match _ v (ElimRecord xs _) = throw $ patternMismatch (prettyP v) (show xs)

matchMany :: forall a m. MonadError Error m => Highlightable a => BoolAlg a -> List (Val a) -> Cont a -> m (Env a × Cont a × a × List Match)
matchMany 𝒶 Nil κ = pure (empty × κ × 𝒶.top × Nil)
matchMany 𝒶 (v : vs) (ContElim σ) = do
   γ × κ' × α × w <- match 𝒶 v σ
   γ' × κ'' × β × ws <- matchMany 𝒶 vs κ'
   pure $ γ `disjointUnion` γ' × κ'' × (α `𝒶.meet` β) × (w : ws)
matchMany _ (_ : vs) (ContExpr _) = throw $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"
matchMany _ _ _ = error absurd

closeDefs :: forall a. Env a -> RecDefs a -> a -> Env a
closeDefs γ ρ α = ρ <#> \σ ->
   let ρ' = ρ `for` σ in V.Fun α $ V.Closure (γ `restrict` (fv ρ' `union` fv σ)) ρ' σ

checkArity :: forall m. MonadError Error m => Ctr -> Int -> m Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) (showCtr c <> " got " <> show n <> " argument(s), expects at most " <> show n')

apply :: forall a m. MonadError Error m => Highlightable a => BoolAlg a -> Val a × Val a -> m (AppTrace × Val a)
apply 𝒶 (V.Fun β (V.Closure γ1 ρ σ) × v) = do
   let γ2 = closeDefs γ1 ρ β
   γ3 × e'' × β' × w <- match 𝒶 v σ
   t'' × v'' <- eval 𝒶 (γ1 <+> γ2 <+> γ3) (asExpr e'') (β `𝒶.meet` β')
   pure $ T.AppClosure (S.fromFoldable (keys ρ)) w t'' × v''
apply 𝒶 (V.Fun α (V.Foreign φ vs) × v) = do
   t × v'' <- runExists apply' φ
   pure $ T.AppForeign (length vs + 1) t × v''
   where
   vs' = vs <> singleton v

   apply' :: forall t. ForeignOp' t -> m (ForeignTrace × Val _)
   apply' (ForeignOp' φ') = do
      t × v'' <- do
         if φ'.arity > length vs' then pure $ Nothing × V.Fun α (V.Foreign φ vs')
         else first Just <$> φ'.op 𝒶 vs'
      pure $ mkExists (ForeignTrace' (ForeignOp' φ') t) × v''
apply _ (V.Fun α (V.PartialConstr c vs) × v) = do
   check (length vs < n) ("Too many arguments to " <> showCtr c)
   pure $ T.AppConstr c × v'
   where
   n = successful (arity c)
   v' =
      if length vs < n - 1 then V.Fun α $ V.PartialConstr c (vs <> singleton v)
      else V.Constr α c (vs <> singleton v)
apply _ (_ × v) = throw $ "Found " <> prettyP v <> ", expected function"

apply2 :: forall a m. MonadError Error m => Highlightable a => BoolAlg a -> Val a × Val a × Val a -> m ((AppTrace × AppTrace) × Val a)
apply2 𝒶 (u1 × v1 × v2) = do
   t1 × u2 <- apply 𝒶 (u1 × v1)
   t2 × v <- apply 𝒶 (u2 × v2)
   pure $ (t1 × t2) × v

eval :: forall a m. MonadError Error m => Highlightable a => BoolAlg a -> Env a -> Expr a -> a -> m (Trace × Val a)
eval _ γ (Var x) _ = (T.Var x × _) <$> lookup' x γ
eval _ γ (Op op) _ = (T.Op op × _) <$> lookup' op γ
eval 𝒶 _ (Int α n) α' = pure (T.Const × V.Int (α `𝒶.meet` α') n)
eval 𝒶 _ (Float α n) α' = pure (T.Const × V.Float (α `𝒶.meet` α') n)
eval 𝒶 _ (Str α str) α' = pure (T.Const × V.Str (α `𝒶.meet` α') str)
eval 𝒶 γ (Record α xes) α' = do
   xts × xvs <- traverse (flip (eval 𝒶 γ) α') xes <#> D.unzip
   pure $ T.Record xts × V.Record (α `𝒶.meet` α') xvs
eval 𝒶 γ (Dictionary α ees) α' = do
   (ts × vs) × (ts' × us) <- traverse (traverse (flip (eval 𝒶 γ) α')) ees <#> (P.unzip >>> (unzip # both))
   let
      ss × αs = (vs <#> \u -> string.match u) # unzip
      d = D.fromFoldable $ zip ss (zip αs us)
   pure $ T.Dictionary (zip ss (zip ts ts')) (d <#> snd >>> erase) × V.Dictionary (α `𝒶.meet` α') (DictRep d)
eval 𝒶 γ (Constr α c es) α' = do
   checkArity c (length es)
   ts × vs <- traverse (flip (eval 𝒶 γ) α') es <#> unzip
   pure (T.Constr c ts × V.Constr (α `𝒶.meet` α') c vs)
eval 𝒶 γ (Matrix α e (x × y) e') α' = do
   t × v <- eval 𝒶 γ e' α'
   let (i' × β) × (j' × β') = fst (intPair.match v)
   check (i' × j' >= 1 × 1) ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
   tss × vss <- unzipToArray <$> ((<$>) unzipToArray) <$>
      ( sequence $ do
           i <- range 1 i'
           singleton $ sequence $ do
              j <- range 1 j'
              let γ' = D.singleton x (V.Int β i) `disjointUnion` (D.singleton y (V.Int β' j))
              singleton (eval 𝒶 (γ <+> γ') e α')
      )
   pure $ T.Matrix tss (x × y) (i' × j') t × V.Matrix (α `𝒶.meet` α') (MatrixRep (vss × (i' × β) × (j' × β')))
   where
   unzipToArray :: forall b c. List (b × c) -> Array b × Array c
   unzipToArray = unzip >>> bimap A.fromFoldable A.fromFoldable
eval _ γ (Lambda σ) α =
   pure $ T.Const × V.Fun α (V.Closure (γ `restrict` fv σ) empty σ)
eval 𝒶 γ (Project e x) α = do
   t × v <- eval 𝒶 γ e α
   case v of
      V.Record _ xvs -> (T.Project t x × _) <$> lookup' x xvs
      _ -> throw $ "Found " <> prettyP v <> ", expected record"
eval 𝒶 γ (App e e') α = do
   t × v <- eval 𝒶 γ e α
   t' × v' <- eval 𝒶 γ e' α
   t'' × v'' <- apply 𝒶 (v × v')
   pure $ T.App t t' t'' × v''
eval 𝒶 γ (Let (VarDef σ e) e') α = do
   t × v <- eval 𝒶 γ e α
   γ' × _ × α' × w <- match 𝒶 v σ -- terminal meta-type of eliminator is meta-unit
   t' × v' <- eval 𝒶 (γ <+> γ') e' α' -- (α ∧ α') for consistency with functions? (similarly for module defs)
   pure $ T.Let (T.VarDef w t) t' × v'
eval 𝒶 γ (LetRec ρ e) α = do
   let γ' = closeDefs γ ρ α
   t × v <- eval 𝒶 (γ <+> γ') e α
   pure $ T.LetRec (erase <$> ρ) t × v

eval_module :: forall a m. MonadError Error m => Highlightable a => BoolAlg a -> Env a -> Module a -> a -> m (Env a)
eval_module 𝒶 γ = go empty
   where
   go :: Env a -> Module a -> a -> m (Env a)
   go γ' (Module Nil) _ = pure γ'
   go y' (Module (Left (VarDef σ e) : ds)) α = do
      _ × v <- eval 𝒶 (γ <+> y') e α
      γ'' × _ × α' × _ <- match 𝒶 v σ
      go (y' <+> γ'') (Module ds) α'
   go γ' (Module (Right ρ : ds)) α =
      go (γ' <+> closeDefs (γ <+> γ') ρ α) (Module ds) α
