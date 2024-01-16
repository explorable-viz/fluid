module Eval where

import Prelude hiding (absurd, apply, top)

import Bind (varAnon)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (fromFoldable) as A
import Data.Bifunctor (bimap)
import Data.Exists (mkExists, runExists)
import Data.List (List(..), (:), length, range, unzip, zip)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.Set (fromFoldable, toUnfoldable) as Set
import Data.Set (subset)
import Data.Traversable (sequence, traverse)
import Data.Tuple (snd)
import DataType (Ctr, arity, consistentWith, dataTypeFor, showCtr)
import Dict (Dict, disjointUnion, empty, get, keys, lookup)
import Dict (fromFoldable, singleton, unzip) as D
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), Expr(..), RecDefs(..), VarDef(..), asExpr, fv)
import Lattice ((∧), erase, top)
import Pretty (prettyP)
import Primitive (intPair, string, unpack)
import Trace (AppTrace(..), Trace(..), VarDef(..)) as T
import Trace (AppTrace, ForeignTrace(..), ForeignTrace'(..), Match(..), Trace)
import Util (type (×), (×), (∪), absurd, both, check, error, orElse, singleton, successful, throw, with)
import Util.Pair (unzip) as P
import Val (BaseVal(..), Fun(..)) as V
import Val (class Ann, DictRep(..), Env, ForeignOp(..), ForeignOp'(..), MatrixRep(..), Val(..), forDefs, lookup', restrict, (<+>))

patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: forall a m. MonadError Error m => Ann a => Val a -> Elim a -> m (Env a × Cont a × a × Match)
match v (ElimVar x κ)
   | x == varAnon = pure (empty × κ × top × MatchVarAnon (erase v))
   | otherwise = pure (D.singleton x v × κ × top × MatchVar x (erase v))
match (Val α (V.Constr c vs)) (ElimConstr m) = do
   with "Pattern mismatch" $ singleton c `consistentWith` keys m
   κ <- lookup c m # orElse ("Incomplete patterns: no branch for " <> showCtr c)
   γ × κ' × α' × ws <- matchMany vs κ
   pure (γ × κ' × (α ∧ α') × MatchConstr c ws)
match v (ElimConstr m) = do
   d <- dataTypeFor $ keys m
   throw $ patternMismatch (prettyP v) (show d)
match (Val α (V.Record xvs)) (ElimRecord xs κ) = do
   check (subset xs (Set.fromFoldable $ keys xvs)) $ patternMismatch (show (keys xvs)) (show xs)
   let xs' = xs # Set.toUnfoldable
   γ × κ' × α' × ws <- matchMany (xs' <#> flip get xvs) κ
   pure (γ × κ' × (α ∧ α') × MatchRecord (D.fromFoldable (zip xs' ws)))
match v (ElimRecord xs _) = throw $ patternMismatch (prettyP v) (show xs)

matchMany :: forall a m. MonadError Error m => Ann a => List (Val a) -> Cont a -> m (Env a × Cont a × a × List Match)
matchMany Nil κ = pure (empty × κ × top × Nil)
matchMany (v : vs) (ContElim σ) = do
   γ × κ' × α × w <- match v σ
   γ' × κ'' × β × ws <- matchMany vs κ'
   pure $ γ `disjointUnion` γ' × κ'' × (α ∧ β) × (w : ws)
matchMany (_ : vs) (ContExpr _) = throw $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"
matchMany _ _ = error absurd

closeDefs :: forall a. Env a -> Dict (Elim a) -> a -> Env a
closeDefs γ ρ α = ρ <#> \σ ->
   let ρ' = ρ `forDefs` σ in Val α (V.Fun $ V.Closure (restrict (fv ρ' ∪ fv σ) γ) ρ' σ)

checkArity :: forall m. MonadError Error m => Ctr -> Int -> m Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) (showCtr c <> " got " <> show n <> " argument(s), expects at most " <> show n')

apply :: forall a m. MonadError Error m => Ann a => Val a × Val a -> m (AppTrace × Val a)
apply (Val β (V.Fun (V.Closure γ1 ρ σ)) × v) = do
   let γ2 = closeDefs γ1 ρ β
   γ3 × e'' × β' × w <- match v σ
   t'' × v'' <- eval (γ1 <+> γ2 <+> γ3) (asExpr e'') (β ∧ β')
   pure $ T.AppClosure (Set.fromFoldable (keys ρ)) w t'' × v''
apply (Val α (V.Fun (V.Foreign (ForeignOp (id × φ)) vs)) × v) = do
   t × v'' <- runExists apply' φ
   pure $ T.AppForeign (length vs + 1) t × v''
   where
   vs' = vs <> singleton v

   apply' :: forall t. ForeignOp' t -> m (ForeignTrace × Val _)
   apply' (ForeignOp' φ') = do
      t × v'' <- do
         if φ'.arity > length vs' then pure $ Nothing × Val α (V.Fun (V.Foreign (ForeignOp (id × φ)) vs'))
         else first Just <$> φ'.op vs'
      pure $ ForeignTrace (id × mkExists (ForeignTrace' (ForeignOp' φ') t)) × v''
apply (Val α (V.Fun (V.PartialConstr c vs)) × v) = do
   check (length vs < n) ("Too many arguments to " <> showCtr c)
   pure $ T.AppConstr c × v'
   where
   n = successful (arity c)
   v' =
      if length vs < n - 1 then Val α (V.Fun $ V.PartialConstr c (vs <> singleton v))
      else Val α (V.Constr c (vs <> singleton v))
apply (_ × v) = throw $ "Found " <> prettyP v <> ", expected function"

apply2 :: forall a m. MonadError Error m => Ann a => Val a × Val a × Val a -> m ((AppTrace × AppTrace) × Val a)
apply2 (u1 × v1 × v2) = do
   t1 × u2 <- apply (u1 × v1)
   t2 × v <- apply (u2 × v2)
   pure $ (t1 × t2) × v

eval :: forall a m. MonadError Error m => Ann a => Env a -> Expr a -> a -> m (Trace × Val a)
eval γ (Var x) _ = (T.Var x × _) <$> lookup' x γ
eval γ (Op op) _ = (T.Op op × _) <$> lookup' op γ
eval _ (Int α n) α' = pure (T.Const × Val (α ∧ α') (V.Int n))
eval _ (Float α n) α' = pure (T.Const × Val (α ∧ α') (V.Float n))
eval _ (Str α str) α' = pure (T.Const × Val (α ∧ α') (V.Str str))
eval γ (Record α xes) α' = do
   xts × xvs <- traverse (flip (eval γ) α') xes <#> D.unzip
   pure $ T.Record xts × Val (α ∧ α') (V.Record xvs)
eval γ (Dictionary α ees) α' = do
   (ts × vs) × (ts' × us) <- traverse (traverse (flip (eval γ) α')) ees <#> (P.unzip >>> (unzip # both))
   let
      ss × αs = vs <#> unpack string # unzip
      d = D.fromFoldable $ zip ss (zip αs us)
   pure $ T.Dictionary (zip ss (zip ts ts')) (d <#> snd >>> erase) × Val (α ∧ α') (V.Dictionary (DictRep d))
eval γ (Constr α c es) α' = do
   checkArity c (length es)
   ts × vs <- traverse (flip (eval γ) α') es <#> unzip
   pure (T.Constr c ts × Val (α ∧ α') (V.Constr c vs))
eval γ (Matrix α e (x × y) e') α' = do
   t × Val _ v <- eval γ e' α'
   let (i' × β) × (j' × β') = intPair.unpack v
   check (i' × j' >= 1 × 1) ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
   tss × vss <- unzipToArray <$> ((<$>) unzipToArray) <$>
      ( sequence do
           i <- range 1 i'
           singleton $ sequence do
              j <- range 1 j'
              let γ' = D.singleton x (Val β (V.Int i)) `disjointUnion` (D.singleton y (Val β' (V.Int j)))
              singleton (eval (γ <+> γ') e α')
      )
   pure $ T.Matrix tss (x × y) (i' × j') t × Val (α ∧ α') (V.Matrix (MatrixRep (vss × (i' × β) × (j' × β'))))
   where
   unzipToArray :: forall b c. List (b × c) -> Array b × Array c
   unzipToArray = unzip >>> bimap A.fromFoldable A.fromFoldable
eval γ (Lambda α σ) α' =
   pure $ T.Const × Val (α ∧ α') (V.Fun (V.Closure (restrict (fv σ) γ) empty σ))
eval γ (Project e x) α = do
   t × v <- eval γ e α
   case v of
      Val _ (V.Record xvs) -> (T.Project t x × _) <$> lookup' x xvs
      _ -> throw $ "Found " <> prettyP v <> ", expected record"
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
eval γ (LetRec (RecDefs α ρ) e) α' = do
   let γ' = closeDefs γ ρ (α ∧ α')
   t × v <- eval (γ <+> γ') e (α ∧ α')
   pure $ T.LetRec (RecDefs unit $ erase <$> ρ) t × v
