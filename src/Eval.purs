module Eval where

import Prelude hiding (absurd, apply, top)

import Bind (varAnon)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (fromFoldable) as A
import Data.Bifunctor (bimap)
import Data.Exists (mkExists, runExists)
import Data.List (List(..), (:), length, range, zip)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Profunctor.Strong (first)
import Data.Set (fromFoldable, toUnfoldable) as Set
import Data.Set (subset)
import Data.Traversable (sequence, traverse)
import Data.Tuple (snd)
import DataType (Ctr, arity, consistentWith, dataTypeFor, showCtr)
import Dict (Dict)
import Dict (fromFoldable) as D
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), Expr(..), RecDefs(..), VarDef(..), asExpr, fv)
import Lattice ((∧), erase, top)
import Pretty (prettyP)
import Primitive (intPair, string, unpack)
import Trace (AppTrace(..), Trace(..), VarDef(..)) as T
import Trace (AppTrace, ForeignTrace(..), ForeignTrace'(..), Match(..), Trace)
import Util (type (×), (×), both, check, orElse, singleton, defined, throw, unzip, with)
import Util.Map (disjointUnion, get, keys, lookup, lookup', maplet, restrict, (<+>))
import Util.Pair (unzip) as P
import Util.Set (empty, (∪))
import Val (BaseVal(..), Fun(..)) as V
import Val (class Ann, DictRep(..), Env(..), EnvExpr(..), ForeignOp(..), ForeignOp'(..), MatrixRep(..), Val(..), forDefs)

patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: forall a m. MonadError Error m => Ann a => Val a -> Elim a -> m (Env a × Cont a × a × Match)
match v (ElimVar x κ)
   | x == varAnon = pure (empty × κ × top × MatchVarAnon (erase v))
   | otherwise = pure (maplet x v × κ × top × MatchVar x (erase v))
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
   pure (γ × κ' × (α ∧ α') × MatchRecord (wrap $ D.fromFoldable (zip xs' ws)))
match v (ElimRecord xs _) = throw $ patternMismatch (prettyP v) (show xs)

matchMany :: forall a m. MonadError Error m => Ann a => List (Val a) -> Cont a -> m (Env a × Cont a × a × List Match)
matchMany Nil κ = pure (empty × κ × top × Nil)
matchMany (v : vs) (ContElim σ) = do
   γ × κ' × α × w <- match v σ
   γ' × κ'' × β × ws <- matchMany vs κ'
   pure $ γ `disjointUnion` γ' × κ'' × (α ∧ β) × (w : ws)
matchMany (_ : vs) (ContExpr _) = throw $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"

closeDefs :: forall a. Env a -> Dict (Elim a) -> a -> Env a
closeDefs γ ρ α = Env
   ( ρ <#> \σ ->
        let ρ' = ρ `forDefs` σ in Val α (V.Fun $ V.Closure (restrict (fv ρ' ∪ fv σ) γ) ρ' σ)
   )

checkArity :: forall m. MonadError Error m => Ctr -> Int -> m Unit
checkArity c n = do
   n' <- arity c
   check (n' >= n) (showCtr c <> " got " <> show n <> " argument(s), expects at most " <> show n')

apply :: forall a m. MonadError Error m => Ann a => Val a × Val a -> m (AppTrace × Val a)
apply (Val β (V.Fun (V.Closure γ1 ρ σ)) × v) = do
   let γ2 = closeDefs γ1 ρ β
   γ3 × e'' × β' × w <- match v σ
   t'' × v'' <- eval (EnvExpr (γ1 <+> γ2 <+> γ3) (asExpr e'')) (β ∧ β')
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
   n = defined (arity c)
   v' =
      if length vs < n - 1 then Val α (V.Fun $ V.PartialConstr c (vs <> singleton v))
      else Val α (V.Constr c (vs <> singleton v))
apply (_ × v) = throw $ "Found " <> prettyP v <> ", expected function"

apply2 :: forall a m. MonadError Error m => Ann a => Val a × Val a × Val a -> m ((AppTrace × AppTrace) × Val a)
apply2 (u1 × v1 × v2) = do
   t1 × u2 <- apply (u1 × v1)
   t2 × v <- apply (u2 × v2)
   pure $ (t1 × t2) × v

eval :: forall a m. MonadError Error m => Ann a => EnvExpr a -> a -> m (Trace × Val a)
eval (EnvExpr γ (Var x)) _ = (T.Var x × _) <$> lookup' x γ
eval (EnvExpr γ (Op op)) _ = (T.Op op × _) <$> lookup' op γ
eval (EnvExpr _ (Int α n)) α' = pure (T.Const × Val (α ∧ α') (V.Int n))
eval (EnvExpr _ (Float α n)) α' = pure (T.Const × Val (α ∧ α') (V.Float n))
eval (EnvExpr _ (Str α str)) α' = pure (T.Const × Val (α ∧ α') (V.Str str))
eval (EnvExpr γ (Record α xes)) α' = do
   xts × xvs <- traverse (\e -> eval (EnvExpr γ e) α') xes <#> unzip
   pure $ T.Record xts × Val (α ∧ α') (V.Record xvs)
eval (EnvExpr γ (Dictionary α ees)) α' = do
   (ts × vs) × (ts' × us) <- traverse (traverse (\e -> eval (EnvExpr γ e) α')) ees <#> (P.unzip >>> (unzip # both))
   let
      ss × αs = vs <#> unpack string # unzip
      d = wrap $ D.fromFoldable $ zip ss (zip αs us)
   pure $ T.Dictionary (zip ss (zip ts ts')) (d <#> snd >>> erase) × Val (α ∧ α') (V.Dictionary (DictRep d))
eval (EnvExpr γ (Constr α c es)) α' = do
   checkArity c (length es)
   ts × vs <- traverse (\e -> eval (EnvExpr γ e) α') es <#> unzip
   pure (T.Constr c ts × Val (α ∧ α') (V.Constr c vs))
eval (EnvExpr γ (Matrix α e (x × y) e')) α' = do
   t × Val _ v <- eval (EnvExpr γ e') α'
   let (i' × β) × (j' × β') = intPair.unpack v
   check (i' × j' >= 1 × 1) ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
   tss × vss <- unzipToArray <$> ((<$>) unzipToArray) <$>
      ( sequence do
           i <- range 1 i'
           singleton $ sequence do
              j <- range 1 j'
              let γ' = maplet x (Val β (V.Int i)) `disjointUnion` (maplet y (Val β' (V.Int j)))
              singleton (eval (EnvExpr (γ <+> γ') e) α')
      )
   pure $ T.Matrix tss (x × y) (i' × j') t × Val (α ∧ α') (V.Matrix (MatrixRep (vss × (i' × β) × (j' × β'))))
   where
   unzipToArray :: forall b c. List (b × c) -> Array b × Array c
   unzipToArray = unzip >>> bimap A.fromFoldable A.fromFoldable
eval (EnvExpr γ (Lambda α σ)) α' =
   pure $ T.Const × Val (α ∧ α') (V.Fun (V.Closure (restrict (fv σ) γ) empty σ))
eval (EnvExpr γ (Project e x)) α = do
   t × v <- eval (EnvExpr γ e) α
   case v of
      Val _ (V.Record xvs) -> (T.Project t x × _) <$> lookup' x xvs
      _ -> throw $ "Found " <> prettyP v <> ", expected record"
eval (EnvExpr γ (App e e')) α = do
   t × v <- eval (EnvExpr γ e) α
   t' × v' <- eval (EnvExpr γ e') α
   t'' × v'' <- apply (v × v')
   pure $ T.App t t' t'' × v''
eval (EnvExpr γ (Let (VarDef σ e) e')) α = do
   t × v <- eval (EnvExpr γ e) α
   γ' × _ × α' × w <- match v σ -- terminal meta-type of eliminator is meta-unit
   t' × v' <- eval (EnvExpr (γ <+> γ') e') α' -- (α ∧ α') for consistency with functions? (similarly for module defs)
   pure $ T.Let (T.VarDef w t) t' × v'
eval (EnvExpr γ (LetRec (RecDefs α ρ) e)) α' = do
   let γ' = closeDefs γ ρ (α ∧ α')
   t × v <- eval (EnvExpr (γ <+> γ') e) (α ∧ α')
   pure $ T.LetRec (RecDefs unit $ erase <$> ρ) t × v
