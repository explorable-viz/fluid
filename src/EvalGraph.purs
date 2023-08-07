module EvalGraph
   ( apply
   , eval
   , match
   , matchMany
   , patternMismatch
   ) where

import Bindings (varAnon)
import Control.Monad.Except (except)
import Control.Monad.Trans.Class (lift)
import Data.Array (range, singleton) as A
import Data.Either (note)
import Data.Exists (runExists)
import Data.List (List(..), (:), length, snoc, unzip, zip)
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst)
import DataType (checkArity, arity, consistentWith, dataTypeFor, showCtr)
import Dict (disjointUnion, fromFoldable, empty, get, keys, lookup, singleton) as D
import Expr (Cont(..), Elim(..), Expr(..), VarDef(..), RecDefs, fv, asExpr)
import Graph (Vertex, WithGraph2, class Graph, new)
import Prelude hiding (apply)
import Pretty (prettyP)
import Primitive (string, intPair)
import Util (type (×), MayFail, check, error, report, successful, with, (×))
import Util.Pair (unzip) as P
import Val (Val(..), Fun(..)) as V
import Val (DictRep(..), Env, MatrixRep(..), Val, lookup', for, restrict, (<+>), ForeignOp'(..))

{-# Matching #-}
patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: Val Vertex -> Elim Vertex -> MayFail (Env Vertex × Cont Vertex × Set Vertex)
match v (ElimVar x κ)
   | x == varAnon = pure (D.empty × κ × S.empty)
   | otherwise = pure (D.singleton x v × κ × S.empty)
match (V.Constr α c vs) (ElimConstr m) = do
   with "Pattern mismatch" $ S.singleton c `consistentWith` D.keys m
   κ <- note ("Incomplete patterns: no branch for " <> showCtr c) (D.lookup c m)
   γ × κ' × αs <- matchMany vs κ
   pure (γ × κ' × (S.insert α αs))
match v (ElimConstr m) = do
   d <- dataTypeFor $ D.keys m
   report $ patternMismatch (prettyP v) (show d)
match (V.Record α xvs) (ElimRecord xs κ) = do
   check (S.subset xs (S.fromFoldable $ D.keys xvs))
      $ patternMismatch (show (D.keys xvs)) (show xs)
   let xs' = xs # S.toUnfoldable :: List String
   γ × κ' × αs <- matchMany (flip D.get xvs <$> xs') κ
   pure $ γ × κ' × (S.insert α αs)
match v (ElimRecord xs _) = report (patternMismatch (prettyP v) (show xs))

matchMany :: List (Val Vertex) -> Cont Vertex -> MayFail (Env Vertex × Cont Vertex × Set Vertex)
matchMany Nil κ = pure (D.empty × κ × S.empty)
matchMany (v : vs) (ContElim σ) = do
   γ × κ × αs <- match v σ
   γ' × κ' × βs <- matchMany vs κ
   pure $ γ `D.disjointUnion` γ' × κ' × (αs `S.union` βs)
matchMany (_ : vs) (ContExpr _) = report $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"
matchMany _ _ = error "absurd"

closeDefs :: forall g. Graph g => Env Vertex -> RecDefs Vertex -> Set Vertex -> WithGraph2 g (Env Vertex)
closeDefs γ ρ αs =
   flip traverse ρ \σ ->
      let
         ρ' = ρ `for` σ
      in
         V.Fun <$> (V.Closure <$> lift (new αs) <@> (γ `restrict` (fv ρ' `S.union` fv σ)) <@> ρ' <@> σ)

{-# Evaluation #-}
apply :: forall g. Graph g => Val Vertex -> Val Vertex -> WithGraph2 g (Val Vertex)
apply (V.Fun (V.Closure α γ1 ρ σ)) v = do
   γ2 <- closeDefs γ1 ρ (S.singleton α)
   γ3 × κ × αs <- except $ match v σ
   eval (γ1 <+> γ2 <+> γ3) (asExpr κ) (S.insert α αs)
apply (V.Fun (V.PartialConstr α c vs)) v = do
   let n = successful (arity c)
   except $ check (length vs < n) ("Too many arguments to " <> showCtr c)
   let
      v' =
         if length vs < n - 1 then V.Fun $ V.PartialConstr α c (snoc vs v)
         else V.Constr α c (snoc vs v)
   pure v'
apply (V.Fun (V.Foreign φ vs)) v = do
   let vs' = snoc vs v
   let
      apply' :: forall t. ForeignOp' t -> WithGraph2 g (Val Vertex)
      apply' (ForeignOp' φ') =
         if φ'.arity > length vs' then pure $ V.Fun (V.Foreign φ vs')
         else φ'.op' vs'
   runExists apply' φ
apply _ v = except $ report $ "Found " <> prettyP v <> ", expected function"

eval :: forall g. Graph g => Env Vertex -> Expr Vertex -> Set Vertex -> WithGraph2 g (Val Vertex)
eval γ (Var x) _ = except $ lookup' x γ
eval γ (Op op) _ = except $ lookup' op γ
eval _ (Int α n) αs = V.Int <$> lift (new (S.insert α αs)) <@> n
eval _ (Float α n) αs = V.Float <$> lift (new (S.insert α αs)) <@> n
eval _ (Str α s) αs = V.Str <$> lift (new (S.insert α αs)) <@> s
eval γ (Record α xes) αs = do
   xvs <- traverse (flip (eval γ) αs) xes
   V.Record <$> lift (new (S.insert α αs)) <@> xvs
eval γ (Dictionary α ees) αs = do
   vs × us <- traverse (traverse (flip (eval γ) αs)) ees <#> P.unzip
   let
      ss × βs = (vs <#> string.match) # unzip
      d = D.fromFoldable $ zip ss (zip βs us)
   V.Dictionary <$> lift (new (S.insert α αs)) <@> DictRep d
eval γ (Constr α c es) αs = do
   except $ checkArity c (length es)
   vs <- traverse (flip (eval γ) αs) es
   V.Constr <$> lift (new (S.insert α αs)) <@> c <@> vs
eval γ (Matrix α e (x × y) e') αs = do
   v <- eval γ e' αs
   let (i' × β) × (j' × β') = fst (intPair.match v)
   except $ check
      (i' × j' >= 1 × 1)
      ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
   vss <- sequence $ do
      i <- A.range 1 i'
      A.singleton $ sequence $ do
         j <- A.range 1 j'
         let γ' = D.singleton x (V.Int β i) `D.disjointUnion` (D.singleton y (V.Int β' j))
         A.singleton (eval (γ <+> γ') e αs)
   V.Matrix <$> lift (new (S.insert α αs)) <@> MatrixRep (vss × (i' × β) × (j' × β'))
eval γ (Lambda σ) αs =
   V.Fun <$> (V.Closure <$> lift (new αs) <@> γ `restrict` fv σ <@> D.empty <@> σ)
eval γ (Project e x) αs = do
   v <- eval γ e αs
   except $ case v of
      V.Record _ xvs -> lookup' x xvs
      _ -> report $ "Found " <> prettyP v <> ", expected record"
eval γ (App e e') αs = do
   v <- eval γ e αs
   v' <- eval γ e' αs
   apply v v'
eval γ (Let (VarDef σ e) e') αs = do
   v <- eval γ e αs
   γ' × _ × _ <- except $ match v σ -- terminal meta-type of eliminator is meta-unit
   eval (γ <+> γ') e' αs
eval γ (LetRec ρ e) αs = do
   γ' <- closeDefs γ ρ αs
   eval (γ <+> γ') e αs
