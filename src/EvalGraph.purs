module EvalGraph
   ( apply
   , eval
   , eval'
   , match
   , matchMany
   , patternMismatch
   ) where

import Bindings (varAnon)
import Control.Monad.Except (except)
import Control.Monad.State (State)
import Control.Monad.Trans.Class (lift)
import Data.Array (fromFoldable, foldM, range, snoc) as A
import Data.Either (note)
import Data.Exists (runExists)
import Data.List (List(..), (:), length, foldM, snoc, unzip, zip)
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (traverse)
import Data.Tuple (fst)
import DataType (checkArity, arity, consistentWith, dataTypeFor, showCtr)
import Dict (disjointUnion, fromFoldable, empty, get, keys, lookup, insert, singleton) as D
import Expr (Cont(..), Elim(..), Expr(..), VarDef(..), RecDefs, fv, asExpr)
import Foreign.Object (foldM) as D
import Graph (Vertex, class Graph, GraphAccumT, HeapT, extendG, fresh)
import Graph (extend) as G
import Prelude hiding (apply)
import Pretty (prettyP)
import Primitive (string, intPair)
import Util (type (+), type (×), MayFail, MayFailT, check, error, report, successful, unimplemented, with, (×))
import Util.Pair (unzip) as P
import Val (Val(..), Fun(..)) as V
import Val (Val, Env, lookup', for, restrict, (<+>), ForeignOp'(..))

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

closeDefs :: forall g. Graph g => g -> Env Vertex -> RecDefs Vertex -> Set Vertex -> HeapT ((+) String) (g × Env Vertex)
closeDefs g0 γ0 ρ0 αs =
   D.foldM
      ( \(g × γ) x_i σ_i -> do
           α_i <- fresh
           let ρ_i = ρ0 `for` σ_i
           let v_i = V.Fun $ V.Closure α_i (γ0 `restrict` (fv ρ_i `S.union` fv σ_i)) ρ_i σ_i
           pure $ (G.extend α_i αs g) × (D.insert x_i v_i γ)
      )
      (g0 × D.empty)
      ρ0

{-# Evaluation #-}
apply :: forall g. Graph g => g -> Val Vertex × Val Vertex -> HeapT ((+) String) (g × Val Vertex)
apply g2 (V.Fun (V.Closure α γ1 ρ σ) × v) = do
   g3 × γ2 <- closeDefs g2 γ1 ρ (S.singleton α)
   γ3 × κ × αs <- lift $ match v σ
   eval g3 (γ1 <+> γ2 <+> γ3) (asExpr κ) (S.insert α αs)
apply g (V.Fun (V.PartialConstr α c vs) × v) = do
   let n = successful (arity c)
   lift $ check (length vs < n) ("Too many arguments to " <> showCtr c)
   let
      v' =
         if length vs < n - 1 then V.Fun $ V.PartialConstr α c (snoc vs v)
         else V.Constr α c (snoc vs v)
   pure $ g × v'
apply g (V.Fun (V.Foreign φ vs) × v) = do
   let vs' = snoc vs v
   let
      apply' :: forall t. ForeignOp' t -> HeapT ((+) String) (g × Val Vertex)
      apply' (ForeignOp' φ') =
         if φ'.arity > length vs' then pure $ g × V.Fun (V.Foreign φ vs')
         else φ'.op' (g × vs')
   runExists apply' φ
apply _ (_ × v) = lift $ report $ "Found " <> prettyP v <> ", expected function"

eval' :: forall g. Graph g => Env Vertex -> Expr Vertex -> Set Vertex -> MayFailT (GraphAccumT g (State Int)) (Val Vertex)
eval' γ (Var x) _ = except $ lookup' x γ
eval' γ (Op op) _ = except $ lookup' op γ
eval' _ (Int α n) αs = V.Int <$> lift (extendG (S.insert α αs)) <@> n
eval' _ (Float α n) αs = V.Float <$> lift (extendG (S.insert α αs)) <@> n
eval' _ (Str α s) αs = V.Str <$> lift (extendG (S.insert α αs)) <@> s
eval' γ (Record α xes) αs = do
   xvs <- traverse (flip (eval' γ) αs) xes
   V.Record <$> lift (extendG (S.insert α αs)) <@> xvs
eval' γ (Dictionary α ees) αs = do
   vs × us <- traverse (traverse (flip (eval' γ) αs)) ees <#> P.unzip
   let
      ss × βs = (vs <#> string.match) # unzip
      d = D.fromFoldable $ zip ss (zip βs us)
   V.Dictionary <$> lift (extendG (S.insert α αs)) <@> d
eval' γ (Constr α c es) αs = do
   except $ checkArity c (length es)
   vs <- traverse (flip (eval' γ) αs) es
   V.Constr <$> lift (extendG (S.insert α αs)) <@> c <@> vs
eval' _ _ _ = error unimplemented

eval :: forall g. Graph g => g -> Env Vertex -> Expr Vertex -> Set Vertex -> HeapT ((+) String) (g × Val Vertex)
eval g γ (Matrix α e (x × y) e') αs = do
   α' <- fresh
   g' × v <- eval g γ e' αs
   let (m × β) × (n × β') = fst (intPair.match v)
   lift $ check (m × n >= 1 × 1)
      ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (m × n) <> ")")
   g_mn × vss <-
      A.foldM
         ( \(g_i × vss) i -> do
              g_i' × vs_i <-
                 A.foldM
                    ( \(g_ij × vs) j -> do
                         let γ' = D.singleton x (V.Int β i) `D.disjointUnion` (D.singleton y (V.Int β' j))
                         g_ij' × v_ij <- eval g_ij (γ <+> γ') e αs
                         pure $ g_ij' × A.snoc vs v_ij
                    )
                    (g_i × A.fromFoldable [])
                    (A.range 1 n)
              pure $ g_i' × A.snoc vss vs_i
         )
         (g' × A.fromFoldable [])
         (A.range 1 m)
   pure $ (G.extend α' (S.insert α αs) g_mn) × V.Matrix α' (vss × (m × β) × (n × β'))
eval g γ (Lambda σ) αs = do
   α' <- fresh
   pure $ (G.extend α' αs g) × V.Fun (V.Closure α' (γ `restrict` fv σ) D.empty σ)
eval g γ (Project e x) αs = do
   g' × v <- eval g γ e αs
   lift $ case v of
      V.Record _ xvs -> ((×) g') <$> lookup' x xvs
      _ -> report $ "Found " <> prettyP v <> ", expected record"
eval g γ (App e e') αs = do
   g1 × cls <- eval g γ e αs
   g2 × v <- eval g1 γ e' αs
   apply g2 (cls × v)
eval g γ (Let (VarDef σ e) e') αs = do
   g1 × v <- eval g γ e αs
   γ' × _ × _ <- lift $ match v σ -- terminal meta-type of eliminator is meta-unit
   eval g1 (γ <+> γ') e' αs
eval g γ (LetRec ρ e) αs = do
   g1 × γ' <- closeDefs g γ ρ αs
   eval g1 (γ <+> γ') e αs
eval _ _ _ _ = error unimplemented
