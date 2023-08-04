module EvalGraph where

import Pretty

import Bindings (varAnon)
import Control.Monad.State (runState)
import Control.Monad.Trans.Class (lift)
import Foreign.Object (foldM) as D
import Data.Either (Either, note)
import Data.Functor ((<$>))
import Data.List (length, range, singleton, unzip, zip, foldM)
import Data.List (List(..), (:))
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (class Traversable, traverse)
import DataType (consistentWith, checkArity, dataTypeFor, showCtr)
import Dict (disjointUnion, empty, get, keys, lookup, singleton, insert) as D
import Expr (Cont(..), Elim(..), Expr(..), fv)
import Graph (Vertex, class Graph, Heap, HeapT, fresh)
import Graph (union) as G
import Prelude (bind, const, discard, flip, otherwise, pure, show, (#), ($), (+), (<#>), (<>), (==), (<<<))
import Primitive (string)
import Util (MayFail, error, type (×), (×), with, report, check, both)
import Util.Pair (unzip) as P
import Util.Pair (Pair(..))
import Val (Val(..), Fun(..)) as V
import Val (Val, Env, lookup', restrict)

{-# Allocating addresses #-}
runAlloc :: forall t a. Traversable t => t a -> (t Vertex) × Int
runAlloc e = runState (alloc e) 0

alloc :: forall t a. Traversable t => t a -> Heap (t Vertex)
alloc = traverse (const fresh)

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
   let xs' = xs # S.toUnfoldable
   γ × κ' × αs <- matchMany (xs' <#> flip D.get xvs) κ
   pure $ γ × κ' × (S.insert α αs)
match v (ElimRecord xs _) = report (patternMismatch (prettyP v) (show xs))

matchMany :: List (Val Vertex) -> Cont Vertex -> MayFail (Env Vertex × Cont Vertex × Set Vertex)
matchMany Nil κ = pure (D.empty × κ × S.empty)
matchMany (v : vs) (ContElim σ) = do
   γ × κ' × αs <- match v σ
   γ' × κ'' × βs <- matchMany vs κ'
   pure $ γ `D.disjointUnion` γ' × κ'' × (αs `S.union` βs)
matchMany (_ : vs) (ContExpr _) = report $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"
matchMany _ _ = error "absurd"

{-# Evaluation #-}
-- evalSeq

eval :: forall g. Graph g => g -> Env Vertex -> Expr Vertex -> Set Vertex -> HeapT (Either String) (g × Val Vertex)
eval g γ (Var x) _ = ((×) g) <$> lift (lookup' x γ)
eval g γ (Op op) _ = ((×) g) <$> lift (lookup' op γ)
eval g _ (Int α n) vs = do
   α' <- fresh
   pure $ (G.union α' (S.insert α vs) g) × (V.Int α' n)
eval g _ (Float α n) vs = do
   α' <- fresh
   pure $ (G.union α' (S.insert α vs) g) × (V.Float α' n)
eval g _ (Str α str) vs = do
   α' <- fresh
   pure $ (G.union α' (S.insert α vs) g) × (V.Str α' str)
eval g γ (Record α xes) vs = do
   α' <- fresh
   g' × xvs <- D.foldM
      ( \(g_prev × xvs) x e -> do
           (g_next × val_i) <- eval g_prev γ e vs
           pure $ g_next × D.insert x val_i xvs
      )
      (g × D.empty)
      xes
   pure $ (G.union α' (S.insert α vs) g') × V.Record α' xvs
eval g γ (Dictionary α ees) vrts = do
   α' <- fresh
   g' × xvs <- foldM
      ( \(g_prev × xvs) (Pair e1 e2) -> do
           (g1 × v1) <- eval g_prev γ e1 vrts
           let s × β = error "to be replaced with <string.match v1>" :: String × Vertex {- string.match v1 -}
           (g2 × v2) <- eval g1 γ e2 vrts
           pure $ g2 × D.insert s (β × v2) xvs
      )
      (g × D.empty)
      ees
   pure $ (G.union α' (S.insert α vrts) g') × V.Dictionary α' xvs
eval g γ (Constr α c es) vrts = do
   α' <- fresh
   lift $ checkArity c (length es)
   g_n × vs <- foldM
      ( \(g_prev × vs) e -> do
           (g_next × v) <- eval g_prev γ e vrts
           pure $ g_next × (vs <> (v : Nil)) -- foldM traverses the list from left-to-right, hence we append rather than prepend onto the list of values
      )
      (g × Nil)
      es
   pure $ (G.union α' (S.insert α vrts) g_n) × (V.Constr α' c vs)
-- eval γ (Matrix α e (x × y) e') α' = do
--    t × v <- eval γ e' α'
--    let (i' × β) × (j' × β') = fst (intPair.match v)
--    check (i' × j' >= 1 × 1) ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
--    tss × vss <- unzipToArray <$> ((<$>) unzipToArray) <$>
--       ( sequence $ do
--            i <- range 1 i'
--            singleton $ sequence $ do
--               j <- range 1 j'
--               let γ' = D.singleton x (V.Int β i) `disjointUnion` (D.singleton y (V.Int β' j))
--               singleton (eval (γ <+> γ') e α')
--       )
--    pure $ T.Matrix tss (x × y) (i' × j') t × V.Matrix (α ∧ α') (vss × (i' × β) × (j' × β'))
--    where
--    unzipToArray :: forall b c. List (b × c) -> Array b × Array c
--    unzipToArray = unzip >>> bimap A.fromFoldable A.fromFoldable
-- eval g γ (Matrix α e (x × y) e') vrts = do
--    error "todo"
eval g γ (Lambda σ) vrts = do
   α' <- fresh
   pure $ (G.union α' vrts g) × V.Fun (V.Closure α' (γ `restrict` fv σ) D.empty σ)
eval g γ (Project e x) vrts = do
   g' × v <- eval g γ e vrts
   lift $ case v of
      V.Record _ xvs -> ((×) g') <$> lookup' x xvs
      _ -> report $ "Found " <> prettyP v <> ", expected record"
-- eval g γ (App e e') vrts = do
--    g1 × cls <- eval g γ e vrts
--    g2 × v   <- eval g1 γ e' vrts
--    t'' × v'' <- apply (v × v')
--    pure $ T.App t t' t'' × v''
eval _ _ _ _ = error "to do"
-- eval γ (Let (VarDef σ e) e') α = do
--    t × v <- eval γ e α
--    γ' × _ × α' × w <- match v σ -- terminal meta-type of eliminator is meta-unit
--    t' × v' <- eval (γ <+> γ') e' α' -- (α ∧ α') for consistency with functions? (similarly for module defs)
--    pure $ T.Let (T.VarDef w t) t' × v'
-- eval γ (LetRec ρ e) α = do
--    let γ' = closeDefs γ ρ α
--    t × v <- eval (γ <+> γ') e α
--    pure $ T.LetRec (erase <$> ρ) t × v
