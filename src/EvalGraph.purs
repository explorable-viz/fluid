module EvalGraph where

import Prelude (bind, const, discard, flip, otherwise, pure, show, (#), ($), (+), (<#>), (<>), (==))
import Bindings (varAnon)
import Expr (Cont(..), Elim(..), Expr(..))
import Graph (Vertex, class Graph, Heap, HeapT, fresh)
import Control.Monad.State (runState)
import Control.Monad.Trans.Class (lift)
import Data.Functor ((<$>))
import Data.Either (Either, note)
import Data.List (List(..), length, (:))
import Data.Set as S
import Data.Set (Set)
import Data.Traversable (class Traversable, traverse)
import DataType (consistentWith, dataTypeFor, showCtr)
import Dict (disjointUnion, empty, get, keys, lookup, singleton) as D
import Util (MayFail, error, type (×), (×), with, report, check)
import Pretty
import Val (Val(..)) as V
import Val (Val, Env, lookup')

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
eval :: forall g. Graph g => g -> Env Vertex -> Expr Vertex -> Set Vertex -> HeapT (Either String) (g × Val Vertex)
eval g γ (Var x) _ = ((×) g) <$> lift (lookup' x γ)
eval g γ (Op op) _ = ((×) g) <$> lift (lookup' op γ)
-- eval g γ (Int α n) verts = do
--    α' <- freshVertex
--    -- pure (g)
--    error "todo"
eval _ _ _ _ = error "to do"
-- eval _ (Float α n) α' = pure (T.Const × V.Float (α ∧ α') n)
-- eval _ (Str α str) α' = pure (T.Const × V.Str (α ∧ α') str)
-- eval γ (Record α xes) α' = do
--    xts × xvs <- traverse (flip (eval γ) α') xes <#> D.unzip
--    pure $ T.Record xts × V.Record (α ∧ α') xvs
-- eval γ (Dictionary α ees) α' = do
--    (ts × vs) × (ts' × us) <- traverse (traverse (flip (eval γ) α')) ees <#> (P.unzip >>> (unzip # both))
--    let
--       ss × αs = (vs <#> \u -> string.match u) # unzip
--       d = D.fromFoldable $ zip ss (zip αs us)
--    pure $ T.Dictionary (zip ss (zip ts ts')) (d <#> snd >>> erase) × V.Dictionary (α ∧ α') d
-- eval γ (Constr α c es) α' = do
--    checkArity c (length es)
--    ts × vs <- traverse (flip (eval γ) α') es <#> unzip
--    pure (T.Constr c ts × V.Constr (α ∧ α') c vs)
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
-- eval γ (Lambda σ) α =
--    pure $ T.Const × V.Fun (V.Closure α (γ `restrict` fv σ) empty σ)
-- eval γ (Project e x) α = do
--    t × v <- eval γ e α
--    case v of
--       V.Record _ xvs -> (T.Project t x × _) <$> lookup' x xvs
--       _ -> report $ "Found " <> prettyP v <> ", expected record"
-- eval γ (App e e') α = do
--    t × v <- eval γ e α
--    t' × v' <- eval γ e' α
--    t'' × v'' <- apply (v × v')
--    pure $ T.App t t' t'' × v''
-- eval γ (Let (VarDef σ e) e') α = do
--    t × v <- eval γ e α
--    γ' × _ × α' × w <- match v σ -- terminal meta-type of eliminator is meta-unit
--    t' × v' <- eval (γ <+> γ') e' α' -- (α ∧ α') for consistency with functions? (similarly for module defs)
--    pure $ T.Let (T.VarDef w t) t' × v'
-- eval γ (LetRec ρ e) α = do
--    let γ' = closeDefs γ ρ α
--    t × v <- eval (γ <+> γ') e α
--    pure $ T.LetRec (erase <$> ρ) t × v
