module EvalGraph where

import Prelude (bind, const, discard, flip, otherwise, pure, show, (#), ($), (+), (<#>), (<>), (==))
import Bindings (varAnon)
import Expr (Cont(..), Elim(..), Expr)
import Graph (Vertex(..))
import Control.Monad.State (State, get, put, runState)
import Data.Either (note)
import Data.List (List(..), length, (:))
import Data.Set as S
import Data.Set (Set)
import Data.Traversable (traverse)
import DataType (consistentWith, dataTypeFor, showCtr)
import Dict (disjointUnion, empty, get, keys, lookup, singleton) as D
import Util (MayFail, error, type (×), (×), with, report, check)
import Pretty
import Val (Val(..)) as V
import Val (Val, Env)

{-# Allocating addresses #-}
type Fresh a = State Int a

freshVertex :: Fresh Vertex
freshVertex = do
   s <- get
   put (s + 1)
   pure (Vertex (show s))

runAlloc :: forall a. Expr a -> (Expr Vertex) × Int
runAlloc e = runState (alloc e) 0

alloc :: forall a. Expr a -> Fresh (Expr Vertex)
alloc = traverse (const freshVertex)

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
