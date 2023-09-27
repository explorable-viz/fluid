module EvalGraph
   ( GraphConfig
   , apply
   , eval
   , eval_module
   , graphGC
   , match
   , patternMismatch
   ) where

import Prelude hiding (apply, add)

import Bindings (varAnon)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (range, singleton) as A
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.List (List(..), length, snoc, unzip, zip, (:))
import Data.Set (Set, empty, insert, intersection, singleton, union)
import Data.Set as S
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst)
import DataType (checkArity, arity, consistentWith, dataTypeFor, showCtr)
import Dict (disjointUnion, fromFoldable, empty, get, keys, lookup, singleton) as D
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), Expr(..), VarDef(..), RecDefs, Module(..), fv, asExpr)
import GaloisConnection (GaloisConnection)
import Graph (class Graph, Vertex, sinks)
import Graph (vertices) as G
import Graph.GraphWriter (class MonadGraphAlloc, alloc, new, runWithGraphAllocT)
import Graph.Slice (bwdSlice, fwdSlice, selectαs, select𝔹s, vertices)
import Lattice (Raw, 𝔹, botOf)
import Pretty (prettyP)
import Primitive (string, intPair)
import Util (type (×), check, error, orElse, successful, throw, with, (×))
import Util.Pair (unzip) as P
import Val (DictRep(..), Env, ForeignOp'(..), MatrixRep(..), Val, for, lookup', restrict, (<+>))
import Val (Val(..), Fun(..)) as V

type GraphConfig g =
   { g :: g
   , n :: Int
   , γα :: Env Vertex
   }

{-# Matching #-}
patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: forall m. MonadGraphAlloc m => Val Vertex -> Elim Vertex -> m (Env Vertex × Cont Vertex × Set Vertex)
match v (ElimVar x κ)
   | x == varAnon = pure (D.empty × κ × empty)
   | otherwise = pure (D.singleton x v × κ × empty)
match (V.Constr α c vs) (ElimConstr m) = do
   with "Pattern mismatch" $ S.singleton c `consistentWith` D.keys m
   κ <- D.lookup c m # orElse ("Incomplete patterns: no branch for " <> showCtr c)
   γ × κ' × αs <- matchMany vs κ
   pure (γ × κ' × (insert α αs))
match v (ElimConstr m) = do
   d <- dataTypeFor $ D.keys m
   throw $ patternMismatch (prettyP v) (show d)
match (V.Record α xvs) (ElimRecord xs κ) = do
   check (S.subset xs (S.fromFoldable $ D.keys xvs))
      $ patternMismatch (show (D.keys xvs)) (show xs)
   let xs' = xs # S.toUnfoldable
   γ × κ' × αs <- matchMany (flip D.get xvs <$> xs') κ
   pure $ γ × κ' × (insert α αs)
match v (ElimRecord xs _) = throw (patternMismatch (prettyP v) (show xs))

matchMany :: forall m. MonadGraphAlloc m => List (Val Vertex) -> Cont Vertex -> m (Env Vertex × Cont Vertex × Set Vertex)
matchMany Nil κ = pure (D.empty × κ × empty)
matchMany (v : vs) (ContElim σ) = do
   γ × κ × αs <- match v σ
   γ' × κ' × βs <- matchMany vs κ
   pure $ γ `D.disjointUnion` γ' × κ' × (αs `union` βs)
matchMany (_ : vs) (ContExpr _) = throw $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"
matchMany _ _ = error "absurd"

closeDefs :: forall m. MonadGraphAlloc m => Env Vertex -> RecDefs Vertex -> Set Vertex -> m (Env Vertex)
closeDefs γ ρ αs =
   flip traverse ρ \σ ->
      let
         ρ' = ρ `for` σ
      in
         V.Fun <$> new αs <@> V.Closure (γ `restrict` (fv ρ' `union` fv σ)) ρ' σ

{-# Evaluation #-}
apply :: forall m. MonadGraphAlloc m => Val Vertex -> Val Vertex -> m (Val Vertex)
apply (V.Fun α (V.Closure γ1 ρ σ)) v = do
   γ2 <- closeDefs γ1 ρ (singleton α)
   γ3 × κ × αs <- match v σ
   eval (γ1 <+> γ2 <+> γ3) (asExpr κ) (insert α αs)
apply (V.Fun α (V.Foreign φ vs)) v =
   runExists apply' φ
   where
   vs' = snoc vs v

   apply' :: forall t. ForeignOp' t -> m (Val Vertex)
   apply' (ForeignOp' φ') =
      if φ'.arity > length vs' then
         V.Fun <$> new (singleton α) <@> V.Foreign φ vs'
      else φ'.op' vs'
apply (V.Fun α (V.PartialConstr c vs)) v = do
   check (length vs < n) ("Too many arguments to " <> showCtr c)
   if length vs < n - 1 then V.Fun <$> new (singleton α) <@> V.PartialConstr c (snoc vs v)
   else pure $ V.Constr α c (snoc vs v)
   where
   n = successful (arity c)
apply _ v = throw $ "Found " <> prettyP v <> ", expected function"

eval :: forall m. MonadGraphAlloc m => Env Vertex -> Expr Vertex -> Set Vertex -> m (Val Vertex)
eval γ (Var x) _ = lookup' x γ
eval γ (Op op) _ = lookup' op γ
eval _ (Int α n) αs = V.Int <$> new (insert α αs) <@> n
eval _ (Float α n) αs = V.Float <$> new (insert α αs) <@> n
eval _ (Str α s) αs = V.Str <$> new (insert α αs) <@> s
eval γ (Record α xes) αs = do
   xvs <- traverse (flip (eval γ) αs) xes
   V.Record <$> new (insert α αs) <@> xvs
eval γ (Dictionary α ees) αs = do
   vs × us <- traverse (traverse (flip (eval γ) αs)) ees <#> P.unzip
   let
      ss × βs = (vs <#> string.match) # unzip
      d = D.fromFoldable $ zip ss (zip βs us)
   V.Dictionary <$> new (insert α αs) <@> DictRep d
eval γ (Constr α c es) αs = do
   checkArity c (length es)
   vs <- traverse (flip (eval γ) αs) es
   α' <- new (insert α αs)
   pure $ V.Constr α' c vs
eval γ (Matrix α e (x × y) e') αs = do
   v <- eval γ e' αs
   let (i' × β) × (j' × β') = fst (intPair.match v)
   check
      (i' × j' >= 1 × 1)
      ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
   vss <- sequence $ do
      i <- A.range 1 i'
      A.singleton $ sequence $ do
         j <- A.range 1 j'
         let γ' = D.singleton x (V.Int β i) `D.disjointUnion` (D.singleton y (V.Int β' j))
         A.singleton (eval (γ <+> γ') e αs)
   V.Matrix <$> new (insert α αs) <@> MatrixRep (vss × (i' × β) × (j' × β'))
eval γ (Lambda σ) αs =
   V.Fun <$> new αs <@> V.Closure (γ `restrict` fv σ) D.empty σ
eval γ (Project e x) αs = do
   v <- eval γ e αs
   case v of
      V.Record _ xvs -> lookup' x xvs
      _ -> throw $ "Found " <> prettyP v <> ", expected record"
eval γ (App e e') αs = do
   v <- eval γ e αs
   v' <- eval γ e' αs
   apply v v'
eval γ (Let (VarDef σ e) e') αs = do
   v <- eval γ e αs
   γ' × _ × αs' <- match v σ -- terminal meta-type of eliminator is meta-unit
   eval (γ <+> γ') e' αs' -- (αs ∧ αs') for consistency with functions? (similarly for module defs)
eval γ (LetRec ρ e) αs = do
   γ' <- closeDefs γ ρ αs
   eval (γ <+> γ') e αs

eval_module :: forall m. MonadGraphAlloc m => Env Vertex -> Module Vertex -> Set Vertex -> m (Env Vertex)
eval_module γ = go D.empty
   where
   go :: Env Vertex -> Module Vertex -> Set Vertex -> m (Env Vertex)
   go γ' (Module Nil) _ = pure γ'
   go y' (Module (Left (VarDef σ e) : ds)) αs = do
      v <- eval (γ <+> y') e αs
      γ'' × _ × α' <- match v σ
      go (y' <+> γ'') (Module ds) α'
   go γ' (Module (Right ρ : ds)) αs = do
      γ'' <- closeDefs (γ <+> γ') ρ αs
      go (γ' <+> γ'') (Module ds) αs

type EvalGaloisConnection g a b = GaloisConnection a b
   ( selecte𝔹 :: a -> Expr 𝔹
   , selectv𝔹 :: b -> Val 𝔹
   , runδv :: (Val 𝔹 -> Val 𝔹) -> b
   , g :: g
   )

graphGC
   :: forall g m
    . MonadError Error m
   => Graph g
   => GraphConfig g
   -> Raw Expr
   -> m (EvalGaloisConnection g (Set Vertex) (Set Vertex))
graphGC { g, n, γα } e = do
   (g' × _) × eα × vα <- do
      runWithGraphAllocT (g × n) $ do
         eα <- alloc e
         vα <- eval γα eα S.empty
         pure (eα × vα)
   let
      selecte𝔹 αs = select𝔹s eα αs
      selectv𝔹 αs = select𝔹s vα αs
      runδv δv = selectαs (δv (botOf vα)) vα
      fwd αs = G.vertices (fwdSlice αs g') `intersection` vertices vα
      -- TODO: want (vertices eα `union` foldMap vertices γα) rather than sinks g' here?
      bwd αs = G.vertices (bwdSlice αs g') `intersection` sinks g'
   pure { selecte𝔹, selectv𝔹, runδv, g: g', fwd, bwd }
