module EvalGraph where

import Prelude hiding (apply)

import Bind (Bind, (↦), varAnon)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (range) as A
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.List (List(..), length, reverse, snoc, unzip, zip, (:))
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.Set (Set, insert)
import Data.Set as Set
import Data.Traversable (class Foldable, for, sequence, traverse)
import Data.Tuple (curry, snd)
import DataType (checkArity, arity, consistentWith, dataTypeFor, showCtr)
import Dict (Dict)
import Dict (fromFoldable) as D
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDefs(..), VarDef(..), asExpr, fv)
import GaloisConnection (GaloisConnection(..))
import Graph (class Graph, DVertex, Vertex, dvertex, insert', op, pack, selectαs', select𝔹s, showGraph, showVertices', unDVertex, vertices, vertices')
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSlice, fwdSlice)
import Graph.WithGraph (class MonadWithGraphAlloc, alloc, new, runAllocT, runWithGraphT_spy)
import Lattice (𝔹, Raw)
import Pretty (prettyP)
import Primitive (intPair, string, unpack)
import ProgCxt (ProgCxt(..))
import Test.Util.Debug (checking, tracing)
import Util (type (×), Endo, check, concatM, orElse, singleton, spyFunWhen, defined, throw, withMsg, (×), (⊆))
import Util.Map (disjointUnion, get, keys, lookup, lookup', maplet, restrict, (<+>))
import Util.Pair (unzip) as P
import Util.Set ((∪), empty)
import Val (BaseVal(..), Fun(..)) as V
import Val (DictRep(..), Env(..), EnvExpr(..), ForeignOp(..), ForeignOp'(..), MatrixRep(..), Val(..), forDefs)

-- Needs a better name.
type GraphConfig =
   { progCxt :: ProgCxt Vertex
   , n :: Int
   , γ :: Env Vertex
   }

patternMismatch :: String -> String -> String
patternMismatch s s' = "Pattern mismatch: found " <> s <> ", expected " <> s'

match :: forall m. MonadWithGraphAlloc m => Val Vertex -> Elim Vertex -> m (Env Vertex × Cont Vertex × Set DVertex)
match v (ElimVar x κ)
   | x == varAnon = pure (empty × κ × empty)
   | otherwise = pure (maplet x v × κ × empty)
match (Val α v@(V.Constr c vs)) (ElimConstr m) = do
   withMsg "Pattern mismatch" $ Set.singleton c `consistentWith` keys m
   κ <- lookup c m # orElse ("Incomplete patterns: no branch for " <> showCtr c)
   γ × κ' × αs <- matchMany vs κ
   pure (γ × κ' × (insert (dvertex α v) αs))
match v (ElimConstr m) = do
   d <- dataTypeFor $ keys m
   throw $ patternMismatch (prettyP v) (show d)
match (Val α v@(V.Dictionary (DictRep xvs))) (ElimDict xs κ) = do
   check (Set.subset xs (Set.fromFoldable $ keys xvs))
      $ patternMismatch (show (keys xvs)) (show xs)
   let xs' = xs # Set.toUnfoldable
   let xvs' = unwrap xvs
   γ × κ' × αs <- matchMany (map (\k -> snd (get k xvs')) xs') κ
   pure $ γ × κ' × (insert (dvertex α v) αs)
match v (ElimDict xs _) = throw (patternMismatch (prettyP v) (show xs))

matchMany :: forall m. MonadWithGraphAlloc m => List (Val Vertex) -> Cont Vertex -> m (Env Vertex × Cont Vertex × Set DVertex)
matchMany Nil κ = pure (empty × κ × empty)
matchMany (v : vs) (ContElim σ) = do
   γ × κ × αs <- match v σ
   γ' × κ' × βs <- matchMany vs κ
   pure $ γ `disjointUnion` γ' × κ' × (αs ∪ βs)
matchMany (_ : vs) (ContExpr _) = throw $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"

closeDefs :: forall m. MonadWithGraphAlloc m => Env Vertex -> Dict (Elim Vertex) -> Set DVertex -> m (Env Vertex)
closeDefs γ ρ αs =
   Env <$> for ρ \σ ->
      let
         ρ' = ρ `forDefs` σ
         v = V.Fun (V.Closure (restrict (fv ρ' ∪ fv σ) γ) ρ' σ)
      in
         Val <$> new (pack v) (unDVertex αs) <@> v

apply :: forall m. MonadWithGraphAlloc m => Val Vertex -> Val Vertex -> m (Val Vertex)
apply (Val α v'@(V.Fun (V.Closure γ1 ρ σ))) v = do
   γ2 <- closeDefs γ1 ρ (singleton (dvertex α v'))
   γ3 × κ × αs <- match v σ
   eval (γ1 <+> γ2 <+> γ3) (asExpr κ) (insert (dvertex α v') αs)
apply (Val α (V.Fun (V.Foreign (ForeignOp (id × φ)) vs))) v =
   runExists apply' φ
   where
   vs' = snoc vs v

   apply' :: forall t. ForeignOp' t -> m (Val Vertex)
   apply' (ForeignOp' φ') =
      if φ'.arity > length vs' then
         Val <$> new (pack v') (singleton α) <@> v'
      else φ'.op' vs'
      where
      v' = (V.Fun (V.Foreign (ForeignOp (id × φ)) vs'))
apply (Val α (V.Fun (V.PartialConstr c vs))) v = do
   check (length vs < n) ("Too many arguments to " <> showCtr c)
   if length vs < n - 1 then
      let v' = V.Fun (V.PartialConstr c (snoc vs v)) in Val <$> new (pack v') (singleton α) <@> v'
   else
      let v' = V.Constr c (snoc vs v) in Val <$> new (pack v') (singleton α) <@> v'
   where
   n = defined (arity c)
apply _ v = throw $ "Found " <> prettyP v <> ", expected function"

eval :: forall m. MonadWithGraphAlloc m => Env Vertex -> Expr Vertex -> Set DVertex -> m (Val Vertex)
eval γ (Var x) _ = withMsg "Variable lookup" $ lookup' x γ
eval γ (Op op) _ = withMsg "Variable lookup" $ lookup' op γ
eval _ (Int α n) αs = Val <$> new (pack v) (insert' α αs) <@> v
   where
   v = V.Int n
eval _ (Float α n) αs = Val <$> new (pack v) (insert' α αs) <@> v
   where
   v = V.Float n
eval _ (Str α s) αs = Val <$> new (pack v) (insert' α αs) <@> v
   where
   v = V.Str s
eval γ (Dictionary α ees) αs = do
   vs × us <- traverse (traverse (flip (eval γ) αs)) ees <#> P.unzip
   let
      ss × βs = (vs <#> unpack string) # unzip
      d = wrap $ D.fromFoldable $ zip ss (zip βs us)
      v = V.Dictionary (DictRep d)
   Val <$> new (pack v) (insert' α αs) <@> v
eval γ (Constr α c es) αs = do
   checkArity c (length es)
   vs <- traverse (flip (eval γ) αs) es
   let v = V.Constr c vs
   Val <$> new (pack v) (insert' α αs) <@> v
eval γ (Matrix α e (x × y) e') αs = do
   Val _ v <- eval γ e' αs
   let (i' × β) × (j' × β') = intPair.unpack v
   check
      (i' × j' >= 1 × 1)
      ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
   vss <- sequence do
      i <- A.range 1 i'
      singleton $ sequence do
         j <- A.range 1 j'
         let γ' = maplet x (Val β (V.Int i)) `disjointUnion` (maplet y (Val β' (V.Int j)))
         singleton (eval (γ <+> γ') e αs)
   let v' = V.Matrix (MatrixRep (vss × (i' × β) × (j' × β')))
   Val <$> new (pack v') (insert' α αs) <@> v'
eval γ (Lambda α σ) αs =
   Val <$> new (pack v) (insert' α αs) <@> v
   where
   v = V.Fun (V.Closure (restrict (fv σ) γ) empty σ)
eval γ (Project e x) αs = do
   v <- eval γ e αs
   case v of
      Val _ (V.Dictionary (DictRep d)) -> withMsg "Dict lookup" (snd <$> lookup x d # orElse ("Key \"" <> x <> "\" not found"))
      _ -> throw $ "Found " <> prettyP v <> ", expected dictionary"
eval γ (DProject e x) α = do
   v <- eval γ e α
   v' <- eval γ x α
   case v of
      Val _ (V.Dictionary (DictRep d)) ->
         case v' of
            Val _ (V.Str s) -> withMsg "Dict lookup" $ snd <$> lookup s d # orElse ("Key \"" <> s <> "\" not found")
            _ -> throw $ "Found " <> prettyP v' <> ", expected string"
      _ -> throw $ "Found " <> prettyP v <> ", expected dict"
eval γ (App e e') αs = do
   v <- eval γ e αs
   v' <- eval γ e' αs
   apply v v'
eval γ (Let (VarDef σ e) e') αs = do
   v <- eval γ e αs
   γ' × _ × αs' <- match v σ -- terminal meta-type of eliminator is meta-unit
   eval (γ <+> γ') e' αs' -- (αs ∧ αs') for consistency with functions? (similarly for module defs)
eval γ (LetRec defs@(RecDefs α ρ) e) αs = do
   γ' <- closeDefs γ ρ insertedα
   eval (γ <+> γ') e insertedα
   where
   insertedα = insert (dvertex α defs) αs

eval_module :: forall m. MonadWithGraphAlloc m => Env Vertex -> Module Vertex -> Set DVertex -> m (Env Vertex)
eval_module γ = go empty
   where
   go :: Env Vertex -> Module Vertex -> Set DVertex -> m (Env Vertex)
   go γ' (Module Nil) _ = pure γ'
   go y' (Module (Left (VarDef σ e) : ds)) αs = do
      v <- eval (γ <+> y') e αs
      γ'' × _ × αs' <- match v σ
      go (y' <+> γ'') (Module ds) αs'
   go γ' (Module (Right defs@(RecDefs α ρ) : ds)) αs = do
      γ'' <- closeDefs (γ <+> γ') ρ (insert (dvertex α defs) αs)
      go (γ' <+> γ'') (Module ds) αs

eval_progCxt :: forall m. MonadWithGraphAlloc m => ProgCxt Vertex -> m (Env Vertex)
eval_progCxt (ProgCxt { primitives, mods, datasets }) =
   flip concatM primitives ((reverse mods <#> addModule) <> (reverse datasets <#> addDataset))
   where
   addModule :: Module Vertex -> Env Vertex -> m (Env Vertex)
   addModule mod γ = do
      γ' <- eval_module γ mod empty
      pure $ γ <+> γ'

   addDataset :: Bind (Expr Vertex) -> Env Vertex -> m (Env Vertex)
   addDataset (x ↦ e) γ = do
      v <- eval γ e empty
      pure $ γ <+> maplet x v

type GraphEval g s t =
   { g :: g
   , graph_fwd :: Set DVertex -> Endo g
   , graph_bwd :: Set DVertex -> Endo g
   , inα :: s Vertex
   , outα :: t Vertex
   }

withOp :: forall g s t. Graph g => GraphEval g s t -> GraphEval g t s
withOp { g, graph_fwd, graph_bwd, inα, outα } =
   { g: op g, graph_fwd, graph_bwd, inα: outα, outα: inα }

graphGC :: forall g s t. Graph g => Apply s => Apply t => Foldable s => Foldable t => GraphEval g s t -> GaloisConnection (s 𝔹) (t 𝔹)
graphGC { g, graph_fwd, graph_bwd, inα, outα } = GC
   { fwd: \in𝔹 -> select𝔹s outα (vertices (graph_fwd (selectαs' in𝔹 inα) g))
   , bwd: \out𝔹 -> select𝔹s inα (vertices (graph_bwd (selectαs' out𝔹 outα) g))
   }

graphEval :: forall m. MonadError Error m => GraphConfig -> Raw Expr -> m (GraphEval GraphImpl EnvExpr Val)
graphEval { n, γ } e = do
   _ × _ × g × inα × outα <- flip runAllocT n do
      eα <- alloc e
      let inα = EnvExpr γ eα
      g × outα <- runWithGraphT_spy (eval γ eα mempty) (vertices' inα)
      when checking.outputsInGraph $ check (vertices outα ⊆ vertices g) "outputs in graph"
      pure (g × inα × outα)
   pure { g, graph_fwd, graph_bwd, inα, outα }
   where
   graph_fwd = curry (fwdSlice # spyFun' tracing.graphFwdSlice "fwdSlice")
   graph_bwd = curry (bwdSlice # spyFun' tracing.graphBwdSlice "bwdSlice")
   spyFun' b msg = spyFunWhen b msg (showVertices' *** showGraph) showGraph
