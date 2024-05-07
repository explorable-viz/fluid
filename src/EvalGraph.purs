module EvalGraph where

import Prelude hiding (apply)

import Bind (Bind, (↦), varAnon)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (range) as A
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.List (List(..), length, reverse, snoc, unzip, zip, (:))
import Data.Newtype (wrap)
import Data.Profunctor.Strong ((***))
import Data.Set (Set, insert)
import Data.Set as Set
import Data.Traversable (for, sequence, traverse)
import Data.Tuple (curry)
import DataType (checkArity, arity, consistentWith, dataTypeFor, showCtr)
import Dict (Dict)
import Dict (fromFoldable) as D
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDefs(..), VarDef(..), asExpr, fv)
import GaloisConnection (GaloisConnection(..))
import Graph (class Graph, Vertex, op, selectαs, select𝔹s, showGraph, showVertices, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSlice, fwdSlice)
import Graph.WithGraph (class MonadWithGraphAlloc, alloc, new, runAllocT, runWithGraphT_spy)
import Lattice (𝔹, Raw)
import Pretty (prettyP)
import Primitive (intPair, string, unpack)
import ProgCxt (ProgCxt(..))
import Test.Util.Debug (checking, tracing)
import Util (type (×), Endo, check, concatM, orElse, singleton, spyFunWhen, defined, throw, with, (×), (⊆))
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

match :: forall m. MonadWithGraphAlloc m => Val Vertex -> Elim Vertex -> m (Env Vertex × Cont Vertex × Set Vertex)
match v (ElimVar x κ)
   | x == varAnon = pure (empty × κ × empty)
   | otherwise = pure (maplet x v × κ × empty)
match (Val α (V.Constr c vs)) (ElimConstr m) = do
   with "Pattern mismatch" $ Set.singleton c `consistentWith` keys m
   κ <- lookup c m # orElse ("Incomplete patterns: no branch for " <> showCtr c)
   γ × κ' × αs <- matchMany vs κ
   pure (γ × κ' × (insert α αs))
match v (ElimConstr m) = do
   d <- dataTypeFor $ keys m
   throw $ patternMismatch (prettyP v) (show d)
match (Val α (V.Record xvs)) (ElimRecord xs κ) = do
   check (Set.subset xs (Set.fromFoldable $ keys xvs))
      $ patternMismatch (show (keys xvs)) (show xs)
   let xs' = xs # Set.toUnfoldable
   γ × κ' × αs <- matchMany (flip get xvs <$> xs') κ
   pure $ γ × κ' × (insert α αs)
match v (ElimRecord xs _) = throw (patternMismatch (prettyP v) (show xs))

matchMany :: forall m. MonadWithGraphAlloc m => List (Val Vertex) -> Cont Vertex -> m (Env Vertex × Cont Vertex × Set Vertex)
matchMany Nil κ = pure (empty × κ × empty)
matchMany (v : vs) (ContElim σ) = do
   γ × κ × αs <- match v σ
   γ' × κ' × βs <- matchMany vs κ
   pure $ γ `disjointUnion` γ' × κ' × (αs ∪ βs)
matchMany (_ : vs) (ContExpr _) = throw $
   show (length vs + 1) <> " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?"

closeDefs :: forall m. MonadWithGraphAlloc m => Env Vertex -> Dict (Elim Vertex) -> Set Vertex -> m (Env Vertex)
closeDefs γ ρ αs =
   Env <$> for ρ \σ ->
      let ρ' = ρ `forDefs` σ in Val <$> new αs <@> V.Fun (V.Closure (restrict (fv ρ' ∪ fv σ) γ) ρ' σ)

apply :: forall m. MonadWithGraphAlloc m => Val Vertex -> Val Vertex -> m (Val Vertex)
apply (Val α (V.Fun (V.Closure γ1 ρ σ))) v = do
   γ2 <- closeDefs γ1 ρ (singleton α)
   γ3 × κ × αs <- match v σ
   eval (γ1 <+> γ2 <+> γ3) (asExpr κ) (insert α αs)
apply (Val α (V.Fun (V.Foreign (ForeignOp (id × φ)) vs))) v =
   runExists apply' φ
   where
   vs' = snoc vs v

   apply' :: forall t. ForeignOp' t -> m (Val Vertex)
   apply' (ForeignOp' φ') =
      if φ'.arity > length vs' then
         Val <$> new (singleton α) <@> (V.Fun (V.Foreign (ForeignOp (id × φ)) vs'))
      else φ'.op' vs'
apply (Val α (V.Fun (V.PartialConstr c vs))) v = do
   check (length vs < n) ("Too many arguments to " <> showCtr c)
   if length vs < n - 1 then Val <$> new (singleton α) <@> V.Fun (V.PartialConstr c (snoc vs v))
   else Val <$> new (singleton α) <@> V.Constr c (snoc vs v)
   where
   n = defined (arity c)
apply _ v = throw $ "Found " <> prettyP v <> ", expected function"

eval :: forall m. MonadWithGraphAlloc m => Env Vertex -> Expr Vertex -> Set Vertex -> m (Val Vertex)
eval γ (Var x) _ = with "Variable lookup" $ lookup' x γ
eval γ (Op op) _ = with "Variable lookup" $ lookup' op γ
eval _ (Int α n) αs = Val <$> new (insert α αs) <@> V.Int n
eval _ (Float α n) αs = Val <$> new (insert α αs) <@> V.Float n
eval _ (Str α s) αs = Val <$> new (insert α αs) <@> V.Str s
eval γ (Record α xes) αs = do
   xvs <- traverse (flip (eval γ) αs) xes
   Val <$> new (insert α αs) <@> V.Record xvs
eval γ (Dictionary α ees) αs = do
   vs × us <- traverse (traverse (flip (eval γ) αs)) ees <#> P.unzip
   let
      ss × βs = (vs <#> unpack string) # unzip
      d = wrap $ D.fromFoldable $ zip ss (zip βs us)
   Val <$> new (insert α αs) <@> V.Dictionary (DictRep d)
eval γ (Constr α c es) αs = do
   checkArity c (length es)
   vs <- traverse (flip (eval γ) αs) es
   Val <$> new (insert α αs) <@> V.Constr c vs
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
   Val <$> new (insert α αs) <@> V.Matrix (MatrixRep (vss × (i' × β) × (j' × β')))
eval γ (Lambda α σ) αs =
   Val <$> new (insert α αs) <@> V.Fun (V.Closure (restrict (fv σ) γ) empty σ)
eval γ (Project e x) αs = do
   v <- eval γ e αs
   case v of
      Val _ (V.Record xvs) -> with "Record lookup" (lookup' x xvs)
      _ -> throw $ "Found " <> prettyP v <> ", expected record"
eval γ (App e e') αs = do
   v <- eval γ e αs
   v' <- eval γ e' αs
   apply v v'
eval γ (Let (VarDef σ e) e') αs = do
   v <- eval γ e αs
   γ' × _ × αs' <- match v σ -- terminal meta-type of eliminator is meta-unit
   eval (γ <+> γ') e' αs' -- (αs ∧ αs') for consistency with functions? (similarly for module defs)
eval γ (LetRec (RecDefs α ρ) e) αs = do
   γ' <- closeDefs γ ρ (insert α αs)
   eval (γ <+> γ') e (insert α αs)

eval_module :: forall m. MonadWithGraphAlloc m => Env Vertex -> Module Vertex -> Set Vertex -> m (Env Vertex)
eval_module γ = go empty
   where
   go :: Env Vertex -> Module Vertex -> Set Vertex -> m (Env Vertex)
   go γ' (Module Nil) _ = pure γ'
   go y' (Module (Left (VarDef σ e) : ds)) αs = do
      v <- eval (γ <+> y') e αs
      γ'' × _ × α' <- match v σ
      go (y' <+> γ'') (Module ds) α'
   go γ' (Module (Right (RecDefs α ρ) : ds)) αs = do
      γ'' <- closeDefs (γ <+> γ') ρ (insert α αs)
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

type GraphEval g =
   { gc :: GaloisConnection (EnvExpr 𝔹) (Val 𝔹)
   , gc_op :: GaloisConnection (Val 𝔹) (EnvExpr 𝔹)
   , γeα :: EnvExpr Vertex
   , g :: g
   , vα :: Val Vertex
   }

type GraphEval2 g s t =
   { gc :: GaloisConnection (s 𝔹) (t 𝔹)
   , g :: g
   , inα :: s Vertex
   , outα :: t Vertex
   }

withOp :: forall g s t. Graph g => GraphEval2 g s t -> GraphEval2 g t s
withOp { gc: GC { fwd, bwd }, g, inα, outα } =
   { gc: GC { fwd: bwd, bwd: fwd }, g: op g, inα: outα, outα: inα }

graphGC_new
   :: forall m
    . MonadError Error m
   => GraphConfig
   -> Raw Expr
   -> m (GraphEval2 GraphImpl EnvExpr Val)
graphGC_new { n, γ } e = do
   _ × _ × g × inα × outα <- flip runAllocT n do
      eα <- alloc e
      let inα = EnvExpr γ eα
      let inputs = vertices inα
      g × outα :: _ × Val Vertex <- runWithGraphT_spy (eval γ eα mempty) inputs
      when checking.outputsInGraph $ check (vertices outα ⊆ vertices g) "outputs in graph"
      pure (g × inα × outα)
   let
      gc :: GaloisConnection (EnvExpr 𝔹) (Val 𝔹)
      gc = GC
         { fwd: \in𝔹 -> select𝔹s outα (vertices (fwdSlice' (selectαs in𝔹 inα) g))
         , bwd: \out𝔹 -> select𝔹s inα (vertices (bwdSlice' (selectαs out𝔹 outα) g))
         }
   pure { gc, g, inα, outα }
   where
   fwdSlice' :: Set Vertex -> Endo GraphImpl
   fwdSlice' = curry (fwdSlice # spyFun' tracing.graphFwdSlice "fwdSlice")

   bwdSlice' :: Set Vertex -> Endo GraphImpl
   bwdSlice' = curry (bwdSlice # spyFun' tracing.graphBwdSlice "bwdSlice")

   spyFun' b msg = spyFunWhen b msg (showVertices *** showGraph) showGraph

graphGC
   :: forall m
    . MonadError Error m
   => GraphConfig
   -> Raw Expr
   -> m (GraphEval GraphImpl)
graphGC { n, γ } e = do
   _ × _ × g × eα × outα <- flip runAllocT n do
      eα <- alloc e
      let inα = EnvExpr γ eα
      let inputs = vertices inα
      g × outα :: _ × Val Vertex <- runWithGraphT_spy (eval γ eα mempty) inputs
      when checking.outputsInGraph $ check (vertices outα ⊆ vertices g) "outputs in graph"
      pure (g × eα × outα)

   let inα = EnvExpr γ eα
   pure
      { gc: GC
           { fwd: \in𝔹 -> select𝔹s outα (vertices (fwdSlice' (selectαs in𝔹 inα) g))
           , bwd: \out𝔹 -> select𝔹s inα (vertices (bwdSlice' (selectαs out𝔹 outα) g))
           }
      , gc_op: GC
           { fwd: \out𝔹 -> select𝔹s inα (vertices (fwdSlice' (selectαs out𝔹 outα) (op g)))
           , bwd: \in𝔹 -> select𝔹s outα (vertices (bwdSlice' (selectαs in𝔹 inα) (op g)))
           }
      , γeα: EnvExpr γ eα
      , g
      , vα: outα
      }
   where
   fwdSlice' :: Set Vertex -> Endo GraphImpl
   fwdSlice' = curry (fwdSlice # spyFun' tracing.graphFwdSlice "fwdSlice")

   bwdSlice' :: Set Vertex -> Endo GraphImpl
   bwdSlice' = curry (bwdSlice # spyFun' tracing.graphBwdSlice "bwdSlice")

   spyFun' b msg = spyFunWhen b msg (showVertices *** showGraph) showGraph
