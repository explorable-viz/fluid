module EvalGraph where

import Prelude hiding (apply)

import Bind (varAnon)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadReader)
import Data.Array (range) as A
import Data.Either (Either(..))
import Data.List (List(..), foldM, foldl, length, snoc, unzip, zip, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Profunctor.Strong ((***))
import Data.Set (Set, insert)
import Data.Set as Set
import Data.Traversable (class Foldable, for, sequence, traverse)
import Data.Tuple (curry, fst, snd)
import DataType (arity, checkArity, consistentWith, dataTypeFor, showCtr)
import Dict (Dict)
import Dict (fromFoldable) as D
import Doc (ParagraphElem(..), DocOpt(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), Expr(..), Module(..), RecDefs(..), VarDef(..), asExpr, fv)
import File (class LoadFile, FileCxt)
import GaloisConnection (GaloisConnection(..))
import Graph (class Graph, Vertex, op, selectαs, select𝔹s, showGraph, showVertices, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSlice, fwdSlice)
import Graph.WithGraph (class MonadWithGraphAlloc, alloc, new, runAllocT, runWithGraphT_spy)
import Lattice (Raw, 𝔹)
import ModuleGraph (ModuleName, ModuleCxt)
import Pretty (prettyP)
import Primitive (intPair, string, unpack)
import ProgCxt (ProgCxt(..))
import Test.Util.Debug (checking, tracing)
import Util (type (×), Endo, check, defined, definitely, orElse, singleton, spyFunWhen, throw, withMsg, (×), (⊆))
import Util.Map (disjointUnion, get, keys, lookup, lookup', maplet, restrict, (<+>))
import Util.Pair (unzip) as P
import Util.Set ((∪), empty)
import Val (BaseVal(..), Fun(..)) as V
import Val (DictRep(..), Env(..), EnvExpr(..), ForeignOp(..), ForeignOp'(..), MatrixDim(..), MatrixRep(..), Val(..), forDefs)

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
match (Val α _ (V.Constr c vs)) (ElimConstr m) = do
   withMsg "Pattern mismatch" $ Set.singleton c `consistentWith` keys m
   κ <- lookup c m # orElse ("Incomplete patterns: no branch for " <> showCtr c)
   γ × κ' × αs <- matchMany vs κ
   pure (γ × κ' × (insert α αs))
match v (ElimConstr m) = do
   d <- dataTypeFor $ keys m
   throw $ patternMismatch (prettyP v) (show d)
match (Val α _ (V.Dictionary (DictRep xvs))) (ElimDict xs κ) = do
   check (Set.subset xs (Set.fromFoldable $ keys xvs))
      $ patternMismatch (show (keys xvs)) (show xs)
   let xs' = xs # Set.toUnfoldable
   let xvs' = unwrap xvs
   γ × κ' × αs <- matchMany (map (\k -> snd (get k xvs')) xs') κ
   pure $ γ × κ' × (insert α αs)
match v (ElimDict xs _) = throw (patternMismatch (prettyP v) (show xs))

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
      let
         ρ' = ρ `forDefs` σ
      in
         new (flip Val None) αs (V.Fun (V.Closure (restrict (fv ρ' ∪ fv σ) γ) ρ' σ))

apply :: forall m. MonadWithGraphAlloc m => MonadReader FileCxt m => MonadAff m => LoadFile m => Val Vertex -> Val Vertex -> m (Val Vertex)
apply (Val α _ (V.Fun (V.Closure γ1 ρ σ))) v = do
   γ2 <- closeDefs γ1 ρ (singleton α)
   γ3 × κ × αs <- match v σ
   eval (γ1 <+> γ2 <+> γ3) (asExpr κ) (insert α αs)
apply (Val α _ (V.Fun (V.Foreign (ForeignOp (id × φ)) vs))) v =
   apply' φ
   where
   vs' = snoc vs v

   apply' :: ForeignOp' -> m (Val Vertex)
   apply' (ForeignOp' φ') =
      if φ'.arity > length vs' then
         new (flip Val None) (singleton α) v'
      else φ'.op vs'
      where
      v' = V.Fun (V.Foreign (ForeignOp (id × φ)) vs')
apply (Val α _ (V.Fun (V.PartialConstr c vs))) v = do
   check (length vs < n) ("Too many arguments to " <> showCtr c)
   new (flip Val None) (singleton α) v'
   where
   v' =
      if length vs < n - 1 then
         V.Fun (V.PartialConstr c (snoc vs v))
      else
         V.Constr c (snoc vs v)
   n = defined (arity c)
apply _ v = throw $ "Found " <> prettyP v <> ", expected function"

eval :: forall m. MonadWithGraphAlloc m => MonadReader FileCxt m => MonadAff m => LoadFile m => Env Vertex -> Expr Vertex -> Set Vertex -> m (Val Vertex)
eval γ (Var x) _ =
   withMsg "Variable lookup" $ lookup' x γ
eval γ (Op op) _ =
   withMsg "Variable lookup" $ lookup' op γ
eval _ (Int α n) αs =
   new (flip Val None) (insert α αs) (V.Int n)
eval _ (Float α n) αs =
   new (flip Val None) (insert α αs) (V.Float n)
eval _ (Str α s) αs =
   new (flip Val None) (insert α αs) (V.Str s)
eval γ (Dictionary α ees) αs = do
   vs × us <- traverse (traverse (flip (eval γ) αs)) ees <#> P.unzip
   let
      ss × βs = (vs <#> unpack string) # unzip
      d = D.fromFoldable $ zip ss (zip βs us)
   new (flip Val None) (insert α αs) $ V.Dictionary (DictRep d)
eval γ (Constr α c es) αs = do
   checkArity c (length es)
   vs <- traverse (flip (eval γ) αs) es
   new (flip Val None) (insert α αs) $ V.Constr c vs
eval γ (Matrix α e (x × y) e') αs = do
   Val _ _ v <- eval γ e' αs
   let (i' × β) × (j' × β') = intPair.unpack v
   check
      (i' × j' >= 1 × 1)
      ("array must be at least (" <> show (1 × 1) <> "); got (" <> show (i' × j') <> ")")
   vss <- sequence do
      i <- A.range 1 i'
      singleton $ sequence do
         j <- A.range 1 j'
         let γ' = maplet x (Val β None (V.Int i)) `disjointUnion` (maplet y (Val β' None (V.Int j)))
         singleton (eval (γ <+> γ') e αs)
   new (flip Val None) (insert α αs) $ V.Matrix (MatrixRep (vss × MatrixDim (i' × β) × MatrixDim (j' × β')))
eval γ (Lambda α σ) αs =
   new (flip Val None) (insert α αs) $ V.Fun (V.Closure (restrict (fv σ) γ) empty σ)
eval γ (Project e x) αs = do
   v <- eval γ e αs
   case v of
      Val _ _ (V.Dictionary (DictRep d)) ->
         withMsg "Dict lookup" (snd <$> lookup x d # orElse ("Key \"" <> x <> "\" not found"))
      _ -> throw $ "Found " <> prettyP v <> ", expected dictionary"
eval γ (DProject e x) α = do
   v <- eval γ e α
   v' <- eval γ x α
   case v of
      Val _ _ (V.Dictionary (DictRep d)) ->
         case v' of
            Val _ _ (V.Str s) ->
               withMsg "Dict lookup" $ snd <$> lookup s d # orElse ("Key \"" <> s <> "\" not found")
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
eval γ (LetRec (RecDefs α ρ) e) αs = do
   γ' <- closeDefs γ ρ (insert α αs)
   eval (γ <+> γ') e (insert α αs)
eval γ (DocExpr p e) αs = do
   Val α vdoc u <- eval γ e αs
   pv <- sequence (evalToken (γ <+> maplet "this" (Val α None u)) <$> p)
   pure $ Val α (Doc pv <> vdoc) u
   where
   evalToken :: Env Vertex -> ParagraphElem Expr Vertex -> m (ParagraphElem Val Vertex)
   evalToken _ (Token s) = pure $ Token s
   evalToken γ' (Unquote e') = Unquote <$> eval γ' e' empty

eval_module :: forall m. MonadWithGraphAlloc m => MonadReader FileCxt m => MonadAff m => LoadFile m => Env Vertex -> Module Vertex -> Set Vertex -> m (Env Vertex)
eval_module γ = go empty
   where
   go :: Env Vertex -> Module Vertex -> Set Vertex -> m (Env Vertex)
   go γ' (Module Nil) _ = pure γ'
   go y' (Module (Left (VarDef σ e) : ds)) αs = do
      v <- eval (γ <+> y') e αs
      γ'' × _ × αs' <- match v σ
      go (y' <+> γ'') (Module ds) αs'
   go γ' (Module (Right (RecDefs α ρ) : ds)) αs = do
      γ'' <- closeDefs (γ <+> γ') ρ (insert α αs)
      go (γ' <+> γ'') (Module ds) αs

eval_progCxt
   :: forall m
    . MonadWithGraphAlloc m
   => MonadReader FileCxt m
   => MonadAff m
   => LoadFile m
   => ProgCxt Vertex
   -> ModuleCxt Vertex
   -> m (Env Vertex)
eval_progCxt (ProgCxt { primitives }) { roots, topsorted, graph, modules } = do
   γs <- evalAll primitives topsorted
   let γs' = map (\dep -> definitely ("has env") $ Map.lookup dep γs) roots
   let γ = foldl (<+>) primitives γs'
   pure γ

   where
   evalAll :: Env Vertex -> List ModuleName -> m (Map ModuleName (Env Vertex))
   evalAll γ mods = foldM evalOne Map.empty mods

      where
      evalOne :: Map ModuleName (Env Vertex) -> ModuleName -> m (Map ModuleName (Env Vertex))
      evalOne γs name = do
         let
            (defs' × γs') = definitely "deps evaluated" do
               deps <- Map.lookup name graph
               γs' <- traverse (\dep -> Map.lookup dep γs) deps
               defs' <- Map.lookup name modules
               pure (defs' × γs')
         γ' <- eval_module (foldl (<+>) γ γs') defs' empty
         pure $ Map.insert name γ' γs

type GraphEval g s t =
   { g :: g
   , graph_fwd :: Set Vertex -> Endo g
   , graph_bwd :: Set Vertex -> Endo g
   , inα :: s Vertex
   , outα :: t Vertex
   }

withOp :: forall g s t. Graph g => GraphEval g s t -> GraphEval g t s
withOp { g, graph_fwd, graph_bwd, inα, outα } =
   { g: op g, graph_fwd, graph_bwd, inα: outα, outα: inα }

graphGC
   :: forall g s t
    . Graph g
   => Apply s
   => Apply t
   => Foldable s
   => Foldable t
   => GraphEval g s t
   -> { fwd :: s 𝔹 -> t 𝔹 × g
      , bwd :: t 𝔹 -> s 𝔹 × g
      }
graphGC { g, graph_fwd, graph_bwd, inα, outα } =
   { fwd: \in𝔹 ->
        let
           g' = graph_fwd (selectαs in𝔹 inα) g
        in
           select𝔹s outα (vertices g') × g'
   , bwd: \out𝔹 ->
        let
           g' = graph_bwd (selectαs out𝔹 outα) g
        in
           select𝔹s inα (vertices g') × g'
   }

toGC
   :: forall g s t
    . Graph g
   => Apply s
   => Apply t
   => Foldable s
   => Foldable t
   => { fwd :: s 𝔹 -> t 𝔹 × g
      , bwd :: t 𝔹 -> s 𝔹 × g
      }
   -> GaloisConnection (s 𝔹) (t 𝔹)
toGC { fwd, bwd } = GC { fwd: fst <<< fwd, bwd: fst <<< bwd }

graphEval :: forall m. MonadAff m => MonadReader FileCxt m => LoadFile m => MonadError Error m => GraphConfig -> Raw Expr -> m (GraphEval GraphImpl EnvExpr Val)
graphEval { n, γ } e = do
   _ × _ × g × inα × outα <- flip runAllocT n do
      eα <- alloc e
      let inα = EnvExpr γ eα
      g × outα <- runWithGraphT_spy (eval γ eα mempty) (vertices inα)
      when checking.outputsInGraph $ check (vertices outα ⊆ vertices g) "outputs in graph"
      pure (g × inα × outα)
   pure { g, graph_fwd, graph_bwd, inα, outα }
   where
   graph_fwd = curry (fwdSlice # spyFun' tracing.graphFwdSlice "fwdSlice")
   graph_bwd = curry (bwdSlice # spyFun' tracing.graphBwdSlice "bwdSlice")
   spyFun' b msg = spyFunWhen b msg (showVertices *** showGraph) showGraph
