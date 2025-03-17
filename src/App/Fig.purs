module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (SelState, Selection, 𝕊, as𝕊, getPersistent, getTransient, selState, to𝕊)
import App.Util.Selector (envVal)
import App.View (view)
import App.View.Util (Direction(..), Fig, FigSpec, HTMLId, Redraw, View, drawView)
import App.View.Util.D3 (remove, rootSelect)
import Bind (Var)
import Control.Apply (lift2)
import Data.Array (concat, fromFoldable, zipWith)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (first, (***))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for, for_, sequence_)
import Data.Tuple (fst, snd)
import Dict (Dict)
import Dict (fromFoldable) as D
import Effect (Effect)
import EvalGraph (graphEval, graphGC, withOp)
import GaloisConnection (GaloisConnection(..), deMorgan)
import Graph (class Graph, DVertex, DVertex', Vertex(..), runQuery, select𝔹s, vertices)
import Graph.GraphImpl (GraphImpl)
import Lattice (class BoundedMeetSemilattice, Raw, 𝔹, botOf, erase, topOf)
import Module.Web (File, loadProgCxt, prepConfig)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Test.Util.Debug (tracing)
import Util (type (×), AffError, Endo, Setter, definitely', spyWhen, (×))
import Util.Map (insert, keys, lookup, mapWithKey, restrict, filterKeys)
import Util.Set ((∪), (\\), (∈), empty)
import Val (Env(..), EnvExpr(..), Val(..), unrestrictGC)

str
   :: { output :: String -- pseudo-variable to use as name of output view
      , input :: String -- prefix for input views
      , intermediate :: String
      }
str =
   { output: "output"
   , input: "input"
   , intermediate: "intermediate"
   }

mkFullId :: String -> String -> String -> String
mkFullId divId mid suffix = "#" <> divId <> "-" <> mid <> "-" <> suffix

selectOutput :: Setter Fig (Val (SelState 𝔹))
selectOutput δv fig@{ dir, γ, v } = fig
   { v = δv v
   , γ = if dir == LinkedInputs then botOf γ else γ
   , dir = LinkedOutputs
   }

setOutputView :: Setter Fig View
setOutputView δvw fig = fig
   { out_view = fig.out_view <#> δvw
   }

selectInput :: Var -> Setter Fig (Val (SelState 𝔹))
selectInput x δv fig@{ dir, γ, v } = fig
   { γ = envVal x δv γ
   , v = if dir == LinkedOutputs then botOf v else v
   , dir = LinkedInputs
   }

setInputView :: Var -> Setter Fig View
setInputView x δvw fig = fig
   { in_views = insert x (lookup x fig.in_views # join <#> δvw) fig.in_views
   }

selectIntermediate :: Vertex -> Setter Fig (Val (SelState 𝔹))
selectIntermediate (Vertex α) δv fig = fig
   { intermediate_values = insert α (definitely' $ lookup α fig.intermediate_values <#> δv) fig.intermediate_values
   }

setIntermediateView :: Vertex -> Setter Fig View
setIntermediateView (Vertex α) δvw fig = fig
   { intermediate_views = insert α (lookup α fig.intermediate_views # join <#> δvw) fig.intermediate_views
   }

selectIntermediates :: forall g. Graph g => Set DVertex -> Selection g -> Selection (Set (DVertex' (Val Vertex))) -> Array (String × Val (SelState 𝔹))
selectIntermediates inerts g vs =
   vs𝕊
   where
   verts = { persistent: vertices g.persistent, transient: vertices g.transient }

   vs' = (snd <<< unwrap) `Set.map` (vs.persistent ∪ vs.transient) # fromFoldable :: Array (Val Vertex)

   vs_selected = (\v@(Val α _) -> α × { persistent: select𝔹s v verts.persistent, transient: select𝔹s v verts.transient }) <$> vs' :: Array (Vertex × Selection (Val 𝔹))

   vs_inert = (\v -> select𝔹s v inerts) <$> vs' :: Array (Val 𝔹)

   setSels :: Val 𝔹 -> Vertex × Selection (Val 𝔹) -> String × Val (SelState 𝔹)
   setSels inert (Vertex α × v) = α × (selState <$> inert <*> v.persistent <*> v.transient)

   vs𝕊 = zipWith setSels vs_inert vs_selected

selectionResult :: Fig -> Val (SelState 𝕊) × Env (SelState 𝕊) × Dict (Val (SelState 𝔹))
selectionResult fig@{ spec, dir } =
   case dir of
      LinkedOutputs ->
         let
            v1 × γ1 × g × inertBwd = fig.linkedOutputs fig.v
            report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
         in
            (lift2 as𝕊 <$> fig.v <*> v1) × ((to𝕊 <$> _) <$> report γ1) × intermediates g inertBwd
      LinkedInputs ->
         let
            γ1 × v1 × g × inertFwd = fig.linkedInputs fig.γ
            report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
         in
            ((to𝕊 <$> _) <$> report v1) × (lift2 as𝕊 <$> fig.γ <*> γ1) × intermediates g inertFwd
   where
   intermediates :: Selection GraphImpl -> Set DVertex -> Dict (Val (SelState 𝔹))
   intermediates g inerts = D.fromFoldable $ concat $ for spec.queries
      \query -> selectIntermediates inerts g { persistent: runQuery query g.persistent, transient: runQuery query g.transient }

drawIntermediates :: HTMLId -> Dict (Val (SelState 𝔹)) -> Array String -> Redraw -> Effect Unit
drawIntermediates divId intermediates unused redraw = do
   let prefix = divId <> "-" <> str.intermediate
   for_ unused \α -> rootSelect ("#" <> prefix <> "-" <> α) >>= remove

   sequence_ $ flip mapWithKey intermediates \α v -> do
      drawView { divId: prefix, suffix: α, view: unsafePartial $ view α (map to𝕊 <$> v) Nothing } (selectIntermediate (Vertex α)) (setIntermediateView (Vertex α)) redraw

drawFig :: HTMLId -> Fig -> Effect Unit
drawFig divId fig = do
   drawView { divId, suffix: str.output, view: out_view } selectOutput setOutputView redraw
   sequence_ $ flip mapWithKey in_views \x view -> do
      drawView { divId: divId <> "-" <> str.input, suffix: x, view } (selectInput x) (setInputView x) redraw
   drawIntermediates divId intermediate_values unused redraw
   where
   out_view × in_views × intermediate_values =
      selectionResult fig # unsafePartial
         (flip (view str.output) fig.out_view *** (\(Env γ) -> mapWithKey view γ <*> fig.in_views) *** (\d -> d # filterKeys \α -> not (Vertex α ∈ fig.in_roots)))

   unused :: Array String
   unused = fromFoldable (keys fig.intermediate_values \\ keys intermediate_values)

   redraw = (_ $ fig { intermediate_values = intermediate_values }) >>> drawFig divId

drawFile :: File × String -> Effect Unit
drawFile (file × src) =
   addEditorView (codeMirrorDiv $ unwrap file) >>= drawCode src

unprojExpr :: forall a. BoundedMeetSemilattice a => Raw EnvExpr -> GaloisConnection (Env a) (EnvExpr a)
unprojExpr (EnvExpr _ e) = GC
   { fwd: \γ -> EnvExpr γ (topOf e)
   , bwd: \(EnvExpr γ _) -> γ
   }

lift
   :: forall f f' g
    . Apply f
   => Apply f'
   => f (𝔹 -> 𝔹 -> SelState 𝔹)
   -> (f' 𝔹 -> f 𝔹 × g)
   -> (f' (SelState 𝔹) -> f (SelState 𝔹) × g × g)
lift selState_f bwd = bwd'
   where
   bwd' :: f' (SelState 𝔹) -> f (SelState 𝔹) × g × g
   bwd' v =
      let
         (persistent × g) = bwd (v <#> getPersistent)
         (transient × g') = bwd (v <#> getTransient)
      in
         (selState_f <*> persistent <*> transient) × g × g'

loadFig :: forall m. FigSpec -> AffError m Fig
loadFig spec@{ fluidSrcPaths, inputs, imports, file, datasets } = do
   progCxt <- loadProgCxt fluidSrcPaths imports datasets
   { s, e, gconfig } <- prepConfig fluidSrcPaths file progCxt
   eval@({ inα: EnvExpr γα _, outα, g: g0 }) <- graphEval gconfig e
   let
      inputs_set = Set.fromFoldable inputs
      EnvExpr γ e' = erase eval.inα
      { fwd: focusFwd, bwd: focusBwd } = unwrap (unrestrictGC γ inputs_set >>> unprojExpr (EnvExpr γ e'))

      γ_restricted = restrict inputs_set γα

      in_roots = Set.fromFoldable $ (\(Val α _) -> α) <$> unwrap γ_restricted

      graphgc = graphGC eval
      graphgc_op = graphGC (withOp eval)

      gcBwd :: Val 𝔹 -> Env 𝔹 × GraphImpl
      gcBwd v = first focusBwd (graphgc.bwd v)

      gcFwd :: Env 𝔹 -> Val 𝔹 × GraphImpl
      gcFwd γ = graphgc_op.bwd (deMorgan focusFwd γ)

      in_views = const Nothing <$> unwrap γ_restricted

      γ0 = botOf γα :: Env 𝔹
      v0 = botOf outα :: Val 𝔹

      inertBwd = vertices g0 \\ (vertices $ snd (gcBwd (topOf outα))) :: Set DVertex
      inertFwd = vertices $ snd $ (graphgc.fwd <<< focusFwd) γ0

      γInert = selState <$> select𝔹s γα inertBwd :: Env (𝔹 -> 𝔹 -> SelState 𝔹)
      vInert = selState <$> select𝔹s outα inertFwd :: Val (𝔹 -> 𝔹 -> SelState 𝔹)

      vf = lift γInert gcBwd
      γf = lift vInert gcFwd

      linkedInputs :: Env (SelState 𝔹) -> Env (SelState 𝔹) × Val (SelState 𝔹) × Selection GraphImpl × Set DVertex
      linkedInputs = γf >>> \(v × g × g') -> (fst $ vf v) × v × { persistent: g, transient: g' } × inertFwd

      linkedOutputs :: Val (SelState 𝔹) -> Val (SelState 𝔹) × Env (SelState 𝔹) × Selection GraphImpl × Set DVertex
      linkedOutputs = vf >>> \(γ × g × g') -> (fst $ γf γ) × γ × { persistent: g, transient: g' } × inertBwd

   pure
      { spec
      , s
      , γ: γInert <*> γ0 <*> γ0
      , v: vInert <*> v0 <*> v0
      , linkedOutputs
      , linkedInputs
      , dir: LinkedOutputs
      , in_views
      , out_view: Nothing
      , intermediate_views: empty
      , intermediate_values: empty
      , in_roots
      }

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

drawFigWithCode :: { fig :: Fig, divId :: HTMLId } -> Effect Unit
drawFigWithCode { fig, divId } = do
   drawFig divId fig
   addEditorView (codeMirrorDiv divId) >>= drawCode (prettyP fig.s)

drawCode :: String -> EditorView -> Effect Unit
drawCode s ed =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]
