module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (SelState, SelStates, Selection(..), 𝕊, as𝕊, distributeSel, mergeSelStates, selState, to𝔹, to𝕊)
import App.Util.Selector (envVal)
import App.Util.Selector (lift) as Sel
import App.View (view)
import App.View.Util (Direction(..), Fig, FigSpec, HTMLId, Redraw, View, drawView)
import App.View.Util.D3 (remove, rootSelect)
import Bind (Var)
import Control.Apply (lift2)
import Data.Array (fromFoldable, zipWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (first, (***))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for_, sequence_)
import Data.Tuple (fst, snd)
import Dict (fromFoldable) as D
import Effect (Effect)
import EvalGraph (graphEval, graphGC, withOp)
import GaloisConnection (GaloisConnection(..), deMorgan)
import Graph (class Graph, DVertex', Vertex(..), DVertex, runQuery, select𝔹s, vertices)
import Graph.GraphImpl (GraphImpl)
import Lattice (class BoundedMeetSemilattice, Raw, 𝔹, botOf, erase, topOf)
import Module.Web (File, loadProgCxt, prepConfig)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Test.Util.Debug (tracing)
import Util (type (×), AffError, Endo, Setter, spyWhen, (×))
import Util.Map (filterKeys, insert, keys, lookup, mapWithKey, restrict)
import Util.Set (empty, (\\), (∈), (∪))
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

selectOutput :: Setter Fig (Val (SelStates 𝔹))
selectOutput δv fig@{ dir, store: Selection store } = fig
   { store =
        Selection
           { persistent:
                { v: v_p
                , γ: if dir == LinkedInputs then botOf store.persistent.γ else store.persistent.γ :: Env (SelState 𝔹)
                , ι: store.persistent.ι :: Env (SelState 𝔹)
                }
           , transient:
                { v: v_t
                , γ: if dir == LinkedInputs then botOf store.transient.γ else store.transient.γ :: Env (SelState 𝔹)
                , ι: store.transient.ι :: Env (SelState 𝔹)
                }
           }
   , dir = LinkedOutputs
   }
   where
   Selection { persistent: v_p, transient: v_t } =
      Selection store <#> _.v # Sel.lift δv

setOutputView :: Setter Fig View
setOutputView δvw fig = fig
   { out_view = fig.out_view <#> δvw
   }

selectInput :: Var -> Setter Fig (Val (SelStates 𝔹))
selectInput x δv fig@{ dir, store: Selection store } = fig
   { store =
        Selection
           { persistent:
                { v: if dir == LinkedOutputs then botOf store.persistent.v else store.persistent.v :: Val (SelState 𝔹)
                , γ: γ_p
                , ι: store.persistent.ι :: Env (SelState 𝔹)
                }
           , transient:
                { v: if dir == LinkedOutputs then botOf store.transient.v else store.transient.v :: Val (SelState 𝔹)
                , γ: γ_t
                , ι: store.transient.ι :: Env (SelState 𝔹)
                }
           }
   , dir = LinkedInputs
   }
   where
   Selection { persistent: γ_p, transient: γ_t } =
      Selection store <#> _.γ # Sel.lift (envVal x δv)

setInputView :: Var -> Setter Fig View
setInputView x δvw fig = fig
   { in_views = insert x (lookup x fig.in_views # join <#> δvw) fig.in_views
   }

selectIntermediate :: Vertex -> Setter Fig (Val (SelStates 𝔹))
selectIntermediate (Vertex α) δv fig@{ store: Selection store } = fig
   { store = Selection { persistent: store.persistent { ι = ι }, transient: store.transient { ι = ι' } } }
   where
   Selection { persistent: ι, transient: ι' } =
      Selection store <#> _.ι # Sel.lift (envVal α δv)

setIntermediateView :: Vertex -> Setter Fig View
setIntermediateView (Vertex α) δvw fig = fig
   { intermediate_views = insert α (lookup α fig.intermediate_views # join <#> δvw) fig.intermediate_views
   }

selectIntermediates :: forall g. Graph g => Set DVertex -> g -> Set (DVertex' (Val Vertex)) -> Env (SelState 𝔹)
selectIntermediates inerts g vs =
   Env $ D.fromFoldable vs𝕊
   where
   verts = vertices g
   vs' = (snd <<< unwrap) `Set.map` vs # fromFoldable :: Array (Val Vertex)

   vs_selected = (\v@(Val α _) -> α × select𝔹s v verts) <$> vs' :: Array (Vertex × Val 𝔹)
   vs_inert = (\v -> select𝔹s v inerts) <$> vs' :: Array (Val 𝔹)

   setSel :: Val 𝔹 -> Vertex × Val 𝔹 -> String × Val (SelState 𝔹)
   setSel inert (Vertex α × v) = α × (selState <$> inert <*> v)

   vs𝕊 = zipWith setSel vs_inert vs_selected

selectionResult' :: Fig -> (Val (SelState 𝔹) × Env (SelState 𝔹)) -> Val (SelState 𝕊) × Env (SelState 𝕊) × Set DVertex × GraphImpl
selectionResult' fig@{ dir } (v × γ) = case dir of
   LinkedOutputs ->
      let
         v1 × γ1 × g = fig.linkedOutputs v
         report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
      in
         (lift2 as𝕊 <$> v <*> v1) × ((to𝕊 <$> _) <$> report γ1) × fig.inertBwd × g
   LinkedInputs ->
      let
         γ1 × v1 × g = fig.linkedInputs γ
         report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
      in
         ((to𝕊 <$> _) <$> report v1) × (lift2 as𝕊 <$> γ <*> γ1) × fig.inertFwd × g

selectionResult :: Fig -> Selection (Val (SelState 𝕊)) × Selection (Env (SelState 𝕊)) × Selection (Env (SelState 𝔹))
selectionResult fig@{ spec, store: Selection store } =
   Selection { persistent: vs_p, transient: vs_t }
      × Selection { persistent: γs_p, transient: γs_t }
      × ιs
   where

   vs_p × γs_p × inert × g = selectionResult' fig (store.persistent.v × store.transient.γ)
   vs_t × γs_t × _ × g' = selectionResult' fig (store.transient.v × store.transient.γ)

   ιs =
      ( \query ->
           let
              vs = runQuery query g ∪ runQuery query g'
           in
              Selection
                 { persistent: selectIntermediates inert g vs
                 , transient: selectIntermediates inert g' vs
                 }
      ) <$> spec.query
         # maybe (Selection { persistent: empty, transient: empty }) (map $ filterKeys (\α -> not (Vertex α ∈ fig.in_roots)))

drawIntermediates :: HTMLId -> Selection (Env (SelState 𝔹)) -> Array String -> Redraw -> Effect Unit
drawIntermediates divId intermediates unused redraw = do
   let prefix = divId <> "-" <> str.intermediate
   for_ unused \α -> rootSelect ("#" <> prefix <> "-" <> α) >>= remove

   let Env intermediates' = mergeSelStates intermediates

   sequence_ $ flip mapWithKey intermediates' \α v -> do
      drawView { divId: prefix, suffix: α, view: unsafePartial $ view α (map to𝕊 <$> v) Nothing } (selectIntermediate (Vertex α)) (setIntermediateView (Vertex α)) redraw

drawFig :: HTMLId -> Fig -> Effect Unit
drawFig divId fig@{ store: Selection store } = do
   drawView { divId, suffix: str.output, view: out_view } selectOutput setOutputView redraw

   sequence_ $ flip mapWithKey in_views \x view -> do
      drawView { divId: divId <> "-" <> str.input, suffix: x, view } (selectInput x) (setInputView x) redraw

   drawIntermediates divId (Selection intermediate_values) unused redraw
   where
   out_view × in_views × (Selection intermediate_values) =
      selectionResult fig # unsafePartial
         ( (flip (view str.output) fig.out_view) <<< mergeSelStates
              *** (\(Env γ) -> mapWithKey view γ <*> fig.in_views) <<< mergeSelStates
              *** identity
         )

   selKeys :: forall a. Selection (Env a) -> Set String
   selKeys (Selection γ) = keys γ.persistent ∪ keys γ.transient

   unused :: Array String
   unused = fromFoldable (keys store.persistent.ι ∪ keys store.transient.ι \\ selKeys (Selection intermediate_values))

   newStore = Selection
      { persistent: { v: store.persistent.v, γ: store.persistent.γ, ι: intermediate_values.persistent }
      , transient: { v: store.transient.v, γ: store.transient.γ, ι: intermediate_values.transient }
      }
   redraw = (_ $ fig { store = newStore }) >>> drawFig divId

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
   => f (𝔹 -> 𝔹 -> Selection (SelState 𝔹))
   -> (f' 𝔹 -> f 𝔹 × g)
   -> (Selection (f' (SelState 𝔹)) -> (Selection (f (SelState 𝔹)) × g × g))
lift selStates_f bwd = bwd'
   where
   bwd' :: Selection (f' (SelState 𝔹)) -> Selection (f (SelState 𝔹)) × g × g
   bwd' (Selection s) =
      let
         (persistent × g) = bwd (s # _.persistent <#> to𝔹)
         (transient × g') = bwd (s # _.transient <#> to𝔹)
      in
         (selStates_f <*> persistent <*> transient # distributeSel) × g × g'

lift1
   :: forall f f' g
    . Apply f
   => Apply f'
   => f (𝔹 -> (SelState 𝔹))
   -> (f' 𝔹 -> f 𝔹 × g)
   -> f' (SelState 𝔹)
   -> f (SelState 𝔹) × g
lift1 selStates_f bwd = bwd'
   where
   bwd' :: f' (SelState 𝔹) -> f (SelState 𝔹) × g
   bwd' v =
      let
         (selection × g) = bwd (v <#> to𝔹)
      in
         (selStates_f <*> selection) × g

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

      γInert = selState <$> select𝔹s γα inertBwd :: Env (𝔹 -> SelState 𝔹)
      γ_init = γInert <*> γ0
      vInert = selState <$> select𝔹s outα inertFwd :: Val (𝔹 -> SelState 𝔹)
      v_init = vInert <*> v0

      vf = lift1 γInert gcBwd :: Val (SelState 𝔹) -> Env (SelState 𝔹) × GraphImpl
      γf = lift1 vInert gcFwd :: Env (SelState 𝔹) -> Val (SelState 𝔹) × GraphImpl

      linkedInputs :: Env (SelState 𝔹) -> Env (SelState 𝔹) × Val (SelState 𝔹) × GraphImpl
      linkedInputs = γf >>> \(v × g) -> (fst $ vf v) × v × g

      linkedOutputs :: Val (SelState 𝔹) -> Val (SelState 𝔹) × Env (SelState 𝔹) × GraphImpl
      linkedOutputs = vf >>> \(γ × g) -> (fst $ γf γ) × γ × g
      initStore = { γ: γ_init, v: v_init, ι: empty }
   pure
      { spec
      , s
      , store: Selection { persistent: initStore, transient: initStore }
      , linkedOutputs
      , linkedInputs
      , dir: LinkedOutputs
      , in_views
      , out_view: Nothing
      , intermediate_views: empty
      , in_roots
      , inertFwd
      , inertBwd
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
