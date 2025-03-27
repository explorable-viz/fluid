module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (SelState, SelStates, Selection, SelectionType(..), SetSel, 𝕊, as𝕊, mergeSelStates, selState, splitSelStates, to𝔹, to𝕊)
import App.Util.Selector (envVal, envVal')
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

selectOutput' :: SetSel (Val (SelStates 𝔹)) -> Endo Fig
selectOutput' δv fig@{ v, dir, γ } = fig { v = v', γ = γ', dir = dir' }
   where
   v' × selType = δv v
   γ' × dir' = case selType of
      Persistent -> if dir.persistent == LinkedInputs then botOf γ × dir { persistent = LinkedOutputs } else γ × dir
      Transient -> if dir.transient == LinkedInputs then botOf γ × dir { transient = LinkedOutputs } else γ × dir
      Unselectable -> γ × dir

selectOutput :: Setter Fig (Val (SelStates 𝔹))
selectOutput δv fig@{ dir, v, γ } = fig
   { v = δv v
   , γ = if dir.persistent == LinkedInputs then botOf γ else γ
   , dir = { persistent: LinkedOutputs, transient: LinkedOutputs }
   }

setOutputView :: Setter Fig View
setOutputView δvw fig = fig
   { out_view = fig.out_view <#> δvw }

selectInput' :: Var -> SetSel (Val (SelStates 𝔹)) -> Endo Fig
selectInput' x δv fig@{ v, dir, γ } = fig { v = v', γ = γ', dir = dir' }
   where
   γ' × selType = envVal' x δv γ
   v' × dir' = case selType of
      Persistent -> if dir.persistent == LinkedOutputs then botOf v × dir { persistent = LinkedInputs } else v × dir
      Transient -> if dir.transient == LinkedOutputs then botOf v × dir { transient = LinkedInputs } else v × dir
      Unselectable -> v × dir

selectInput :: Var -> Setter Fig (Val (SelStates 𝔹))
selectInput x δv fig@{ dir, γ, v } = fig
   { γ = envVal x δv γ
   , v = if dir.persistent == LinkedOutputs then botOf v else v
   , dir = { persistent: LinkedInputs, transient: LinkedInputs }
   }

setInputView :: Var -> Setter Fig View
setInputView x δvw fig = fig
   { in_views = insert x (lookup x fig.in_views # join <#> δvw) fig.in_views
   }

selectIntermediate :: Vertex -> Setter Fig (Val (SelStates 𝔹))
selectIntermediate (Vertex α) δv fig@{ ι } = fig
   { ι = envVal α δv ι
   }

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

type SelectionResult =
   { v :: Val (SelState 𝕊)
   , γ :: Env (SelState 𝕊)
   , inert :: Set DVertex
   , g :: GraphImpl
   }

selectionResult' :: Fig -> Val (SelState 𝔹) -> Env (SelState 𝔹) -> SelectionResult
selectionResult' fig@{ dir } v γ = case dir.persistent of
   LinkedOutputs ->
      let
         v1 × γ1 × g = fig.linkedOutputs v
         report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
      in
         { v: lift2 as𝕊 <$> v <*> v1, γ: (to𝕊 <$> _) <$> report γ1, inert: fig.inertBwd, g }
   LinkedInputs ->
      let
         γ1 × v1 × g = fig.linkedInputs γ
         report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
      in
         { v: (to𝕊 <$> _) <$> report v1, γ: lift2 as𝕊 <$> γ <*> γ1, inert: fig.inertFwd, g }

selectionResult :: Fig -> Selection (Val (SelState 𝕊)) × Selection (Env (SelState 𝕊)) × Selection (Env (SelState 𝔹))
selectionResult fig@{ spec, v, γ } =
   { persistent: persistent.v, transient: transient.v } × { persistent: persistent.γ, transient: transient.γ } × ιs
   where
   v' = splitSelStates v
   γ' = splitSelStates γ
   persistent = selectionResult' fig v'.persistent γ'.persistent
   transient = selectionResult' fig v'.transient γ'.transient

   ιs = flip (maybe { persistent: empty, transient: empty }) spec.query
      \query ->
         let
            vs = runQuery query persistent.g ∪ runQuery query transient.g
            intermediates sel = filterKeys (\α -> not (Vertex α ∈ fig.in_roots)) (selectIntermediates sel.inert sel.g vs)
         in
            { persistent: intermediates persistent, transient: intermediates transient }

drawIntermediates :: HTMLId -> Selection (Env (SelState 𝔹)) -> Array String -> Redraw -> Effect Unit
drawIntermediates divId intermediates unused redraw = do
   let prefix = divId <> "-" <> str.intermediate
   for_ unused \α -> rootSelect ("#" <> prefix <> "-" <> α) >>= remove

   let Env intermediates' = mergeSelStates intermediates

   sequence_ $ flip mapWithKey intermediates' \α v ->
      drawView { divId: prefix, suffix: α, view: unsafePartial $ view α (map to𝕊 <$> v) Nothing }
         (selectIntermediate (Vertex α))
         (setIntermediateView (Vertex α))
         redraw

drawFig :: HTMLId -> Fig -> Effect Unit
drawFig divId fig@{ ι } = do
   drawView { divId, suffix: str.output, view: out_view } selectOutput setOutputView redraw

   sequence_ $ flip mapWithKey in_views \x view -> do
      drawView { divId: divId <> "-" <> str.input, suffix: x, view } (selectInput x) (setInputView x) redraw

   drawIntermediates divId intermediate_values unused redraw
   where
   out_view × in_views × intermediate_values =
      selectionResult fig # unsafePartial
         ( flip (view str.output) fig.out_view <<< mergeSelStates
              *** (\(Env γ) -> mapWithKey view γ <*> fig.in_views) <<< mergeSelStates
              *** identity
         )

   unused :: Array String
   unused = fromFoldable (keys ι \\ (keys intermediate_values.persistent ∪ keys intermediate_values.transient))

   redraw = (_ $ fig { ι = mergeSelStates intermediate_values }) >>> drawFig divId

drawFile :: File × String -> Effect Unit
drawFile (file × src) =
   addEditorView (codeMirrorDiv $ unwrap file) >>= drawCode src

unprojExpr :: forall a. BoundedMeetSemilattice a => Raw EnvExpr -> GaloisConnection (Env a) (EnvExpr a)
unprojExpr (EnvExpr _ e) = GC
   { fwd: \γ -> EnvExpr γ (topOf e)
   , bwd: \(EnvExpr γ _) -> γ
   }

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

      vf :: Val (SelState 𝔹) -> Env (SelState 𝔹) × GraphImpl
      vf v = first ((<*>) γInert) (gcBwd (v <#> to𝔹))

      γf :: Env (SelState 𝔹) -> Val (SelState 𝔹) × GraphImpl
      γf γ = first ((<*>) vInert) (gcFwd (γ <#> to𝔹))

      linkedInputs :: Env (SelState 𝔹) -> Env (SelState 𝔹) × Val (SelState 𝔹) × GraphImpl
      linkedInputs = γf >>> \(v × g) -> (fst $ vf v) × v × g

      linkedOutputs :: Val (SelState 𝔹) -> Val (SelState 𝔹) × Env (SelState 𝔹) × GraphImpl
      linkedOutputs = vf >>> \(γ × g) -> (fst $ γf γ) × γ × g
   pure
      { spec
      , s
      , γ: mergeSelStates $ { persistent: γ_init, transient: γ_init }
      , v: mergeSelStates $ { persistent: v_init, transient: v_init }
      , ι: mergeSelStates $ { persistent: empty, transient: empty }
      , linkedOutputs
      , linkedInputs
      , dir: { persistent: LinkedOutputs, transient: LinkedInputs }
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
