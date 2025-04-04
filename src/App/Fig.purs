module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (SelState(..), SelStates(..), Selection, SelectionType(..), 𝕊, Selector', as𝕊, getSel, selState, selStates, to𝔹, to𝕊)
import App.Util.Selector (envVal')
import App.View (view)
import App.View.Util (Direction(..), Fig, FigSpec, HTMLId, Redraw, View, drawView')
import App.View.Util.D3 (remove, rootSelect)
import Bind (Var)
import Control.Apply (lift2)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Strong (first)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for_, sequence_)
import Data.Tuple (fst, snd)
import Dict (Dict)
import Dict (fromFoldable) as D
import Effect (Effect)
import EvalGraph (graphEval, graphGC, withOp)
import GaloisConnection (GaloisConnection(..), deMorgan)
import Graph (DVertex, Vertex(..), runQuery, selectαs, select𝔹s, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.Slice (bwdSlice)
import Lattice (class BoundedMeetSemilattice, Raw, 𝔹, botOf, erase, topOf)
import Module.Web (File(..), loadProgCxt, prepConfig)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Test.Util.Debug (tracing)
import Util (type (×), AffError, Endo, Setter, absurd, error, spyWhen, (×), (∩))
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

selectOutput :: Selector' Val -> Endo Fig
selectOutput δv fig@{ v, dir, γ } = fig { v = v', γ = γ', dir = dir' }
   where
   v' × selType = δv v
   γ' × dir' = case selType of
      Persistent | dir.persistent /= LinkedOutputs -> botOf γ × dir { persistent = LinkedOutputs }
      Transient | dir.transient /= LinkedOutputs -> γ × dir { transient = LinkedOutputs }
      _ -> γ × dir

setOutputView :: Setter Fig View
setOutputView δvw fig = fig
   { out_view = fig.out_view <#> δvw }

selectInput :: Var -> Selector' Val -> Endo Fig
selectInput x δv fig@{ v, dir, γ } = fig { v = v', γ = γ', dir = dir' }
   where
   γ' × selType = envVal' x δv γ
   v' × dir' = case selType of
      Persistent | dir.persistent /= LinkedInputs -> botOf v × dir { persistent = LinkedInputs }
      Transient | dir.transient /= LinkedInputs -> v × dir { transient = LinkedInputs }
      _ -> v × dir

setInputView :: Var -> Setter Fig View
setInputView x δvw fig = fig
   { in_views = insert x (lookup x fig.in_views # join <#> δvw) fig.in_views
   }

selectIntermediate :: Vertex -> Selector' Val -> Endo Fig
selectIntermediate (Vertex α) δv fig@{ ι, dir, γ, v } = fig { ι = ι_final, γ = γ', v = v', dir = dir' }
   where
   ι' × selType = envVal' α δv ι
   γ' × v' × dir' × ι_final = case selType of
      Transient | dir.transient /= Intermediates -> γ × v × dir { transient = Intermediates } × ι'
      Transient -> γ × v × dir × ι'
      _ -> γ × v × dir × ι

setIntermediateView :: Vertex -> Setter Fig View
setIntermediateView (Vertex α) δvw fig = fig
   { intermediate_views = insert α (lookup α fig.intermediate_views # join <#> δvw) fig.intermediate_views
   }

rebuildι :: Set DVertex -> Selection (Set DVertex) -> Dict (Val Vertex) -> Env (SelStates 𝔹)
rebuildι inerts αs ι =
   (Env $ D.fromFoldable $ setSels <$> vs_inert <*> vs_selected)
   where
   -- Consolidate with analogous calculation with γInert etc in loadFig?
   vs_inert = ι <#> \v -> select𝔹s v inerts
   vs_selected = ι <#> \v@(Val α _) -> α × { persistent: select𝔹s v αs.persistent, transient: select𝔹s v αs.transient }

   setSels :: Val 𝔹 -> Vertex × Selection (Val 𝔹) -> String × Val (SelStates 𝔹)
   setSels inert (Vertex α × v) = α × (selStates <$> inert <*> v.persistent <*> v.transient)

type SelectionResult =
   { v :: Val (SelStates 𝕊)
   , γ :: Env (SelStates 𝕊)
   , ι :: Env (SelStates 𝔹)
   , ια :: Env Vertex
   }

selectionResult :: Fig -> SelectionResult
selectionResult fig@{ dir, v, γ, ι, ια } =
   { v: lift2 as𝕊 <$> v <*> reportOut v', γ: lift2 as𝕊 <$> γ <*> reportIn γ', ι: ι', ια: ια' }
   where
   γ1 × v1 × αs =
      case dir.persistent of
         LinkedOutputs -> fig.linkedOutputs Persistent v
         LinkedInputs -> fig.linkedInputs Persistent γ
         Intermediates -> error absurd
   γ2 × v2 × αs' =
      case dir.transient of
         LinkedOutputs -> fig.linkedOutputs Transient v
         LinkedInputs -> fig.linkedInputs Transient γ
         Intermediates -> fig.linkIntermediates ι ια

   ι' × ια' = intermediates fig { persistent: αs, transient: αs' }

   v' = splice v1 v2
   γ' = splice γ1 γ2

   reportIn = spyWhen tracing.mediatingData ("Mediating inputs") prettyP
   reportOut = spyWhen tracing.mediatingData ("Mediating outputs") prettyP

intermediates :: Fig -> Selection (Set DVertex) -> Env (SelStates 𝔹) × Env Vertex
intermediates { spec, in_roots, inerts } αs =
   flip (maybe (empty × empty)) spec.query
      \query ->
         let
            ι = filterKeys (\α -> not (Vertex α ∈ in_roots))
               $ runQuery query
               $ αs.persistent ∪ αs.transient
         in
            rebuildι inerts αs ι × Env ι

drawIntermediates :: HTMLId -> Env (SelStates 𝔹) -> Set String -> Redraw -> Effect Unit
drawIntermediates divId (Env ι) unused redraw = do
   let prefix = divId <> "-" <> str.intermediate
   for_ unused \α -> rootSelect ("#" <> prefix <> "-" <> α) >>= remove
   sequence_ $ flip mapWithKey ι \α v ->
      drawView' { divId: prefix, suffix: α, view: unsafePartial $ view α (map to𝕊 <$> v) Nothing }
         (selectIntermediate (Vertex α))
         (setIntermediateView (Vertex α))
         redraw

drawFig :: HTMLId -> Fig -> Effect Unit
drawFig divId fig = do
   drawView' { divId, suffix: str.output, view: out_view } selectOutput setOutputView redraw

   sequence_ $ flip mapWithKey in_views \x view -> do
      drawView' { divId: divId <> "-" <> str.input, suffix: x, view } (selectInput x) (setInputView x) redraw

   drawIntermediates divId ι (keys fig.ι \\ keys ι) redraw
   where
   { v, γ, ι, ια } = selectionResult fig
   out_view = unsafePartial $ view str.output v fig.out_view
   in_views = (\(Env γ) -> unsafePartial (mapWithKey view γ) <*> fig.in_views) γ
   redraw = (_ $ fig { ι = ι, ια = ια }) >>> drawFig divId

drawFile :: File × String -> Effect Unit
drawFile (File file × src) =
   addEditorView (codeMirrorDiv file) >>= drawCode src

unprojExpr :: forall a. BoundedMeetSemilattice a => Raw EnvExpr -> GaloisConnection (Env a) (EnvExpr a)
unprojExpr (EnvExpr _ e) = GC
   { fwd: \γ -> EnvExpr γ (topOf e)
   , bwd: \(EnvExpr γ _) -> γ
   }

type IO a = { γ :: Env a, v :: Val a }

lift
   :: forall f f' g
    . Apply f
   => Apply f'
   => f (𝔹 -> SelState 𝔹)
   -> (f' 𝔹 -> f 𝔹 × g)
   -> f' (SelState 𝔹)
   -> f (SelState 𝔹) × g
lift selState_f f v = first (apply selState_f) (f (v <#> to𝔹))

loadFig :: forall m. FigSpec -> AffError m Fig
loadFig spec@{ fluidSrcPaths, inputs, imports, file, datasets } = do
   progCxt <- loadProgCxt fluidSrcPaths imports datasets
   { s, e, gconfig } <- prepConfig fluidSrcPaths file progCxt
   eval@({ inα: EnvExpr γα _, outα, g: g0 }) <- graphEval gconfig e
   let
      opEval = withOp eval
      inputs' = Set.fromFoldable inputs
      EnvExpr γ e' = erase eval.inα
      GC focus = unrestrictGC γ inputs' >>> unprojExpr (EnvExpr γ e')
      Env γ_restricted = restrict inputs' γα
      in_roots = Set.fromFoldable $ (\(Val α _) -> α) <$> γ_restricted

      ι_fwd' :: Env Vertex -> Env 𝔹 -> Val 𝔹 × Set DVertex
      ι_fwd' ι_α ι_𝔹 =
         let
            αs = vertices $ bwdSlice (selectαs ι_𝔹 ι_α × opEval.g)
         in
            select𝔹s outα αs × αs

      ι_bwd' :: Env Vertex -> Env 𝔹 -> Env 𝔹 × Set DVertex
      ι_bwd' ι_α ι_𝔹 =
         let
            αs = vertices $ bwdSlice (selectαs ι_𝔹 ι_α × eval.g)
         in
            select𝔹s γα αs × αs

      graphgc = graphGC eval
      graphgc_op = graphGC opEval

      gcBwd :: Val 𝔹 -> Env 𝔹 × GraphImpl
      gcBwd v = first focus.bwd (graphgc.bwd v)

      gcFwd :: Env 𝔹 -> Val 𝔹 × GraphImpl
      gcFwd γ = graphgc_op.bwd (deMorgan focus.fwd γ)

      in_views = const Nothing <$> γ_restricted
      unselected = { γ: botOf γα, v: botOf outα } :: IO 𝔹

      inertBwd = vertices g0 \\ (vertices $ snd $ gcBwd $ topOf outα)
      inertFwd = vertices $ snd $ graphgc.fwd $ focus.fwd unselected.γ

      inert = { γ: select𝔹s γα inertBwd, v: select𝔹s outα inertFwd } :: IO 𝔹
      inert' = { γ: selState <$> inert.γ, v: selState <$> inert.v } :: IO (𝔹 -> SelState 𝔹)

      vf :: Val (SelState 𝔹) -> Env (SelState 𝔹) × GraphImpl
      vf = lift inert'.γ gcBwd

      γf :: Env (SelState 𝔹) -> Val (SelState 𝔹) × GraphImpl
      γf = lift inert'.v gcFwd

      ι_fwd :: Env Vertex -> Env (SelState 𝔹) -> Val (SelState 𝔹) × Set DVertex
      ι_fwd ια = lift inert'.v (ι_fwd' ια)

      ι_bwd :: Env Vertex -> Env (SelState 𝔹) -> Env (SelState 𝔹) × Set DVertex
      ι_bwd ια = lift inert'.γ (ι_bwd' ια)

      linkedInputs :: SelectionType -> Env (SelStates 𝔹) -> Env (SelState 𝔹) × Val (SelState 𝔹) × Set DVertex
      linkedInputs selType γ =
         let v × g = γf (γ <#> getSel selType) in fst (vf v) × v × (vertices g)

      linkedOutputs :: SelectionType -> Val (SelStates 𝔹) -> Env (SelState 𝔹) × Val (SelState 𝔹) × Set DVertex
      linkedOutputs selType v =
         let γ × g = vf (v <#> getSel selType) in γ × fst (γf γ) × (vertices g)

      linkIntermediates :: Env (SelStates 𝔹) -> Env Vertex -> Env (SelState 𝔹) × Val (SelState 𝔹) × Set DVertex
      linkIntermediates ι ια =
         let
            v × αs = ι_fwd ια (ι <#> getSel Transient)
            γ × αs' = ι_bwd ια (ι <#> getSel Transient)
         in
            γ × v × (αs ∪ αs')

   pure
      { spec
      , s
      , γ: selStates <$> inert.γ <*> unselected.γ <*> unselected.γ
      , v: selStates <$> inert.v <*> unselected.v <*> unselected.v
      , ι: empty
      , ια: empty
      , linkedOutputs
      , linkedInputs
      , linkIntermediates
      , dir: { persistent: LinkedOutputs, transient: LinkedOutputs }
      , in_views
      , out_view: Nothing
      , intermediate_views: empty
      , in_roots
      , inerts: inertFwd ∩ inertBwd
      }

splice :: forall f a. Apply f => f (SelState a) -> f (SelState a) -> f (SelStates a)
splice f1 f2 = splice' <$> f1 <*> f2

splice' :: forall a. SelState a -> SelState a -> SelStates a
splice' Inert _ = SelStates Inert
splice' _ Inert = SelStates Inert
splice' (Reactive persistent) (Reactive transient) =
   SelStates (Reactive { persistent, transient })

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

drawFigWithCode :: { fig :: Fig, divId :: HTMLId } -> Effect Unit
drawFigWithCode { fig, divId } = do
   drawFig divId fig
   addEditorView (codeMirrorDiv divId) >>= drawCode (prettyP fig.s)

drawCode :: String -> EditorView -> Effect Unit
drawCode s ed =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]
