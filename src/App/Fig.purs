module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (SelState(..), SelStates(..), Selection, SelectionType(..), SetSel, 𝕊, getSel, selState, selStates, to𝔹, to𝕊, as𝕊)
import App.Util.Selector (envVal, envVal')
import App.View (view)
import App.View.Util (Direction(..), Fig, FigSpec, HTMLId, Redraw, View, drawView')
import App.View.Util.D3 (remove, rootSelect)
import Bind (Var)
import Control.Apply (lift2)
import Data.Array (fromFoldable, zipWith)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (first)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for_, sequence_)
import Data.Tuple (fst, snd)
import Dict (fromFoldable) as D
import Effect (Effect)
import EvalGraph (graphEval, graphGC, withOp)
import GaloisConnection (GaloisConnection(..), deMorgan)
import Graph (DVertex, DVertex', Vertex(..), runQuery, select𝔹s, vertices)
import Graph.GraphImpl (GraphImpl)
import Lattice (class BoundedMeetSemilattice, Raw, 𝔹, botOf, erase, topOf)
import Module.Web (File(..), loadProgCxt, prepConfig)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Test.Util.Debug (tracing)
import Util (type (×), AffError, Endo, Setter, spyWhen, (×), (∩))
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

selectOutput' :: SetSel (Val (SelStates 𝔹)) -> Endo Fig
selectOutput' δv fig@{ v, dir, γ } = fig { v = v', γ = γ', dir = dir' }
   where
   v' × selType = δv v
   γ' × dir' = case selType of
      Persistent | dir.persistent /= LinkedOutputs -> botOf γ × dir { persistent = LinkedOutputs }
      Transient | dir.transient /= LinkedOutputs -> γ × dir { transient = LinkedOutputs }
      _ -> γ × dir

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
      Persistent | dir.persistent /= LinkedInputs -> botOf v × dir { persistent = LinkedInputs }
      Transient | dir.transient /= LinkedInputs -> v × dir { transient = LinkedInputs }
      _ -> v × dir

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

selectIntermediate' :: Vertex -> SetSel (Val (SelStates 𝔹)) -> Endo Fig
selectIntermediate' (Vertex α) δv fig@{ ι } = fig { ι = ι' }
   where
   ι' × _ = envVal' α δv ι

setIntermediateView :: Vertex -> Setter Fig View
setIntermediateView (Vertex α) δvw fig = fig
   { intermediate_views = insert α (lookup α fig.intermediate_views # join <#> δvw) fig.intermediate_views
   }

selectIntermediates :: Set DVertex -> Selection (Set DVertex) -> Set (DVertex' (Val Vertex)) -> Env (SelStates 𝔹)
selectIntermediates inerts g vs =
   (Env $ D.fromFoldable $ zipWith setSels vs_inert vs_selected) -- using <$> and <*> is more readable, but causes errors
   where
   vs' = (snd <<< unwrap) `Set.map` vs # fromFoldable :: Array (Val Vertex)

   -- Consolidate with analogous calculation with γInert etc in loadFig?
   vs_selected = vs' <#> \v@(Val α _) -> α × { persistent: select𝔹s v g.persistent, transient: select𝔹s v g.transient }
   vs_inert = (\v -> select𝔹s v inerts) <$> vs' :: Array (Val 𝔹)

   setSels :: Val 𝔹 -> Vertex × Selection (Val 𝔹) -> String × Val (SelStates 𝔹)
   setSels inert (Vertex α × v) = α × (selStates <$> inert <*> v.persistent <*> v.transient)

type SelectionResult =
   { v :: Val (SelStates 𝕊)
   , γ :: Env (SelStates 𝕊)
   , ι :: Env (SelStates 𝔹)
   }

selectionResult :: Fig -> SelectionResult
selectionResult fig@{ dir, v, γ, inerts } =
   { v: lift2 as𝕊 <$> v <*> reportOut v', γ: lift2 as𝕊 <$> γ <*> reportIn γ', ι: intermediates fig { persistent: αs, transient: αs' } inerts }
   where
   γ1 × v1 × αs =
      case dir.persistent of
         LinkedOutputs -> fig.linkedOutputs Persistent v
         LinkedInputs -> fig.linkedInputs Persistent γ
   γ2 × v2 × αs' =
      case dir.transient of
         LinkedOutputs -> fig.linkedOutputs Transient v
         LinkedInputs -> fig.linkedInputs Transient γ

   v' = splice v1 v2
   γ' = splice γ1 γ2

   reportIn = spyWhen tracing.mediatingData "Mediating inputs" prettyP
   reportOut = spyWhen tracing.mediatingData "Mediating outputs" prettyP

intermediates :: Fig -> Selection (Set DVertex) -> Set DVertex -> Env (SelStates 𝔹)
intermediates { spec, in_roots } g inerts =
   flip (maybe (empty)) spec.query
      \query ->
         (filterKeys (\α -> not (Vertex α ∈ in_roots))) $
            selectIntermediates inerts g (runQuery query g.persistent ∪ runQuery query g.transient)

drawIntermediates :: HTMLId -> Env (SelStates 𝔹) -> Set String -> Redraw -> Effect Unit
drawIntermediates divId (Env ι) unused redraw = do
   let prefix = divId <> "-" <> str.intermediate
   for_ unused \α -> rootSelect ("#" <> prefix <> "-" <> α) >>= remove
   sequence_ $ flip mapWithKey ι \α v ->
      drawView' { divId: prefix, suffix: α, view: unsafePartial $ view α (map to𝕊 <$> v) Nothing }
         (selectIntermediate' (Vertex α))
         (setIntermediateView (Vertex α))
         redraw

drawFig :: HTMLId -> Fig -> Effect Unit
drawFig divId fig = do
   drawView' { divId, suffix: str.output, view: out_view } selectOutput' setOutputView redraw

   sequence_ $ flip mapWithKey in_views \x view -> do
      drawView' { divId: divId <> "-" <> str.input, suffix: x, view } (selectInput' x) (setInputView x) redraw

   drawIntermediates divId ι (keys fig.ι \\ keys ι) redraw
   where
   { v, γ, ι } = selectionResult fig
   out_view = unsafePartial $ view str.output v fig.out_view
   in_views = (\(Env γ) -> unsafePartial (mapWithKey view γ) <*> fig.in_views) γ
   redraw = (_ $ fig { ι = ι }) >>> drawFig divId

drawFile :: File × String -> Effect Unit
drawFile (File file × src) =
   addEditorView (codeMirrorDiv file) >>= drawCode src

unprojExpr :: forall a. BoundedMeetSemilattice a => Raw EnvExpr -> GaloisConnection (Env a) (EnvExpr a)
unprojExpr (EnvExpr _ e) = GC
   { fwd: \γ -> EnvExpr γ (topOf e)
   , bwd: \(EnvExpr γ _) -> γ
   }

lift
   :: forall f f' g
    . Apply f
   => Apply f'
   => f (𝔹 -> SelState 𝔹)
   -> (f' 𝔹 -> f 𝔹 × g)
   -> f' (SelState 𝔹)
   -> f (SelState 𝔹) × g
lift selState_f bwd v = first (apply selState_f) (bwd (v <#> to𝔹))

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

      inertγ = select𝔹s γα inertBwd
      inertv = select𝔹s outα inertFwd
      γInert = selStates <$> inertγ :: Env (𝔹 -> 𝔹 -> SelStates 𝔹)
      vInert = selStates <$> inertv :: Val (𝔹 -> 𝔹 -> SelStates 𝔹)

      γInert' = selState <$> select𝔹s γα inertBwd
      vInert' = selState <$> select𝔹s outα inertFwd

      vf :: Val (SelState 𝔹) -> Env (SelState 𝔹) × GraphImpl
      vf v = lift γInert' gcBwd v

      γf :: Env (SelState 𝔹) -> Val (SelState 𝔹) × GraphImpl
      γf γ = lift vInert' gcFwd γ

      linkedInputs :: SelectionType -> Env (SelStates 𝔹) -> Env (SelState 𝔹) × Val (SelState 𝔹) × Set DVertex
      linkedInputs selType γ =
         let v × g = γf (γ <#> getSel selType) in fst (vf v) × v × (vertices g)

      linkedOutputs :: SelectionType -> Val (SelStates 𝔹) -> Env (SelState 𝔹) × Val (SelState 𝔹) × Set DVertex
      linkedOutputs selType v =
         let γ × g = vf (v <#> getSel selType) in γ × fst (γf γ) × (vertices g)

   pure
      { spec
      , s
      , γ: γInert <*> γ0 <*> γ0
      , v: vInert <*> v0 <*> v0
      , ι: empty
      , ι': empty
      , linkedOutputs
      , linkedInputs
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
