module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (SelState, 𝕊, as𝕊, getPersistent, getTransient, selState, to𝕊)
import App.Util.Selector (envVal)
import App.View (view)
import App.View.Util (Direction(..), Fig, FigSpec, HTMLId, View, drawView)
import Bind (Var)
import Control.Apply (lift2)
import Data.Array (concat, fromFoldable, zipWith)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (first, (***))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for, sequence_)
import Data.Tuple (fst, snd)
import Effect (Effect)
import EvalGraph (graphEval, graphGC', withOp)
import GaloisConnection (GaloisConnection(..), deMorgan)
import Graph (class Graph, DVertex', Vertex, addresses, runQuery, select𝔹s, vertices)
import Graph.GraphImpl (GraphImpl)
import Lattice (class BoundedMeetSemilattice, Raw, 𝔹, botOf, erase, topOf)
import Module.Web (File, loadProgCxt, prepConfig)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Test.Util.Debug (tracing)
import Util (type (×), AffError, Endo, Setter, spyWhen, (×))
import Util.Map (insert, lookup, mapWithKey)
import Util.Set ((∪), (\\))
import Val (Env(..), EnvExpr(..), Val, unrestrictGC)

str
   :: { output :: String -- pseudo-variable to use as name of output view
      , input :: String -- prefix for input views
      }
str =
   { output: "output"
   , input: "input"
   }

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

combineVals :: forall g. Graph g => Set (DVertex' (Val Vertex)) -> Set (DVertex' (Val Vertex)) -> g -> g -> Array (Val (SelState 𝕊))
combineVals persistents transients gPersistent gTransient =
   vals𝕊
   where
   vertsP = (addresses $ vertices $ gPersistent)
   vertsT = (addresses $ vertices $ gTransient)

   vals = (snd <<< unwrap) `Set.map` (persistents ∪ transients) # fromFoldable

   vsP = (\v -> select𝔹s v vertsP) <$> vals :: Array (Val 𝔹)
   vsT = (\v -> select𝔹s v vertsT) <$> vals :: Array (Val 𝔹)

   setSels :: Val 𝔹 -> Val 𝔹 -> Val (SelState 𝕊)
   setSels vP vT = selState false <$> (to𝕊 <$> vP) <*> (to𝕊 <$> vT)

   vals𝕊 = zipWith setSels vsP vsT

selectionResult :: Fig -> Val (SelState 𝕊) × Env (SelState 𝕊) × Array (Val (SelState 𝕊))
selectionResult fig@{ spec, dir } =
   case dir of
      LinkedOutputs ->
         let
            v1 × γ1 × g × g' = fig.linkedOutputs fig.v
            report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
         in
            (lift2 as𝕊 <$> fig.v <*> v1) × ((to𝕊 <$> _) <$> report γ1) × reportI (intermediates g g')
      LinkedInputs ->
         let
            γ1 × v1 × g × g' = fig.linkedInputs fig.γ
            report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
         in
            ((to𝕊 <$> _) <$> report v1) × (lift2 as𝕊 <$> fig.γ <*> γ1) × reportI (intermediates g g')
   where
   intermediates gPersistent gTransient = concat $ for spec.queries
      \query -> combineVals (runQuery query gPersistent) (runQuery query gTransient) gPersistent gTransient

   reportI = spyWhen tracing.intermediates "Intermediate values: " (map (prettyP <<< erase))

drawFig :: HTMLId -> Fig -> Effect Unit
drawFig divId fig = do
   drawView { divId, suffix: str.output, view: out_view } selectOutput setOutputView redraw
   sequence_ $ flip mapWithKey in_views \x view -> do
      drawView { divId: divId <> "-" <> str.input, suffix: x, view } (selectInput x) (setInputView x) redraw
   where
   redraw = (_ $ fig) >>> drawFig divId
   out_view × in_views × _ =
      selectionResult fig # unsafePartial
         (flip (view str.output) fig.out_view *** (\(Env γ) -> mapWithKey view γ <*> fig.in_views) *** identity)

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
      EnvExpr γ e' = erase eval.inα
      { fwd: focusFwd, bwd: focusBwd } = unwrap (unrestrictGC γ (Set.fromFoldable inputs) >>> unprojExpr (EnvExpr γ e'))

      dualFocusBwd = deMorgan focusFwd

      graphgc = graphGC' eval
      graphgc_op = graphGC' (withOp eval)

      gcBwd :: Val 𝔹 -> Env 𝔹 × GraphImpl
      gcBwd v = first focusBwd (graphgc.bwd v)

      gcFwd :: Env 𝔹 -> Val 𝔹 × GraphImpl
      gcFwd γ = graphgc_op.bwd (dualFocusBwd γ)

      in_views = mapWithKey (\_ _ -> Nothing) (unwrap γ)

      γ0 = botOf γα :: Env 𝔹
      v0 = botOf outα :: Val 𝔹

      verts0 = addresses $ vertices g0
      inertBwd = verts0 \\ (addresses $ vertices $ snd (gcBwd (topOf outα))) :: Set Vertex
      inertFwd = (addresses $ vertices $ snd $ (graphgc.fwd <<< focusFwd) γ0)
      γInert = selState <$> select𝔹s γα inertBwd
      vInert = selState <$> select𝔹s outα inertFwd

      v' = lift γInert gcBwd -- Slice value v back to env γ
      γ' = lift vInert gcFwd -- Slice env γ to val v

      linkedInputs = γ' >>> (\(v × g × g') -> ((fst $ v' v) × v × g × g')) :: (Env (SelState 𝔹)) -> (Env (SelState 𝔹) × Val (SelState 𝔹) × GraphImpl × GraphImpl)

      linkedOutputs = v' >>> (\(γ × g × g') -> ((fst $ γ' γ) × γ × g × g')) :: (Val (SelState 𝔹)) -> (Val (SelState 𝔹) × Env (SelState 𝔹) × GraphImpl × GraphImpl)

   pure { spec, s, γ: γInert <*> γ0 <*> γ0, v: vInert <*> v0 <*> v0, linkedOutputs, linkedInputs, dir: LinkedOutputs, in_views, out_view: Nothing }

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

drawFigWithCode :: { fig :: Fig, divId :: HTMLId } -> Effect Unit
drawFigWithCode { fig, divId } = do
   drawFig divId fig
   addEditorView (codeMirrorDiv divId) >>= drawCode (prettyP fig.s)

drawCode :: String -> EditorView -> Effect Unit
drawCode s ed =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]
