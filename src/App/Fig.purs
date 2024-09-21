module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (SelState, 𝕊, as𝕊, getPersistent, getTransient, selState, to𝕊)
import App.Util.Selector (envVal)
import App.View (view)
import App.View.Util (Direction(..), Fig, FigSpec, HTMLId, View, drawView)
import Bind (Var)
import Control.Apply (lift2)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong ((***))
import Data.Set as Set
import Data.Traversable (sequence_)
import Desugarable (desug)
import Effect (Effect)
import EvalGraph (graphEval, graphGC, withOp)
import GaloisConnection ((***)) as GC
import GaloisConnection (GaloisConnection(..), dual, meet)
import Lattice (class BoundedMeetSemilattice, Raw, 𝔹, botOf, erase, neg, topOf)
import Module (File, initialConfig, loadProgCxt, open)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Test.Util.Debug (tracing)
import Util (type (×), AffError, Endo, Setter, spyWhen, (×))
import Util.Map (insert, lookup, mapWithKey)
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

selectionResult :: Fig -> Val (SelState 𝕊) × Env (SelState 𝕊)
selectionResult fig@{ v, dir: LinkedOutputs } =
   (lift2 as𝕊 <$> v <*> v1) × ((to𝕊 <$> _) <$> report γ1)
   where
   v1 × γ1 = (unwrap fig.linkedOutputs).bwd v
   report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
selectionResult fig@{ γ, dir: LinkedInputs } =
   ((to𝕊 <$> _) <$> report v1) × (lift2 as𝕊 <$> γ <*> γ1)
   where
   γ1 × v1 = (unwrap fig.linkedInputs).bwd γ
   report = spyWhen tracing.mediatingData "Mediating outputs" prettyP

drawFig :: HTMLId -> Fig -> Effect Unit
drawFig divId fig = do
   drawView { divId, suffix: str.output, view: out_view } selectOutput setOutputView redraw
   sequence_ $ flip mapWithKey in_views \x view -> do
      drawView { divId: divId <> "-" <> str.input, suffix: x, view } (selectInput x) (setInputView x) redraw
   where
   redraw = (_ $ fig) >>> drawFig divId
   out_view × in_views =
      selectionResult fig # unsafePartial
         (flip (view str.output) fig.out_view *** \(Env γ) -> mapWithKey view γ <*> fig.in_views)

drawFile :: File × String -> Effect Unit
drawFile (file × src) =
   addEditorView (codeMirrorDiv $ unwrap file) >>= drawCode src

unprojExpr :: forall a. BoundedMeetSemilattice a => Raw EnvExpr -> GaloisConnection (Env a) (EnvExpr a)
unprojExpr (EnvExpr _ e) = GC
   { fwd: \γ -> EnvExpr γ (topOf e)
   , bwd: \(EnvExpr γ _) -> γ
   }

lift
   :: forall f g
    . Apply f
   => Apply g
   => f (𝔹 -> 𝔹 -> SelState 𝔹)
   -> g (𝔹 -> 𝔹 -> SelState 𝔹)
   -> GaloisConnection (f 𝔹) (g 𝔹)
   -> GaloisConnection (f (SelState 𝔹)) (g (SelState 𝔹))
lift selState_f selState_g (GC gc) = GC { bwd, fwd }
   where
   fwd :: f (SelState 𝔹) -> g (SelState 𝔹)
   fwd γ = selState_g <*> gc.fwd (γ <#> getPersistent) <*> gc.fwd (γ <#> getTransient)

   bwd :: g (SelState 𝔹) -> f (SelState 𝔹)
   bwd v = selState_f <*> gc.bwd (v <#> getPersistent) <*> gc.bwd (v <#> getTransient)

loadFig :: forall m. FigSpec -> AffError m Fig
loadFig spec@{ inputs, imports, file, datasets } = do
   s <- open file
   e <- desug s
   gconfig <- loadProgCxt imports datasets >>= initialConfig e
   eval@({ inα: EnvExpr γα _, outα }) <- graphEval gconfig e
   let
      EnvExpr γ e' = erase eval.inα
      focus = unrestrictGC γ (Set.fromFoldable inputs) >>> unprojExpr (EnvExpr γ e')
      gc = focus >>> graphGC eval
      gc_dual = graphGC (withOp eval) >>> dual focus
      in_views = mapWithKey (\_ _ -> Nothing) (unwrap γ)

      γ0 = botOf γα
      v0 = botOf outα
      γInert = selState <$> neg (unwrap gc).bwd (topOf outα) -- want to simplify this for ease of computation (attempts similar to v0 result in a lack of inert data)
      vInert = selState <$> (unwrap gc).fwd γ0

      linkedInputs = ((lift γInert vInert gc) `GC.(***)` identity) >>> meet >>> (lift vInert γInert gc_dual)
      linkedOutputs = ((lift vInert γInert gc_dual) `GC.(***)` identity) >>> meet >>> (lift γInert vInert gc)

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
