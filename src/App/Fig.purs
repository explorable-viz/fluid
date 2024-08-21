module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (ReactState, SelState, 𝕊, asℝ, getPersistent, getTransient, reactState, selState, toℝ)
import App.Util.Selector (envVal)
import App.View (view)
import App.View.Util (Direction(..), Fig, FigSpec, HTMLId, View, drawView)
import Bind (Var)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
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
import Util.Map (get, insert, lookup, mapWithKey)
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

-- I want to take a gc, (possibly with dual) on Env B and Val B, and produce a connection on Env rs and Val rs
-- to deal with rs reasonably, I need to define what an inert set is - which we can do as v0
-- should this be more explicitly done on tuples?
-- from an input as B, I can obtain a ReactState S, it's just whether I wish to.
lift :: GaloisConnection (Env 𝔹) (Val 𝔹) -> GaloisConnection (Env (ReactState 𝔹)) (Val (ReactState 𝔹))
lift (GC gc) = (GC { bwd: bwd1, fwd: fwd1 })
   where
   fwd1 :: Env (ReactState 𝔹) -> Val (ReactState 𝔹)
   fwd1 γ = reactState <$> v0 <*> v1 <*> v2
      where
      -- should v0 be gc_dual with a bwd
      v0 = neg gc.fwd (topOf γ)
      v1 = gc.fwd (γ <#> getPersistent)
      v2 = gc.fwd (γ <#> getTransient)

   -- of course, everything here is forced to be primary

   bwd1 :: Val (ReactState 𝔹) -> Env (ReactState 𝔹)
   bwd1 v = reactState <$> v0 <*> v1 <*> v2
      where
      v0 = neg gc.bwd (topOf v)
      v1 = gc.bwd (v <#> getPersistent)
      v2 = gc.bwd (v <#> getTransient)

{-}
selectionResultLift :: Fig -> Val (ReactState 𝕊) × Env (ReactState 𝕊)
selectionResultLift fig@{ v, dir: LinkedOutputs } =
   (arℝ <$> v <*> v1) × (to𝕊 <$> report (y1))
   where
   report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
   GC gc = lift (fig.gc_dual `GC.(***)` identity) >>> meet >>> fig.gc
 - Lift doesn't act on tuples rn, but I don't think this is a problem yet?
   γ1 = gc.bwd (v)
   v1 = gc.fwd (γ1)

selectionResultLift fig@{ v0, γ, dir: LinkedInputs } =
   (toℝ <$> v0 <*> report (selState <$> v1 <*> v2)) ×
      wrap (mapWithKey (\x v -> asℝ <$> get x γ <*> v) (unwrap (selState <$> γ1 <*> γ2)))
   where
   report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
   GC gc = (fig.gc `GC.(***)` identity) >>> meet >>> fig.gc_dual
   γ1 × v1 = gc.bwd (γ <#> unwrap >>> _.persistent)
   γ2 × v2 = gc.bwd (γ <#> unwrap >>> _.transient)
-}

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

selectionResult :: Fig -> Val (ReactState 𝕊) × Env (ReactState 𝕊)
selectionResult fig@{ γ0, v, dir: LinkedOutputs } =
   (asℝ <$> v <*> (selState <$> v1 <*> v2)) × (toℝ <$> γ0 <*> report (selState <$> γ1 <*> γ2))
   where
   report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
   GC gc = (fig.gc_dual `GC.(***)` identity) >>> meet >>> fig.gc

   v1 × γ1 = gc.bwd (_.persistent <$> (unwrap <$> v))
   v2 × γ2 = gc.bwd (v <#> unwrap >>> _.transient)

selectionResult fig@{ v0, γ, dir: LinkedInputs } =
   (toℝ <$> v0 <*> report (selState <$> v1 <*> v2)) ×
      wrap (mapWithKey (\x v -> asℝ <$> get x γ <*> v) (unwrap (selState <$> γ1 <*> γ2)))
   where
   report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
   GC gc = (fig.gc `GC.(***)` identity) >>> meet >>> fig.gc_dual
   γ1 × v1 = gc.bwd (γ <#> unwrap >>> _.persistent)
   γ2 × v2 = gc.bwd (γ <#> unwrap >>> _.transient)

--_ × v0 = neg (gc.bwd (topOf γ))

drawFile :: File × String -> Effect Unit
drawFile (file × src) =
   addEditorView (codeMirrorDiv $ unwrap file) >>= drawCode src

unprojExpr :: forall a. BoundedMeetSemilattice a => Raw EnvExpr -> GaloisConnection (Env a) (EnvExpr a)
unprojExpr (EnvExpr _ e) = GC
   { fwd: \γ -> EnvExpr γ (topOf e)
   , bwd: \(EnvExpr γ _) -> γ
   }

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

      γ0 = neg (unwrap gc).bwd (topOf outα)
      v0 = neg (unwrap gc_dual).bwd (topOf γα)
   pure { spec, s, γ: botOf γα, v: botOf outα, gc, gc_dual, dir: LinkedOutputs, in_views, out_view: Nothing, γ0, v0 }

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

drawFigWithCode :: { fig :: Fig, divId :: HTMLId } -> Effect Unit
drawFigWithCode { fig, divId } = do
   drawFig divId fig
   addEditorView (codeMirrorDiv divId) >>= drawCode (prettyP fig.s)

drawCode :: String -> EditorView -> Effect Unit
drawCode s ed =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]
