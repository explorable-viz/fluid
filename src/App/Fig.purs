module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (ReactState, 𝕊, as𝕊, getPersistent, getTransient, kindOfBot, reactState, to𝕊, vReact)
import App.Util.Selector (envVal)
import App.View (view)
import App.View.Util (Direction(..), Fig, FigSpec, HTMLId, View, drawView)
import Bind (Var)
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

selectOutput :: Setter Fig (Val (ReactState 𝔹))
selectOutput δv fig@{ dir, γ, v } = fig
   { v = δv v
   , γ = if dir == LinkedInputs then kindOfBot <$> γ else γ
   , dir = LinkedOutputs
   }

setOutputView :: Setter Fig View
setOutputView δvw fig = fig
   { out_view = fig.out_view <#> δvw
   }

selectInput :: Var -> Setter Fig (Val (ReactState 𝔹))
selectInput x δv fig@{ dir, γ, v } = fig
   { γ = envVal x δv γ
   , v = if dir == LinkedOutputs then kindOfBot <$> v else v
   , dir = LinkedInputs
   }

setInputView :: Var -> Setter Fig View
setInputView x δvw fig = fig
   { in_views = insert x (lookup x fig.in_views # join <#> δvw) fig.in_views
   }

-- generalise Env, Val to f,g?
lift :: GaloisConnection (Env 𝔹) (Val 𝔹) -> GaloisConnection (Env (ReactState 𝔹)) (Val (ReactState 𝔹))
lift (GC gc) = (GC { bwd: bwd1, fwd: fwd1 })
   where
   fwd1 :: Env (ReactState 𝔹) -> Val (ReactState 𝔹)
   fwd1 γ = reactState <$> v0 <*> v1 <*> v2
      where
      v0 = gc.fwd (botOf γ)
      v1 = gc.fwd (γ <#> getPersistent)
      v2 = gc.fwd (γ <#> getTransient)

   bwd1 :: Val (ReactState 𝔹) -> Env (ReactState 𝔹)
   bwd1 v = reactState <$> v0 <*> v1 <*> v2
      where
      v0 = gc.bwd (botOf v)
      v1 = gc.bwd (v <#> getPersistent)
      v2 = gc.bwd (v <#> getTransient)

liftdual :: GaloisConnection (Val 𝔹) (Env 𝔹) -> GaloisConnection (Val (ReactState 𝔹)) (Env (ReactState 𝔹))
liftdual (GC gc) = (GC { bwd: bwd1, fwd: fwd1 })
   where
   fwd1 :: Val (ReactState 𝔹) -> Env (ReactState 𝔹)
   fwd1 γ = reactState <$> v0 <*> v1 <*> v2
      where
      v0 = gc.fwd (botOf γ)
      v1 = gc.fwd (γ <#> getPersistent)
      v2 = gc.fwd (getTransient <$> γ)

   bwd1 :: Env (ReactState 𝔹) -> Val (ReactState 𝔹)
   bwd1 v = reactState <$> v0 <*> v1 <*> v2
      where
      v0 = gc.bwd (botOf v)
      v1 = gc.bwd (v <#> getPersistent)
      v2 = gc.bwd (v <#> getTransient)

selectionResult :: Fig -> Val (ReactState 𝕊) × Env (ReactState 𝕊)
selectionResult fig@{ v, dir: LinkedOutputs } =
   (as𝕊 <$> v <*> v1) × (to𝕊 <$> report γ1)
   where
   report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
   GC gc = fig.gc
   v1 × γ1 = gc.bwd (v)

selectionResult fig@{ γ, dir: LinkedInputs } =
   (to𝕊 <$> report v1) × (as𝕊 <$> γ <*> γ1)
   where
   report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
   GC gc = fig.gc_dual
   γ1 × v1 = gc.bwd (γ)

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

loadFig :: forall m. FigSpec -> AffError m Fig
loadFig spec@{ inputs, imports, file, datasets } = do
   s <- open file
   e <- desug s
   gconfig <- loadProgCxt imports datasets >>= initialConfig e
   eval@({ inα: EnvExpr γα _, outα }) <- graphEval gconfig e
   let
      EnvExpr γ e' = erase eval.inα
      focus = unrestrictGC γ (Set.fromFoldable inputs) >>> unprojExpr (EnvExpr γ e')
      gc1 = focus >>> graphGC eval
      gc1_dual = graphGC (withOp eval) >>> dual focus
      in_views = mapWithKey (\_ _ -> Nothing) (unwrap γ)

      γ0 = neg (unwrap gc1).bwd (topOf outα)
      v0 = neg (unwrap gc1_dual).bwd (topOf γα)
      gc_dual = ((lift gc1) `GC.(***)` identity) >>> meet >>> (liftdual gc1_dual)
      gc = ((liftdual gc1_dual) `GC.(***)` identity) >>> meet >>> (lift gc1)
   {-v: botOf outα
   γ: botOf γα-}

   pure { spec, s, γ: vReact <$> γ0 <*> botOf γα, v: vReact <$> v0 <*> botOf outα, gc, gc_dual, dir: LinkedOutputs, in_views, out_view: Nothing }

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

drawFigWithCode :: { fig :: Fig, divId :: HTMLId } -> Effect Unit
drawFigWithCode { fig, divId } = do
   drawFig divId fig
   addEditorView (codeMirrorDiv divId) >>= drawCode (prettyP fig.s)

drawCode :: String -> EditorView -> Effect Unit
drawCode s ed =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]
