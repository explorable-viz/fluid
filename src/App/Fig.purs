module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (HTMLId, SelState, Selector, 𝕊, as𝕊, selState, to𝕊)
import App.Util.Selector (envVal)
import App.View (drawView, view)
import Bind (Bind, Var, (↦))
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.Set as Set
import Data.Traversable (sequence_)
import Data.Tuple (curry)
import Desugarable (desug)
import Effect (Effect)
import EvalGraph (graphEval, graphGC, withOp)
import GaloisConnection ((***)) as GC
import GaloisConnection (GaloisConnection(..), dual, meet)
import Lattice (class BoundedMeetSemilattice, Raw, 𝔹, botOf, erase, topOf)
import Module (File, initialConfig, loadProgCxt, open)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import SExpr (Expr) as S
import Test.Util.Debug (tracing)
import Util (type (×), AffError, Endo, assert, spyWhen, (×))
import Util.Map (get, mapWithKey)
import Val (Env, EnvExpr(..), Val, unrestrictGC)

type FigSpec =
   { imports :: Array String
   , datasets :: Array (Bind String)
   , file :: File
   , inputs :: Array Var
   }

data Direction = LinkedInputs | LinkedOutputs

type Fig =
   { spec :: FigSpec
   , s :: Raw S.Expr
   , γ :: Env (SelState 𝔹)
   , v :: Val (SelState 𝔹)
   , gc :: GaloisConnection (Env 𝔹) (Val 𝔹)
   , gc_dual :: GaloisConnection (Val 𝔹) (Env 𝔹)
   , dir :: Direction
   }

-- Pseudo-variable to use as name of output view.
output :: String
output = "output"

selectOutput :: Selector Val -> Endo Fig
selectOutput δv fig@{ dir, γ, v } = fig
   { v = assert (v == botOf v) $ δv v -- this should NOT be true in general
   , γ = if dir == LinkedInputs then botOf γ else γ
   , dir = LinkedOutputs
   }

selectInput :: Bind (Selector Val) -> Endo Fig
selectInput (x ↦ δv) fig@{ dir, γ, v } = fig
   { γ = envVal x δv γ
   , v = if dir == LinkedOutputs then botOf v else v
   , dir = LinkedInputs
   }

drawFig :: HTMLId -> Fig -> Effect Unit
drawFig divId fig = do
   drawView divId output (drawFig divId <<< flip selectOutput fig) out_view
   sequence_ $
      mapWithKey (\x -> drawView divId x (drawFig divId <<< flip (curry selectInput x) fig)) in_views
   where
   out_view × in_views =
      selectionResult fig # unsafePartial (view output *** unwrap >>> mapWithKey view)

selectionResult :: Fig -> Val (SelState 𝕊) × Env (SelState 𝕊)
selectionResult fig@{ v, dir: LinkedOutputs } =
   (as𝕊 <$> v <*> (selState <$> v1 <*> v2)) × (to𝕊 <$> report (selState <$> γ1 <*> γ2))
   where
   report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
   GC gc = (fig.gc_dual `GC.(***)` identity) >>> meet >>> fig.gc
   v1 × γ1 = gc.bwd (v <#> unwrap >>> _.persistent)
   v2 × γ2 = gc.bwd (v <#> unwrap >>> _.transient)
selectionResult fig@{ γ, dir: LinkedInputs } =
   (to𝕊 <$> report (selState <$> v1 <*> v2)) ×
      wrap (mapWithKey (\x v -> as𝕊 <$> get x γ <*> v) (unwrap (selState <$> γ1 <*> γ2)))
   where
   report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
   GC gc = (fig.gc `GC.(***)` identity) >>> meet >>> fig.gc_dual
   γ1 × v1 = gc.bwd (γ <#> unwrap >>> _.persistent)
   γ2 × v2 = gc.bwd (γ <#> unwrap >>> _.transient)

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
   pure { spec, s, γ: botOf γα, v: botOf outα, gc, gc_dual, dir: LinkedOutputs }

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

drawFigWithCode :: { fig :: Fig, divId :: HTMLId } -> Effect Unit
drawFigWithCode { fig, divId } = do
   drawFig divId fig
   addEditorView (codeMirrorDiv divId) >>= drawCode (prettyP fig.s)

drawCode :: String -> EditorView -> Effect Unit
drawCode s ed =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]

-- ======================
-- boilerplate
-- ======================

derive instance Eq Direction
