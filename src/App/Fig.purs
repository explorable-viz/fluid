module App.Fig where

import Prelude hiding (absurd)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (HTMLId, Sel, asSel, toSel)
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
import GaloisConnection (GaloisConnection(..), dual, meet)
import GaloisConnection ((***)) as GC
import Lattice (class BoundedMeetSemilattice, Raw, 𝔹, botOf, erase, topOf)
import Module (File, initialConfig, loadProgCxt, open)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import SExpr (Expr) as S
import Test.Util (Selector)
import Test.Util.Debug (tracing)
import Util (type (×), AffError, Endo, spyWhen, (×))
import Util.Map (get, mapWithKey)
import Val (Env, EnvExpr(..), Val, unrestrictGC)

type FigSpec =
   { divId :: HTMLId
   , imports :: Array String
   , datasets :: Array (Bind String)
   , file :: File
   , inputs :: Array Var
   }

data Direction = LinkedInputs | LinkedOutputs

type Fig =
   { spec :: FigSpec
   , s :: Raw S.Expr
   , γ :: Env 𝔹
   , v :: Val 𝔹
   , gc :: GaloisConnection (Env 𝔹) (Val 𝔹)
   , gc_dual :: GaloisConnection (Val 𝔹) (Env 𝔹)
   , dir :: Direction
   }

-- Pseudo-variable to use as name of output view.
output :: String
output = "output"

-- TODO: replace (expensive) botOf γ by per-variable botOf
selectOutput :: Selector Val -> Endo Fig
selectOutput δv fig@{ dir, γ, v } = fig
   { v = δv v
   , γ = if dir == LinkedInputs then botOf γ else γ
   , dir = LinkedOutputs
   }

selectInput :: Bind (Selector Val) -> Endo Fig
selectInput (x ↦ δv) fig@{ dir, γ, v } = fig
   { γ = envVal x δv γ
   , v = if dir == LinkedOutputs then botOf v else v
   , dir = LinkedInputs
   }

drawFig :: Fig -> Effect Unit
drawFig fig@{ spec: { divId } } = do
   drawView divId output (drawFig <<< flip selectOutput fig) out_view
   sequence_ $ mapWithKey (\x -> drawView divId x (drawFig <<< flip (curry selectInput x) fig)) in_views
   where
   out_view × in_views =
      selectionResult fig
         # unsafePartial (view output *** unwrap >>> mapWithKey view)

selectionResult :: Fig -> Val Sel × Env Sel
selectionResult fig@{ v, dir: LinkedOutputs } =
   (asSel <$> v <*> v') × map toSel (report γ)
   where
   report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
   v' × γ = (unwrap ((fig.gc_dual `GC.(***)` identity) >>> meet >>> fig.gc)).bwd v
selectionResult fig@{ γ, dir: LinkedInputs } =
   (toSel <$> report out) × wrap (mapWithKey (\x v -> asSel <$> get x γ <*> v) (unwrap γ'))
   where
   report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
   γ' × out = (unwrap ((fig.gc `GC.(***)` identity) >>> meet >>> fig.gc_dual)).bwd γ

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

drawFigWithCode :: Fig -> Effect Unit
drawFigWithCode fig = do
   drawFig fig
   addEditorView (codeMirrorDiv fig.spec.divId) >>= drawCode (prettyP fig.s)

drawCode :: String -> EditorView -> Effect Unit
drawCode s ed =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]

-- ======================
-- boilerplate
-- ======================

derive instance Eq Direction
