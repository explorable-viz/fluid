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
import EvalGraph (GraphEval, graphEval, graphGC)
import GaloisConnection (GaloisConnection(..), relatedInputs, relatedOutputs)
import Graph.GraphImpl (GraphImpl)
import Lattice (𝔹, Raw, botOf, erase, topOf)
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
   , eval :: GraphEval GraphImpl EnvExpr Val
   , in_ :: EnvExpr 𝔹
   , out :: Val 𝔹
   , dir :: Direction
   }

-- Pseudo-variable to use as name of output view.
output :: String
output = "output"

-- TODO: replace (expensive) botOf in_ by per-variable botOf
selectOutput :: Selector Val -> Endo Fig
selectOutput δv fig@{ dir, in_: EnvExpr γ e, out } = fig
   { out = δv out
   , in_ = if dir == LinkedInputs then EnvExpr (botOf γ) e else EnvExpr γ e
   , dir = LinkedOutputs
   }

selectInput :: Bind (Selector Val) -> Endo Fig
selectInput (x ↦ δv) fig@{ dir, in_: EnvExpr γ e, out } = fig
   { in_ = EnvExpr (envVal x δv γ) e
   , out = if dir == LinkedOutputs then botOf out else out
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

-- Not easy to express as direct composition of Galois connections because of direct use of e.
unfocus :: Fig -> GaloisConnection (Env 𝔹) (Val 𝔹)
unfocus { spec: { inputs }, eval, in_: EnvExpr γ e } = GC
   { fwd: \γ' -> gc.fwd (EnvExpr (unrestrict.fwd γ') (topOf e))
   , bwd: \v -> unrestrict.bwd (gc.bwd v # \(EnvExpr γ'' _) -> γ'')
   }
   where
   GC gc = graphGC eval
   unrestrict = unwrap (unrestrictGC (erase γ) (Set.fromFoldable inputs))

selectionResult :: Fig -> Val Sel × Env Sel
selectionResult fig@{ out, dir: LinkedOutputs } =
   (asSel <$> out <*> out') × map toSel (report γ)
   where
   report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
   out' × γ = (unwrap (relatedOutputs (unfocus fig))).bwd out
selectionResult fig@{ in_: EnvExpr γ _, dir: LinkedInputs } =
   (toSel <$> report out) × wrap (mapWithKey (\x v -> asSel <$> get x γ <*> v) (unwrap γ'))
   where
   report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
   γ' × out = (unwrap (relatedInputs (unfocus fig))).bwd γ

drawFile :: File × String -> Effect Unit
drawFile (file × src) =
   addEditorView (codeMirrorDiv $ unwrap file) >>= drawCode src

loadFig :: forall m. FigSpec -> AffError m Fig
loadFig spec@{ imports, file, datasets } = do
   s <- open file
   e <- desug s
   gconfig <- loadProgCxt imports datasets >>= initialConfig e
   eval@({ inα: EnvExpr γα _, outα }) <- graphEval gconfig e
   pure { spec, s, eval, in_: EnvExpr (botOf γα) (topOf e), out: botOf outα, dir: LinkedOutputs }

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
