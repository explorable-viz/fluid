module App.Fig where

import Prelude hiding (absurd)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (HTMLId, Sel, asSel, toSel)
import App.Util.Selector (envVal)
import App.View (drawView, view)
import Bind (Bind, Var, (↦))
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (first, (***))
import Data.Set as Set
import Data.Traversable (sequence, sequence_)
import Data.Tuple (curry, fst)
import Desugarable (desug)
import Dict (filterKeys, get, mapWithKey)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import EvalGraph (GraphEval, graphGC)
import Expr (Expr)
import GaloisConnection (GaloisConnection(..), relatedInputs, relatedOutputs)
import Graph.GraphImpl (GraphImpl)
import Lattice (𝔹, Raw, botOf, erase, topOf)
import Module (File, initialConfig, loadProgCxt, open)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import SExpr (Expr) as S
import Test.Util (Selector)
import Test.Util.Debug (tracing)
import Util (type (+), type (×), AffError, Endo, spy, spyWhen, (×))
import Val (Env, Val, unrestrictGC)

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

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
   , gc :: GraphEval GraphImpl
   , in_ :: Env 𝔹 × Expr 𝔹
   , out :: Val 𝔹
   , dir :: Direction
   }

runAffs_ :: forall a. (a -> Effect Unit) -> Array (Aff a) -> Effect Unit
runAffs_ f as = flip runAff_ (sequence as) case _ of
   Left err -> log $ show err
   Right as' -> as' <#> f # sequence_

split :: Selector Val + Selector Val -> Selector Val × Selector Val
split (Left δv) = δv × identity
split (Right δv) = identity × δv

drawFigWithCode :: Fig -> Effect Unit
drawFigWithCode fig = do
   drawFig fig
   drawCode (prettyP fig.s) =<< addEditorView (codeMirrorDiv fig.spec.divId)

-- Pseudo-variable to use as name of output view.
output :: String
output = "output"

-- TODO: replace (expensive) botOf γ by per-variable botOf
selectOutput :: Selector Val -> Endo Fig
selectOutput δv fig@{ dir, in_, out } = fig
   { out = δv out
   , in_ = if dir == LinkedInputs then first botOf in_ else in_
   , dir = LinkedOutputs
   }

selectInput :: Bind (Selector Val) -> Endo Fig
selectInput (x ↦ δv) fig@{ dir, in_, out } = fig
   { in_ = first (envVal x δv) in_
   , out = if dir == LinkedOutputs then botOf out else out
   , dir = LinkedInputs
   }

drawFig :: Fig -> Effect Unit
drawFig fig@{ spec: { divId } } = do
   let out_view × in_views = selectionResult fig # unsafePartial (view output *** mapWithKey view)
   drawView divId output (drawFig <<< flip selectOutput fig) out_view
   sequence_ $ mapWithKey (\x -> drawView divId x (drawFig <<< flip (curry selectInput x) fig)) in_views

-- TODO: express more directly as composition of Galois connections.
unfocus :: Fig -> GaloisConnection (Env 𝔹) (Val 𝔹)
unfocus { spec: { inputs }, gc: { gc: GC gc }, in_: γ × e } = GC
   { fwd: \γ' -> gc.fwd (unrestrict.fwd γ' × topOf e)
   , bwd: \v -> unrestrict.bwd (gc.bwd v # fst)
   }
   where
   unrestrict = unwrap (unrestrictGC (erase <$> γ) (Set.fromFoldable inputs))

selectionResult :: Fig -> Val Sel × Env Sel
selectionResult fig@{ out, dir: LinkedOutputs } =
   (asSel <$> out <*> out') × map (toSel <$> _) (report γ)
   where
   report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
   out' × γ = (unwrap (relatedOutputs (unfocus fig))).bwd (spy "Selected outputs" prettyP out)
selectionResult { spec: { inputs }, gc: { gc }, in_: γ × e, dir: LinkedInputs } =
   (toSel <$> report out) × mapWithKey (\x v -> asSel <$> get x γ <*> v) (γ' # filterKeys (_ `elem` inputs))
   where
   report = spyWhen tracing.mediatingData "Mediating outputs" prettyP
   (γ' × _) × out = (unwrap (relatedInputs gc)).bwd (γ × e)

drawCode :: String -> EditorView -> Effect Unit
drawCode s ed =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]

drawFile :: File × String -> Effect Unit
drawFile (file × src) =
   addEditorView (codeMirrorDiv $ unwrap file) >>= drawCode src

loadFig :: forall m. FigSpec -> AffError m Fig
loadFig spec@{ imports, file, datasets } = do
   s <- open file
   e <- desug s
   gconfig <- loadProgCxt imports datasets >>= initialConfig e
   gc <- graphGC gconfig e
   pure { spec, s, gc, in_: botOf gc.γα × topOf e, out: botOf gc.vα, dir: LinkedOutputs }

-- ======================
-- boilerplate
-- ======================

derive instance Eq Direction
