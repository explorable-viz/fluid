module App.Fig where

import Prelude hiding (absurd, compare)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (ReactState, SelState, Selector, 𝕊, asℝ, selState, toℝ)
import App.Util.Selector (envVal)
import App.View (View, drawView, view)
import App.View.Util (HTMLId)
import Bind (Bind, Var, (↦))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.Set as Set
import Data.Traversable (sequence_)
import Data.Tuple (curry)
import Desugarable (desug)
import Dict (Dict)
import Effect (Effect)
import EvalGraph (graphEval, graphGC, withOp)
import GaloisConnection ((***)) as GC
import GaloisConnection (GaloisConnection(..), dual, meet)
import Lattice (class BoundedMeetSemilattice, Raw, 𝔹, botOf, erase, neg, topOf)
import Module (File, initialConfig, loadProgCxt, open)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import SExpr (Expr) as S
import Test.Util.Debug (tracing)
import Util (type (×), AffError, Endo, spyWhen, (×))
import Util.Map (get, insert, lookup, mapWithKey)
import Val (Env(..), EnvExpr(..), Val, unrestrictGC)

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
   , in_views :: Dict (Maybe View) -- strengthen this
   , out_view :: Maybe View
   , γ0 :: Env (𝔹)
   , v0 :: Val (𝔹)
   }

str
   :: { output :: String -- pseudo-variable to use as name of output view
      , input :: String -- prefix for input views
      }
str =
   { output: "output"
   , input: "input"
   }

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

setInputViewState :: Bind (Endo View) -> Endo Fig
setInputViewState (x ↦ δvw) fig = fig
   { in_views = insert x (lookup x fig.in_views # join <#> δvw) fig.in_views
   }

drawFig :: HTMLId -> Fig -> Effect Unit
drawFig divId fig = do
   drawView divId str.output (drawFig divId <<< flip selectOutput fig) out_view
   sequence_ $ flip mapWithKey in_views \x ->
      drawView (divId <> "-" <> str.input) x (drawFig divId <<< flip (curry selectInput x) fig)
   where
   out_view × in_views =
      selectionResult fig # unsafePartial
         (flip (view str.output) fig.out_view *** \(Env γ) -> mapWithKey view γ <*> fig.in_views)

selectionResult :: Fig -> Val (ReactState 𝕊) × Env (ReactState 𝕊)
selectionResult fig@{ γ0, v, dir: LinkedOutputs } =
   (asℝ <$> v <*> (selState <$> v1 <*> v2)) × (toℝ <$> γ0 <*> report (selState <$> γ1 <*> γ2))
   where
   report = spyWhen tracing.mediatingData "Mediating inputs" prettyP
   GC gc = (fig.gc_dual `GC.(***)` identity) >>> meet >>> fig.gc
   v1 × γ1 = gc.bwd (v <#> unwrap >>> _.persistent)
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

-- ======================
-- boilerplate
-- ======================

derive instance Eq Direction
