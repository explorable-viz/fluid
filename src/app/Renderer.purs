module App.Renderer where

import Prelude hiding (absurd)
import Data.Array (range, zip)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Traversable (sequence, sequence_)
import Data.List (List(..), (:), singleton)
import Data.Tuple (fst, uncurry)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Web.Event.EventTarget (eventListener)
import App.BarChart (BarChart, barChartHandler, drawBarChart)
import App.LineChart (LineChart, drawLineChart, lineChartHandler)
import App.MatrixView (MatrixView(..), drawMatrix, matrixViewHandler, matrixRep)
import App.TableView (EnergyTable(..), drawTable, energyRecord, tableViewHandler)
import App.Util (HTMLId, OnSel, doNothing, from, record)
import Bindings (Bind, Var, find, update)
import DataType (cBarChart, cCons, cLineChart, cNil)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Expl (Expl)
import Expr (Expr)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Lattice (Slice, 𝔹, botOf, expand)
import Module (File(..), open, openDatasetAs)
import Primitive (match, match_fwd)
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import Test.Util (LinkConfig, doLink)
import Util (Endo, MayFail, type (×), type (+), (×), absurd, error, successful)
import Util.SnocList (splitAt)
import Val (Env, Val)
import Val (Val(..)) as V

data View =
   MatrixFig MatrixView |
   EnergyTableView EnergyTable |
   LineChartFig LineChart |
   BarChartFig BarChart

drawView :: HTMLId -> OnSel -> Int -> View -> Effect Unit
drawView divId onSel n (MatrixFig vw) = drawMatrix divId n vw =<< eventListener (onSel <<< matrixViewHandler)
drawView divId onSel n (EnergyTableView vw) = drawTable divId n vw =<< eventListener (onSel <<< tableViewHandler)
drawView divId onSel n (LineChartFig vw) = drawLineChart divId n vw =<< eventListener (onSel <<< lineChartHandler)
drawView divId onSel n (BarChartFig vw) = drawBarChart divId n vw =<< eventListener (onSel <<< barChartHandler)

-- Convert sliced value to appropriate View, discarding top-level annotations for now.
-- 'from' is partial; encapsulate that here.
view :: String -> Slice (Val 𝔹) -> View
view _ (u × V.Constr _ c (v1 : Nil)) | c == cBarChart =
   case expand u (V.Constr false cBarChart (V.Hole false : Nil)) of
      V.Constr _ _ (u1 : Nil) -> BarChartFig (unsafePartial $ record from (u1 × v1))
      _ -> error absurd
view _ (u × V.Constr _ c (v1 : Nil)) | c == cLineChart =
   case expand u (V.Constr false cLineChart (V.Hole false : Nil)) of
      V.Constr _ _ (u1 : Nil) -> LineChartFig (unsafePartial $ record from (u1 × v1))
      _ -> error absurd
view title (u × v@(V.Constr _ c _)) | c == cNil || c == cCons =
   EnergyTableView (EnergyTable { title, table: unsafePartial $ record energyRecord <$> from (u × v) })
view title (u × v@(V.Matrix _ _)) =
   let vss2 = fst (match_fwd (u × v)) × fst (match v) in
   MatrixFig (MatrixView { title, matrix: matrixRep vss2 } )
view _ _ = error absurd

-- An example of the form (let <defs> in expr) can be decomposed as follows.
type SplitDefs = {
   ρ :: Env 𝔹,      -- local env (additional let bindings at beginning of ex)
   s :: S.Expr 𝔹    -- body of example
}

-- Decompose as above.
splitDefs :: Env 𝔹 -> S.Expr 𝔹 -> MayFail SplitDefs
splitDefs ρ0 s' = do
   let defs × s = unsafePartial $ unpack s'
   ρ0ρ <- desugarModuleFwd (S.Module (singleton defs)) >>= eval_module ρ0
   let _ × ρ = splitAt (length ρ0ρ - length ρ0) ρ0ρ
   pure { ρ, s }
   where unpack :: Partial => S.Expr 𝔹 -> (S.VarDefs 𝔹 + S.RecDefs 𝔹) × S.Expr 𝔹
         unpack (S.LetRec defs s)   = Right defs × s
         unpack (S.Let defs s)      = Left defs × s

type Example = {
   ρ0 :: Env 𝔹,     -- ambient env (default imports)
   ρ :: Env 𝔹,      -- local env (loaded dataset, if any, plus additional let bindings at beginning of ex)
   s :: S.Expr 𝔹    -- body of example
}

type ExampleEval = {
   ex :: Example,
   e :: Expr 𝔹,
   t :: Expl 𝔹,
   o :: Val 𝔹
}

type FigSpec = {
   divId :: HTMLId,
   file :: File,
   vars :: Array Var -- variables to be considered "inputs"
}

type LinkingFigSpec = {
   divId :: HTMLId,
   config :: LinkConfig
}

type Fig' = {
   spec :: FigSpec,
   ex_eval :: ExampleEval
}

type Fig r = {
   divId :: HTMLId,
   views :: Array View
   | r
}

type FigState = {
   fig :: Fig',
   views :: Array View
}

drawFig :: forall r . Fig r -> Effect Unit
drawFig fig@{ divId, views } = do
   log $ "Drawing " <> divId
   sequence_ $ 
      uncurry (drawView divId (\o' -> drawFig fig)) <$> zip (range 0 (length views - 1)) views

drawFig' :: Fig' -> Val 𝔹 -> Effect Unit
drawFig' fig o' = do
   let divId = fig.spec.divId
   log $ "Redrawing " <> divId
   let o_view × i_views = successful $ needs fig o'
   sequence_ $ 
      uncurry (drawView divId doNothing) <$> zip (range 0 (length i_views - 1)) i_views
   drawView divId (\selector -> drawFig' fig (selector (o' × fig.ex_eval.o))) (length i_views) o_view

-- For an output selection, views of corresponding input selections.
needs :: Fig' -> Val 𝔹 -> MayFail (View × Array View)
needs fig@{ spec, ex_eval: { ex, e, o, t } } o' = do
   let ρ0ρ' × e × α = evalBwd o' t
       ρ0' × ρ' = splitAt (length ex.ρ) ρ0ρ'
       o'' = evalFwd ρ0ρ' e α t
   views <- valViews (ρ0ρ' × (ex.ρ0 <> ex.ρ)) spec.vars 
   pure $ view "output" (o'' × o) × views

evalExample :: Example -> MayFail ExampleEval
evalExample ex@{ ρ0, ρ, s } = do
   e <- desugarFwd s
   let ρ0ρ = ρ0 <> ρ
   t × o <- eval ρ0ρ e
   pure { ex, e, t, o }

varView :: Var × Slice (Val 𝔹) -> View
varView (x × uv) = view x uv

varView' :: Var -> Slice (Env 𝔹) -> MayFail View
varView' x (ρ' × ρ) = do
   v <- find x ρ
   v' <- find x ρ'
   pure $ varView (x × (v' × v))

valViews :: Slice (Env 𝔹) -> Array Var -> MayFail (Array View)
valViews (ρ' × ρ) vars = sequence (flip varView' (ρ' × ρ) <$> vars)

selectOnly :: Bind (Val 𝔹) -> Endo (Env 𝔹)
selectOnly xv ρ = update (botOf ρ) xv

loadFig :: FigSpec -> Aff Fig'
loadFig spec@{ divId, file, vars } = do
   -- TODO: not every example should run with this dataset.
   ρ0 × ρ <- openDatasetAs (File "example/linking/renewables") "data"
   open file <#> \e -> successful do
      { ρ: ρ1, s } <- splitDefs (ρ0 <> ρ) e
      ex_eval <- evalExample { ρ0, ρ: ρ <> ρ1, s }
      pure { spec, ex_eval }

loadLinkingFig :: LinkingFigSpec -> Aff (Fig ())
loadLinkingFig { divId, config } = do
   link <- doLink config
   pure { divId, views: [
      view "primary view" (config.v1_sel × link.v1),
      view "linked view" link.v2,
      view "common data" link.data_sel
   ] }
