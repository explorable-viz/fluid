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
import App.Util (HTMLId, from, record)
import Bindings (Bind, Var, find, update)
import DataType (cBarChart, cCons, cLineChart, cNil)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Expl (Expl)
import Expr (Expr)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Lattice (𝔹, botOf, expand)
import Module (File(..), open, openDatasetAs)
import Primitive (Slice, match, match_fwd)
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import Test.Util (LinkConfig, doLink, selectCell)
import Util (Endo, MayFail, type (×), type (+), (×), absurd, error, successful)
import Util.SnocList (splitAt)
import Val (Env, Val)
import Val (Val(..)) as V

data View =
   MatrixFig MatrixView |
   EnergyTableView EnergyTable |
   LineChartFig LineChart |
   BarChartFig BarChart

drawView :: HTMLId -> (Unit -> Effect Unit) -> Int -> View -> Effect Unit
drawView divId redraw n (MatrixFig vw) = drawMatrix divId n vw =<< eventListener (matrixViewHandler redraw)
drawView divId redraw n (EnergyTableView vw) = drawTable divId n vw =<< eventListener (tableViewHandler redraw)
drawView divId redraw n (LineChartFig vw) = drawLineChart divId n vw =<< eventListener (lineChartHandler redraw)
drawView divId redraw n (BarChartFig vw) = drawBarChart divId n vw =<< eventListener (barChartHandler redraw)

type Fig = {
   divId :: HTMLId,
   views :: Array View
}

drawFig :: Fig -> Effect Unit
drawFig fig@{ divId, views } = do
   log $ "Drawing " <> divId
   sequence_ $ 
      uncurry (drawView divId (const $ drawFig fig)) <$> zip (range 0 (length views - 1)) views

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

type Example = {
   ρ0 :: Env 𝔹,     -- ambient env (default imports)
   ρ :: Env 𝔹,      -- local env (loaded dataset, if any, plus additional let bindings at beginning of ex)
   s :: S.Expr 𝔹    -- body of example
}

-- Example assumed to be of the form (let <defs> in expr).
type LetExample = {
   ρ :: Env 𝔹,      -- local env (additional let bindings at beginning of ex)
   s :: S.Expr 𝔹    -- body of example
}

-- Interpret a program as a "let" example in the sense above. TODO: generalise to sequence of let/let recs.
splitDefs :: Env 𝔹 -> S.Expr 𝔹 -> MayFail LetExample
splitDefs ρ0 s' = do
   let defs × s = unsafePartial $ unpack s'
   ρ0ρ <- desugarModuleFwd (S.Module (singleton defs)) >>= eval_module ρ0
   let _ × ρ = splitAt (length ρ0ρ - length ρ0) ρ0ρ
   pure { ρ, s }
   where unpack :: Partial => S.Expr 𝔹 -> (S.VarDefs 𝔹 + S.RecDefs 𝔹) × S.Expr 𝔹
         unpack (S.LetRec defs s)   = Right defs × s
         unpack (S.Let defs s)      = Left defs × s

varView :: Var × Slice (Val 𝔹) -> View
varView (x × uv) = view x uv

type ExampleEval = {
   e :: Expr 𝔹,
   t :: Expl 𝔹,
   o :: Val 𝔹
}

evalExample :: Example -> MayFail ExampleEval
evalExample { ρ0, ρ, s } = do
   e <- desugarFwd s
   let ρ0ρ = ρ0 <> ρ
   t × o <- eval ρ0ρ e
   pure { e, t, o }

varView' :: Var -> Slice (Env 𝔹) -> MayFail View
varView' x (ρ' × ρ) = do
   v <- find x ρ
   v' <- find x ρ'
   pure $ varView (x × (v' × v))

valViews :: Val 𝔹 -> NeedsSpec -> Slice (Env 𝔹) -> MayFail (Array View)
valViews o { vars, o' } (ρ' × ρ) = do
   views <- sequence (flip varView' (ρ' × ρ) <$> vars)
   pure $ views <> [ view "output" (o' × o) ]

type NeedsSpec = {
   vars :: Array Var,  -- variables we want views for
   o' :: Val 𝔹         -- selection on output
}

needs :: NeedsSpec -> Example -> MayFail (Array View)
needs spec { ρ0, ρ, s } = do
   { e, o, t } <- evalExample { ρ0, ρ, s }
   let ρ0ρ' × e × α = evalBwd spec.o' t
       ρ0' × ρ' = splitAt (length ρ) ρ0ρ'
       o'' = evalFwd ρ0ρ' e α t
   views <- valViews o spec (ρ0ρ' × (ρ0 <> ρ))
   pure $ views <> [ view "output" (o'' × o) ]

selectOnly :: Bind (Val 𝔹) -> Endo (Env 𝔹)
selectOnly xv ρ = update (botOf ρ) xv

type FigSpec = {
   divId :: HTMLId,
   file :: File,
   vars :: Array Var  -- variables we consider to be "inputs"
}

type LinkingFigSpec = {
   divId :: HTMLId,
   config :: LinkConfig
}

-- TODO: not every example should run with this dataset.
loadFig :: FigSpec -> Aff Fig
loadFig { divId, file, vars } = do
   ρ0 × ρ <- openDatasetAs (File "example/linking/renewables") "data"
   { ρ: ρ1, s } <- (successful <<< splitDefs (ρ0 <> ρ)) <$> open file
   let views = successful (needs { vars, o': selectCell 2 2 5 5 } { ρ0, ρ: ρ <> ρ1, s })
   pure { divId, views }

loadLinkingFig :: LinkingFigSpec -> Aff Fig
loadLinkingFig { divId, config } = do
   link <- doLink config
   pure { divId, views: [
      view "primary view" (config.v1_sel × link.v1),
      view "linked view" link.v2,
      view "common data" link.data_sel
   ] }
