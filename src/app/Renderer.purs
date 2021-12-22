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
import Bindings (Var, find)
import DataType (cBarChart, cCons, cLineChart, cNil)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Expl (Expl)
import Expr (Expr)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Lattice (Slice, 𝔹, botOf, neg, expand)
import Module (File(..), open, openDatasetAs)
import Primitive (match, match_fwd)
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import Util (MayFail, type (×), type (+), (×), absurd, error, successful)
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

type LinkConfig = {
   file1 :: File,
   file2 :: File,
   dataFile :: File,
   dataVar :: Var,
   v1_sel :: Val 𝔹
}

type LinkFigSpec = {
   divId :: HTMLId,
   config :: LinkConfig
}

type Fig = {
   spec :: FigSpec,
   ex_eval :: ExampleEval
}

type LinkFig = {
   divId :: HTMLId,
   views :: Array View
}

type FigState = {
   fig :: Fig,
   views :: Array View
}

drawLinkFig :: LinkFig -> Effect Unit
drawLinkFig fig@{ divId, views } = do
   log $ "Redrawing " <> divId
   sequence_ $ 
      uncurry (drawView divId (\o' -> drawLinkFig fig)) <$> zip (range 0 (length views - 1)) views

drawFig :: Fig -> Val 𝔹 -> Effect Unit
drawFig fig o' = do
   let divId = fig.spec.divId
   log $ "Redrawing " <> divId
   let o_view × i_views = successful $ needs fig o'
   sequence_ $ 
      uncurry (drawView divId doNothing) <$> zip (range 0 (length i_views - 1)) i_views
   drawView divId (\selector -> drawFig fig (selector (o' × fig.ex_eval.o))) (length i_views) o_view

-- For an output selection, views of corresponding input selections.
needs :: Fig -> Val 𝔹 -> MayFail (View × Array View)
needs fig@{ spec, ex_eval: { ex, e, o, t } } o' = do
   let ρ0ρ' × e × α = evalBwd o' t
       ρ0' × ρ' = splitAt (length ex.ρ) ρ0ρ'
       o'' = evalFwd ρ0ρ' e α t
   views <- valViews (ρ0ρ' × (ex.ρ0 <> ex.ρ)) spec.vars 
   pure $ view "output" (o'' × o) × views

varView :: Var × Slice (Val 𝔹) -> View
varView (x × uv) = view x uv

varView' :: Var -> Slice (Env 𝔹) -> MayFail View
varView' x (ρ' × ρ) = do
   v <- find x ρ
   v' <- find x ρ'
   pure $ varView (x × (v' × v))

valViews :: Slice (Env 𝔹) -> Array Var -> MayFail (Array View)
valViews (ρ' × ρ) vars = sequence (flip varView' (ρ' × ρ) <$> vars)

type LinkResult = {
   v1 :: Val 𝔹,             -- original value of view 1
   v2 :: Slice (Val 𝔹),
   data_sel :: Slice (Val 𝔹)
}

doLink :: LinkConfig -> Aff LinkResult
doLink { file1, file2, dataFile, dataVar: x, v1_sel } = do
   let dir = File "linking/"
       name1 × name2 = (dir <> file1) × (dir <> file2)
   -- the views share an ambient environment ρ0 as well as dataset
   ρ0 × ρ <- openDatasetAs (File "example/" <> dir <> dataFile) x
   s1 <- open name1
   s2 <- open name2
   pure $ successful do
      e1 <- desugarFwd s1
      e2 <- desugarFwd s2
      t1 × v1 <- eval (ρ0 <> ρ) e1
      t2 × v2 <- eval (ρ0 <> ρ) e2
      let ρ0ρ × _ × _ = evalBwd v1_sel t1
          _ × ρ' = splitAt 1 ρ0ρ
      v <- find x ρ
      v' <- find x ρ'
      -- make ρ0 and e2 fully available; ρ0 is too big to operate on, so we use (topOf ρ0)
      -- combined with the negation of the dataset environment slice
      pure {
         v1: v1,
         v2: neg (evalFwd (neg (botOf ρ0 <> ρ')) (const true <$> e2) true t2) × v2,
         data_sel: v' × v
      }

loadFig :: FigSpec -> Aff Fig
loadFig spec@{ divId, file, vars } = do
   -- TODO: not every example should run with this dataset.
   ρ0 × ρ <- openDatasetAs (File "example/linking/renewables") "data"
   open file <#> \s' -> successful do
      { ρ: ρ1, s } <- splitDefs (ρ0 <> ρ) s'
      ex_eval <- do
         e <- desugarFwd s
         let ρ0ρ = ρ0 <> ρ <> ρ1
         t × o <- eval ρ0ρ e
         pure { ex: { ρ0, ρ: ρ <> ρ1, s }, e, t, o }
      pure { spec, ex_eval }

loadLinkFig :: LinkFigSpec -> Aff LinkFig
loadLinkFig { divId, config } = do
   link <- doLink config
   pure { divId, views: [
      view "primary view" (config.v1_sel × link.v1),
      view "linked view" link.v2,
      view "common data" link.data_sel
   ] }
