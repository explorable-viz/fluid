module App.Fig where

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

-- Want a nicer way to do this.
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

type FigSpec = {
   divId :: HTMLId,
   file :: File,
   vars :: Array Var -- variables to be considered "inputs"
}

type Fig = {
   spec :: FigSpec,
   ρ0 :: Env 𝔹,     -- ambient env (default imports)
   ρ :: Env 𝔹,      -- local env (loaded dataset, if any, plus additional let bindings at beginning of ex)
   s :: S.Expr 𝔹,   -- body of example
   e :: Expr 𝔹,     -- desugared s
   t :: Expl 𝔹,
   o :: Val 𝔹
}

type LinkFigSpec = {
   divId :: HTMLId,
   file1 :: File,
   file2 :: File,
   dataFile :: File,
   dataVar :: Var,
   v1_sel :: Val 𝔹
}

type LinkFig = {
   spec :: LinkFigSpec,
   ρ0 :: Env 𝔹,      -- ambient environment (default imports)
   ρ :: Env 𝔹,       -- local env (loaded dataset)
   s1 :: S.Expr 𝔹,
   s2 :: S.Expr 𝔹,
   e1 :: Expr 𝔹,
   e2 :: Expr 𝔹,
   t1 :: Expl 𝔹,
   t2 :: Expl 𝔹,
   v1 :: Val 𝔹,      -- TODO: align naming conventions with Fig
   v2 :: Val 𝔹,
   v0 :: Val 𝔹       -- common data named by spec.dataVar
}

type LinkResult = {
   v2' :: Val 𝔹,
   data_sel :: Slice (Val 𝔹)
}

-- TODO: these two need some consolidation.
drawLinkFig :: LinkFig -> Val 𝔹 -> Effect Unit
drawLinkFig fig@{ spec: { divId }, v1 } v1' = do
   log $ "Redrawing " <> divId
   let v1_view × views = successful $ linkFigViews fig v1'
   drawView divId (\selector -> drawLinkFig fig (selector (v1' × v1))) (length views) v1_view
   sequence_ $
      uncurry (drawView divId doNothing) <$> zip (range 0 (length views - 1)) views

drawFig :: Fig -> Val 𝔹 -> Effect Unit
drawFig fig@{ spec: { divId }, o } o' = do
   log $ "Redrawing " <> divId
   let o_view × i_views = successful $ figViews fig o'
   sequence_ $
      uncurry (drawView divId doNothing) <$> zip (range 0 (length i_views - 1)) i_views
   drawView divId (\selector -> drawFig fig (selector (o' × o))) (length i_views) o_view

varView :: Var -> Slice (Env 𝔹) -> MayFail View
varView x (ρ' × ρ) = (\v' v -> view x (v' × v)) <$> find x ρ' <*> find x ρ

valViews :: Slice (Env 𝔹) -> Array Var -> MayFail (Array View)
valViews (ρ' × ρ) vars = sequence (flip varView (ρ' × ρ) <$> vars)

-- For an output selection, views of corresponding input selections.
figViews :: Fig -> Val 𝔹 -> MayFail (View × Array View)
figViews fig@{ spec, ρ0, ρ, e, o, t } o' = do
   let ρ0ρ' × e × α = evalBwd o' t
       ρ0' × ρ' = splitAt (length ρ) ρ0ρ'
       o'' = evalFwd ρ0ρ' e α t
   views <- valViews (ρ0ρ' × (ρ0 <> ρ)) spec.vars
   pure $ view "output" (o'' × o) × views

linkFigViews :: LinkFig -> Val 𝔹 -> MayFail (View × Array View)
linkFigViews fig@{ v1, v2 } v1' = do
   link <- linkResult fig v1'
   pure $ view "primary view" (v1' × v1) ×
          [view "linked view" (link.v2' × v2), view "common data" link.data_sel]

linkResult :: LinkFig -> Val 𝔹 -> MayFail LinkResult
linkResult { spec, ρ0, ρ, e2, t1, t2, v1, v2 } v1_sel = do
   let ρ0ρ × _ × _ = evalBwd v1_sel t1
       _ × ρ' = splitAt 1 ρ0ρ
       x = spec.dataVar
   v <- find x ρ
   v' <- find x ρ'
   -- make ρ0 and e2 fully available; ρ0 is too big to operate on, so we use (topOf ρ0)
   -- combined with the negation of the dataset environment slice
   pure {
      v2': neg (evalFwd (neg (botOf ρ0 <> ρ')) (const true <$> e2) true t2),
      data_sel: v' × v
   }

doLink :: LinkFigSpec -> Aff LinkResult
doLink spec@{ file1, file2, dataFile, dataVar: x, v1_sel } = do
   fig <- loadLinkFig spec
   pure $ successful $ linkResult fig v1_sel

loadFig :: FigSpec -> Aff Fig
loadFig spec@{ divId, file, vars } = do
   -- TODO: not every example should run with this dataset.
   ρ0 × ρ <- openDatasetAs (File "example/linking/renewables") "data"
   open file <#> \s' -> successful do
      { ρ: ρ1, s } <- splitDefs (ρ0 <> ρ) s'
      e <- desugarFwd s
      let ρ0ρ = ρ0 <> ρ <> ρ1
      t × o <- eval ρ0ρ e
      pure { spec, ρ0, ρ: ρ <> ρ1, s, e, t, o }

loadLinkFig :: LinkFigSpec -> Aff LinkFig
loadLinkFig spec@{ file1, file2, dataFile, dataVar: x, v1_sel } = do
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
      v0 <- find x ρ
      pure { spec, ρ0, ρ, s1, s2, e1, e2, t1, t2, v1, v2, v0 }
