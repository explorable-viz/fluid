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
import Val (Env, Val(..))

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
view _ (u × Constr _ c (v1 : Nil)) | c == cBarChart =
   case expand u (Constr false cBarChart (Hole false : Nil)) of
      Constr _ _ (u1 : Nil) -> BarChartFig (unsafePartial $ record from (u1 × v1))
      _ -> error absurd
view _ (u × Constr _ c (v1 : Nil)) | c == cLineChart =
   case expand u (Constr false cLineChart (Hole false : Nil)) of
      Constr _ _ (u1 : Nil) -> LineChartFig (unsafePartial $ record from (u1 × v1))
      _ -> error absurd
view title (u × v@(Constr _ c _)) | c == cNil || c == cCons =
   EnergyTableView (EnergyTable { title, table: unsafePartial $ record energyRecord <$> from (u × v) })
view title (u × v@(Matrix _ _)) =
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
   xs :: Array Var -- variables to be considered "inputs"
}

type Fig = {
   spec :: FigSpec,
   ρ0 :: Env 𝔹,     -- ambient env (default imports)
   ρ :: Env 𝔹,      -- local env (loaded dataset, if any, plus additional let bindings at beginning of ex)
   s :: S.Expr 𝔹,   -- body of example
   e :: Expr 𝔹,     -- desugared s
   t :: Expl 𝔹,
   v :: Val 𝔹
}

type LinkFigSpec = {
   divId :: HTMLId,
   file1 :: File,
   file2 :: File,
   dataFile :: File,
   x :: Var
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
   v1 :: Val 𝔹,
   v2 :: Val 𝔹,
   v0 :: Val 𝔹       -- common data named by spec.x
}

type LinkResult = {
   v' :: Val 𝔹,      -- will represent either v1' or v2'
   v0' :: Val 𝔹
}

-- TODO: consolidate.
drawLinkFig :: LinkFig -> Either (Val 𝔹) (Val 𝔹) -> Effect Unit
drawLinkFig fig@{ spec: { x, divId }, ρ0, e2, t1, t2, v1, v2, v0 } (Left v1') = do
   log $ "Redrawing " <> divId
   let v1_view × v2_view × v0_view = successful do
         { v': v2', v0' } <- linkResult x ρ0 e2 t1 t2 v1'
         pure $ view "primary view" (v1' × v1) × view "linked view" (v2' × v2) × view "common data" (v0' × v0)
   drawView divId (\selector -> drawLinkFig fig (Left $ selector (v1' × v1))) 2 v1_view
   drawView divId (\selector -> drawLinkFig fig (Right $ selector (Hole false × v2))) 0 v2_view
   drawView divId doNothing 1 v0_view
drawLinkFig fig@{ spec: { x, divId }, ρ0, e1, t1, t2, v1, v2, v0 } (Right v2') = do
   log $ "Redrawing " <> divId
   let v1_view × v2_view × v0_view = successful do
         { v': v1', v0' } <- linkResult x ρ0 e1 t2 t1 v2'
         pure $ view "linked view" (v1' × v1) × view "primary view" (v2' × v2) × view "common data" (v0' × v0)
   drawView divId (\selector -> drawLinkFig fig (Left $ selector (Hole false × v1))) 2 v1_view
   drawView divId (\selector -> drawLinkFig fig (Right $ selector (v2' × v2))) 0 v2_view
   drawView divId doNothing 1 v0_view

drawFig :: Fig -> Val 𝔹 -> Effect Unit
drawFig fig@{ spec: { divId }, v } v' = do
   log $ "Redrawing " <> divId
   let v_view × views = successful $ figViews fig v'
   sequence_ $
      uncurry (drawView divId doNothing) <$> zip (range 0 (length views - 1)) views
   drawView divId (\selector -> drawFig fig (selector (v' × v))) (length views) v_view

varView :: Var -> Slice (Env 𝔹) -> MayFail View
varView x (ρ' × ρ) = (\v' v -> view x (v' × v)) <$> find x ρ' <*> find x ρ

valViews :: Slice (Env 𝔹) -> Array Var -> MayFail (Array View)
valViews (ρ' × ρ) xs = sequence (flip varView (ρ' × ρ) <$> xs)

-- For an output selection, views of corresponding input selections.
figViews :: Fig -> Val 𝔹 -> MayFail (View × Array View)
figViews { spec: { xs }, ρ0, ρ, e, t, v } v' = do
   let ρ0ρ' × e × α = evalBwd v' t
       ρ0' × ρ' = splitAt (length ρ) ρ0ρ'
       v'' = evalFwd ρ0ρ' e α t
   views <- valViews (ρ0ρ' × (ρ0 <> ρ)) xs
   pure $ view "output" (v'' × v) × views

linkResult :: Var -> Env 𝔹 -> Expr 𝔹 -> Expl 𝔹 -> Expl 𝔹 -> Val 𝔹 -> MayFail LinkResult
linkResult x ρ0 e2 t1 t2 v1' = do
   let ρ0ρ × _ × _ = evalBwd v1' t1
       _ × ρ' = splitAt 1 ρ0ρ
   v0' <- find x ρ'
   -- make ρ0 and e2 fully available; ρ0 is too big to operate on, so we use (topOf ρ0)
   -- combined with the negation of the dataset environment slice
   let v2' = neg (evalFwd (neg (botOf ρ0 <> ρ')) (const true <$> e2) true t2)
   pure { v': v2', v0' }

loadFig :: FigSpec -> Aff Fig
loadFig spec@{ file } = do
   -- TODO: not every example should run with this dataset.
   ρ0 × ρ <- openDatasetAs (File "example/linking/renewables") "data"
   open file <#> \s' -> successful do
      { ρ: ρ1, s } <- splitDefs (ρ0 <> ρ) s'
      e <- desugarFwd s
      let ρ0ρ = ρ0 <> ρ <> ρ1
      t × v <- eval ρ0ρ e
      pure { spec, ρ0, ρ: ρ <> ρ1, s, e, t, v }

loadLinkFig :: LinkFigSpec -> Aff LinkFig
loadLinkFig spec@{ file1, file2, dataFile, x } = do
   let dir = File "linking/"
       name1 × name2 = (dir <> file1) × (dir <> file2)
   -- the views share an ambient environment ρ0 as well as dataset
   ρ0 × ρ <- openDatasetAs (File "example/" <> dir <> dataFile) x
   s1 × s2 <- (×) <$> open name1 <*> open name2
   pure $ successful do
      e1 × e2 <- (×) <$> desugarFwd s1 <*> desugarFwd s2
      t1 × v1 <- eval (ρ0 <> ρ) e1
      t2 × v2 <- eval (ρ0 <> ρ) e2
      v0 <- find x ρ
      pure { spec, ρ0, ρ, s1, s2, e1, e2, t1, t2, v1, v2, v0 }
