module App.Fig where

import Prelude hiding (absurd)
import Data.Array (range, zip)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Traversable (sequence, sequence_)
import Data.List (List(..), (:), singleton)
import Data.Map (lookup)
import Data.Set (difference)
import Data.Set (singleton) as S
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
import App.Util (HTMLId, OnSel, Selector, doNothing, from, record)
import Bindings (Var)
import DataType (cBarChart, cCons, cLineChart, cNil)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Expr (Expr)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Lattice (𝔹, botOf, neg)
import Module (File(..), open, openDatasetAs)
import Primitive (match_fwd)
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import Trace (Trace)
import Util (MayFail, type (×), type (+), (×), absurd, error, orElse, successful)
import Val (Env, Val(..), concat, concat_inv, dom)

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
view :: String -> Val 𝔹 -> View
view _ (Constr _ c (u1 : Nil)) | c == cBarChart =
   BarChartFig (unsafePartial $ record from u1)
view _ (Constr _ c (u1 : Nil)) | c == cLineChart =
   LineChartFig (unsafePartial $ record from u1)
view title u@(Constr _ c _) | c == cNil || c == cCons =
   EnergyTableView (EnergyTable { title, table: unsafePartial $ record energyRecord <$> from u })
view title u@(Matrix _ _) =
   MatrixFig (MatrixView { title, matrix: matrixRep $ fst (match_fwd u) } )
view _ _ = error absurd

-- An example of the form (let <defs> in expr) can be decomposed as follows.
type SplitDefs = {
   γ :: Env 𝔹,      -- local env (additional let bindings at beginning of ex)
   s :: S.Expr 𝔹    -- body of example
}

-- Decompose as above.
splitDefs :: Env 𝔹 -> S.Expr 𝔹 -> MayFail SplitDefs
splitDefs γ0 s' = do
   let defs × s = unsafePartial $ unpack s'
   γ0γ <- desugarModuleFwd (S.Module (singleton defs)) >>= eval_module γ0
   let _ × γ = concat_inv (dom γ0γ `difference` dom γ0) γ0γ
   pure { γ, s }
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
   γ0 :: Env 𝔹,     -- ambient env (default imports)
   γ :: Env 𝔹,      -- local env (loaded dataset, if any, plus additional let bindings at beginning of ex)
   s :: S.Expr 𝔹,   -- body of example
   e :: Expr 𝔹,     -- desugared s
   t :: Trace 𝔹,
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
   γ0 :: Env 𝔹,      -- ambient environment (default imports)
   γ :: Env 𝔹,       -- local env (loaded dataset)
   s1 :: S.Expr 𝔹,
   s2 :: S.Expr 𝔹,
   e1 :: Expr 𝔹,
   e2 :: Expr 𝔹,
   t1 :: Trace 𝔹,
   t2 :: Trace 𝔹,
   v1 :: Val 𝔹,
   v2 :: Val 𝔹,
   v0 :: Val 𝔹       -- common data named by spec.x
}

type LinkResult = {
   v' :: Val 𝔹,      -- will represent either v1' or v2'
   v0' :: Val 𝔹
}

drawLinkFig :: LinkFig -> Either Selector Selector -> Effect Unit
drawLinkFig fig@{ spec: { x, divId }, γ0, e1, e2, t1, t2, v1, v2 } δv = do
   log $ "Redrawing " <> divId
   let v1 × v2 × δv1 × δv2 × v0 = successful case δv of
         Left δv1 -> do
            let v1' = δv1 v1
            { v', v0' } <- linkResult x γ0 e2 t1 t2 v1'
            pure $ v1 × v' × identity × botOf × v0'
         Right δv2 -> do
            let v2' = δv2 v2
            { v', v0' } <- linkResult x γ0 e1 t2 t1 v2'
            pure $ v' × v2 × botOf × identity × v0'
   drawView divId (\selector -> drawLinkFig fig (Left $ δv1 >>> selector)) 2 $ view "linked view" v1
   drawView divId (\selector -> drawLinkFig fig (Right $ δv2 >>> selector)) 0 $ view "primary view" v2
   drawView divId doNothing 1 $ view "common data" v0

drawFig :: Fig -> Selector -> Effect Unit
drawFig fig@{ spec: { divId } } δv = do
   log $ "Redrawing " <> divId
   let v_view × views = successful $ figViews fig δv
   sequence_ $
      uncurry (drawView divId doNothing) <$> zip (range 0 (length views - 1)) views
   drawView divId (\selector -> drawFig fig (δv >>> selector)) (length views) v_view

varView :: Var -> Env 𝔹 -> MayFail View
varView x γ = view x <$> (lookup x γ # orElse absurd)

valViews :: Env 𝔹 -> Array Var -> MayFail (Array View)
valViews γ xs = sequence (flip varView γ <$> xs)

-- For an output selection, views of corresponding input selections.
figViews :: Fig -> Selector -> MayFail (View × Array View)
figViews { spec: { xs }, t, v } δv = do
   let γ0γ × e × α = evalBwd (δv v) t
       v' = evalFwd γ0γ e α t
   views <- valViews γ0γ xs
   pure $ view "output" v' × views

linkResult :: Var -> Env 𝔹 -> Expr 𝔹 -> Trace 𝔹 -> Trace 𝔹 -> Val 𝔹 -> MayFail LinkResult
linkResult x γ0 e2 t1 t2 v1 = do
   let γ0γ × _ × _ = evalBwd v1 t1
       _ × γ' = concat_inv (S.singleton x) γ0γ
   v0' <- lookup x γ' # orElse absurd
   -- make γ0 and e2 fully available; γ0 was previously too big to operate on, so we use
   -- (topOf γ0) combined with negation of the dataset environment slice
   let v2' = neg (evalFwd (neg (botOf γ0 `concat` γ')) (const true <$> e2) true t2)
   pure { v': v2', v0' }

loadFig :: FigSpec -> Aff Fig
loadFig spec@{ file } = do
   -- TODO: not every example should run with this dataset.
   γ0 × γ <- openDatasetAs (File "example/linking/renewables") "data"
   open file <#> \s' -> successful do
      { γ: γ1, s } <- splitDefs (γ0 `concat` γ) s'
      e <- desugarFwd s
      let γ0γ = γ0 `concat` γ `concat` γ1
      t × v <- eval γ0γ e
      pure { spec, γ0, γ: γ `concat` γ1, s, e, t, v }

loadLinkFig :: LinkFigSpec -> Aff LinkFig
loadLinkFig spec@{ file1, file2, dataFile, x } = do
   let dir = File "linking/"
       name1 × name2 = (dir <> file1) × (dir <> file2)
   -- the views share an ambient environment γ0 as well as dataset
   γ0 × γ <- openDatasetAs (File "example/" <> dir <> dataFile) x
   s1 × s2 <- (×) <$> open name1 <*> open name2
   pure $ successful do
      e1 × e2 <- (×) <$> desugarFwd s1 <*> desugarFwd s2
      t1 × v1 <- eval (γ0 `concat` γ) e1
      t2 × v2 <- eval (γ0 `concat` γ) e2
      v0 <- lookup x γ # orElse absurd
      pure { spec, γ0, γ, s1, s2, e1, e2, t1, t2, v1, v2, v0 }
