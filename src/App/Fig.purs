module App.Fig where

import Prelude hiding (absurd)

import App.BarChart (BarChart, barChartHandler, drawBarChart)
import App.CodeMirror (EditorView, dispatch, getContentsLength, update)
import App.LineChart (LineChart, drawLineChart, lineChartHandler)
import App.MatrixView (MatrixView(..), drawMatrix, matrixViewHandler, matrixRep)
import App.TableView (EnergyTable(..), drawTable, energyRecord, tableViewHandler)
import App.Util (HTMLId, OnSel, Selector, doNothing, from, record)
import Bindings (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (range, zip)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.List (List(..), (:), singleton)
import Data.Set (singleton) as S
import Data.Traversable (sequence, sequence_)
import Data.Tuple (fst, uncurry)
import DataType (cBarChart, cCons, cLineChart, cNil)
import Desugarable (desug)
import Dict (get)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Exception (Error)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalGraph (GraphConfig)
import Expr (Expr)
import Foreign.Object (lookup)
import Graph.GraphImpl (GraphImpl)
import Lattice (𝔹, bot, botOf, erase, neg, topOf)
import Module (File(..), Folder(..), blah, defaultImports2, loadFile, open, openDatasetAs, openDefaultImports)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Primitive (matrixRep) as P
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import SExpr (desugarModuleFwd)
import Trace (Trace)
import Util (type (×), type (+), (×), absurd, error, orElse)
import Val (class Ann, Env, ProgCxtEval(..), Val(..), append_inv, (<+>))
import Web.Event.EventTarget (eventListener)

data View
   = MatrixFig MatrixView
   | EnergyTableView EnergyTable
   | LineChartFig LineChart
   | BarChartFig BarChart

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
   MatrixFig (MatrixView { title, matrix: matrixRep $ fst (P.matrixRep.match u) })
view _ _ = error absurd

-- An example of the form (let <defs> in expr) can be decomposed as follows.
type SplitDefs a =
   { γ :: Env a -- local env (additional let bindings at beginning of ex)
   , s :: S.Expr a -- body of example
   }

-- Decompose as above.
splitDefs :: forall a m. Ann a => MonadError Error m => Env a -> S.Expr a -> m (SplitDefs a)
splitDefs γ0 s' = do
   let defs × s = unsafePartial $ unpack s'
   γ <- desugarModuleFwd (S.Module (singleton defs)) >>= flip (eval_module γ0) bot
   pure { γ, s }
   where
   unpack :: Partial => S.Expr a -> (S.VarDefs a + S.RecDefs a) × S.Expr a
   unpack (S.LetRec defs s) = Right defs × s
   unpack (S.Let defs s) = Left defs × s

type FigSpec =
   { divId :: HTMLId
   , file :: File
   , xs :: Array Var -- variables to be considered "inputs"
   }

type Fig =
   { spec :: FigSpec
   , γ0 :: Env 𝔹 -- ambient env (default imports)
   , γ :: Env 𝔹 -- local env (loaded dataset, if any, plus additional let bindings at beginning of ex)
   , s0 :: S.Expr 𝔹 -- program that was originally "split"
   , s :: S.Expr 𝔹 -- body of example
   , e :: Expr 𝔹 -- desugared s
   , t :: Trace
   , v :: Val 𝔹
   }

type LinkFigSpec =
   { divId :: HTMLId
   , file1 :: File
   , file2 :: File
   , dataFile :: File
   , x :: Var
   }

type LinkFig =
   { spec :: LinkFigSpec
   , γ :: Env 𝔹 -- prog context environment (modules + dataset)
   , s1 :: S.Expr 𝔹
   , s2 :: S.Expr 𝔹
   , e1 :: Expr 𝔹
   , e2 :: Expr 𝔹
   , t1 :: Trace
   , t2 :: Trace
   , v1 :: Val 𝔹
   , v2 :: Val 𝔹
   , v0 :: Val 𝔹 -- common data named by spec.x
   , dataFile :: String -- TODO: provide surface expression instead and prettyprint
   }

type LinkResult =
   { v' :: Val 𝔹 -- will represent either v1' or v2'
   , v0' :: Val 𝔹
   }

drawLinkFig :: LinkFig -> EditorView -> EditorView -> EditorView -> Selector Val + Selector Val -> Effect Unit
drawLinkFig fig@{ spec: { x, divId }, γ, s1, s2, e1, e2, t1, t2, v1, v2, dataFile } ed1 ed2 ed3 δv = do
   log $ "Redrawing " <> divId
   v1' × v2' × δv1 × δv2 × v0 <- case δv of
      Left δv1 -> do
         let v1' = δv1 v1
         { v', v0' } <- linkResult x γ e1 e2 t1 t2 v1'
         pure $ v1' × v' × const v1' × identity × v0'
      Right δv2 -> do
         let v2' = δv2 v2
         { v', v0' } <- linkResult x γ e2 e1 t2 t1 v2'
         pure $ v' × v2' × identity × const v2' × v0'
   drawView divId (\selector -> drawLinkFig fig ed1 ed2 ed3 (Left $ δv1 >>> selector)) 2 $ view "left view" v1'
   drawView divId (\selector -> drawLinkFig fig ed1 ed2 ed3 (Right $ δv2 >>> selector)) 0 $ view "right view" v2'
   drawView divId doNothing 1 $ view "common data" v0
   drawCode ed1 $ prettyP s1
   drawCode ed2 $ prettyP s2
   drawCode ed3 $ dataFile

drawCode :: EditorView -> String -> Effect Unit
drawCode ed s = do
   tr <- update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]
   dispatch ed tr

drawFig :: Fig -> EditorView -> Selector Val -> Effect Unit
drawFig fig@{ spec: { divId }, s0 } ed δv = do
   log $ "Redrawing " <> divId
   v_view × views <- figViews fig δv
   sequence_ $
      uncurry (drawView divId doNothing) <$> zip (range 0 (length views - 1)) views
   drawView divId (\selector -> drawFig fig ed (δv >>> selector)) (length views) v_view
   drawCode ed $ prettyP s0

varView :: forall m. MonadError Error m => Var -> Env 𝔹 -> m View
varView x γ = view x <$> (lookup x γ # orElse absurd)

valViews :: forall m. MonadError Error m => Env 𝔹 -> Array Var -> m (Array View)
valViews γ xs = sequence (flip varView γ <$> xs)

-- For an output selection, views of corresponding input selections and output after round-trip.
figViews :: forall m. MonadError Error m => Fig -> Selector Val -> m (View × Array View)
figViews { spec: { xs }, γ0, γ, e, t, v } δv = do
   let
      γ0γ × e' × α = evalBwd (erase <$> (γ0 <+> γ)) (erase e) (δv v) t
   _ × v' <- eval γ0γ e' α
   views <- valViews γ0γ xs
   pure $ view "output" v' × views

linkResult :: forall m. MonadError Error m => Var -> Env 𝔹 -> Expr 𝔹 -> Expr 𝔹 -> Trace -> Trace -> Val 𝔹 -> m LinkResult
linkResult x γ0γ e1 e2 t1 _ v1 = do
   let
      γ0γ' × _ = evalBwd (erase <$> γ0γ) (erase e1) v1 t1
      γ0' × γ' = append_inv (S.singleton x) γ0γ'
   v0' <- lookup x γ' # orElse absurd
   -- make γ0 and e2 fully available; γ0 was previously too big to operate on, so we use
   -- (topOf γ0) combined with negation of the dataset environment slice
   _ × v2' <- eval (neg ((botOf <$> γ0') <+> γ')) (topOf e2) true
   pure { v': neg v2', v0' }

loadFig :: forall m. MonadAff m => MonadError Error m => FigSpec -> m Fig
loadFig spec@{ file } = do
   { progCxt: ProgCxtEval { γ } } :: GraphConfig GraphImpl <- defaultImports2 >>= blah
   let γ0 = botOf <$> γ
   s' <- open file
   let s0 = botOf s'
   { γ: γ1, s } <- splitDefs γ0 s0
   e <- desug s
   let γ0γ = γ0 <+> γ1
   t × v <- eval γ0γ e bot
   pure { spec, γ0, γ: γ0 <+> γ1, s0, s, e, t, v }

loadLinkFig :: forall m. MonadAff m => MonadError Error m => LinkFigSpec -> m LinkFig
loadLinkFig spec@{ file1, file2, dataFile, x } = do
   let
      dir = File "linking/"
      name1 × name2 = (dir <> file1) × (dir <> file2)
   -- views share ambient environment γ as well as dataset
   { progCxt: ProgCxtEval { γ } } × xv :: GraphConfig GraphImpl × _ <-
      openDefaultImports >>= openDatasetAs (File "example/" <> dir <> dataFile) x
   s1' × s2' <- (×) <$> open name1 <*> open name2
   let
      γ0 = botOf <$> (γ <+> xv)
      s1 = botOf s1'
      s2 = botOf s2'
   dataFile' <- loadFile (Folder "fluid/example/linking") dataFile -- use surface expression instead
   e1 × e2 <- (×) <$> desug s1 <*> desug s2
   t1 × v1 <- eval γ0 e1 bot
   t2 × v2 <- eval γ0 e2 bot
   let v0 = get x γ0
   pure { spec, γ: γ0, s1, s2, e1, e2, t1, t2, v1, v2, v0, dataFile: dataFile' }
