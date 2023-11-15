module App.Fig where

import Prelude hiding (absurd)

import App.BarChart (BarChart, barChartHandler, drawBarChart)
import App.BubbleChart (BubbleChart, bubbleChartHandler, drawBubbleChart)
import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.LineChart (LineChart, drawLineChart, lineChartHandler)
import App.MatrixView (MatrixView(..), drawMatrix, matrixViewHandler, matrixRep)
import App.TableView (TableView(..), drawTable, tableViewHandler)
import App.Util (HTMLId, OnSel, doNothing, from, record)
import App.Util.Select (envVal)
import Bindings (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (range, zip)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.List (List(..), (:), singleton)
import Data.Newtype (unwrap)
import Data.Set (singleton) as S
import Data.Traversable (sequence, sequence_)
import Data.Tuple (fst, snd, uncurry)
import DataType (cBarChart, cBubbleChart, cCons, cLineChart, cNil)
import Desugarable (desug)
import Dict (get)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Effect.Exception (Error)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import Expr (Expr)
import Foreign.Object (lookup)
import Lattice (𝔹, bot, botOf, erase, neg, topOf)
import Module (File(..), Folder(..), initialConfig, datasetAs, defaultImports, loadFile, open)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Primitive (matrixRep) as P
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import SExpr (desugarModuleFwd)
import Test.Util (AffError, Selector)
import Trace (Trace)
import Util (type (+), type (×), (×), Endo, absurd, error, orElse)
import Val (class Ann, Env, Val(..), append_inv, (<+>))
import Web.Event.EventTarget (eventListener)

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

data View
   = MatrixFig MatrixView
   | TableFig TableView
   | LineChartFig LineChart
   | BarChartFig BarChart
   | BubbleChartFig BubbleChart

drawView :: HTMLId -> OnSel -> Int -> View -> Effect Unit
drawView divId onSel n (MatrixFig vw) = drawMatrix divId n vw =<< eventListener (onSel <<< matrixViewHandler)
drawView divId onSel n (TableFig vw) = drawTable divId n vw =<< eventListener (onSel <<< tableViewHandler)
drawView divId onSel n (LineChartFig vw) = drawLineChart divId n vw =<< eventListener (onSel <<< lineChartHandler)
drawView divId onSel n (BarChartFig vw) = drawBarChart divId n vw =<< eventListener (onSel <<< barChartHandler)
drawView divId onSel n (BubbleChartFig vw) = drawBubbleChart divId n vw =<< eventListener (onSel <<< bubbleChartHandler)

-- Convert sliced value to appropriate View, discarding top-level annotations for now.
-- 'from' is partial; encapsulate that here.
view :: String -> Val 𝔹 -> View
view _ (Constr _ c (u1 : Nil)) | c == cBarChart =
   BarChartFig (unsafePartial $ record from u1)
view _ (Constr _ c (u1 : Nil)) | c == cLineChart =
   LineChartFig (unsafePartial $ record from u1)
view _ (Constr _ c (u1 : Nil)) | c == cBubbleChart =
   BubbleChartFig (unsafePartial $ record from u1)
view title u@(Constr _ c _) | c == cNil || c == cCons =
   TableFig (TableView { title, table: unsafePartial $ record identity <$> from u })
view title u@(Matrix _ _) =
   MatrixFig (MatrixView { title, matrix: matrixRep $ fst (P.matrixRep.unpack u) })
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
   , γ0 :: Env 𝔹 -- ambient env
   , γ :: Env 𝔹 -- loaded dataset, if any, plus additional let bindings at beginning of ex
   , s0 :: S.Expr 𝔹 -- program that was originally "split"
   , s :: S.Expr 𝔹 -- body of example
   , e :: Expr 𝔹 -- desugared s
   , t :: Trace
   , v :: Val 𝔹
   }

type LinkedOutputsFigSpec =
   { divId :: HTMLId
   , file1 :: File
   , file2 :: File
   , dataFile :: File
   , x :: Var
   }

type LinkedInputsFigSpec =
   { divId :: HTMLId
   , file :: File
   , x1 :: Var -- variables to be considered "inputs"
   , x2 :: Var
   }

type LinkedOutputsFig =
   { spec :: LinkedOutputsFigSpec
   , γ :: Env 𝔹
   , s1 :: S.Expr 𝔹
   , s2 :: S.Expr 𝔹
   , e1 :: Expr 𝔹
   , e2 :: Expr 𝔹
   , t1 :: Trace
   , t2 :: Trace
   , v1 :: Val 𝔹
   , v2 :: Val 𝔹
   , v0 :: Val 𝔹 -- common data named by spec.x
   , dataFileStr :: String -- TODO: provide surface expression instead and prettyprint
   }

type LinkedInputsFig =
   { spec :: LinkedInputsFigSpec
   , γ0 :: Env 𝔹 -- ambient env
   , γ :: Env 𝔹 -- additional let bindings at beginning of ex; must include vars defined in spec
   , s0 :: S.Expr 𝔹 -- program that was originally "split"
   , s :: S.Expr 𝔹 -- body of example
   , e :: Expr 𝔹
   , t :: Trace
   , v0 :: Val 𝔹 -- common output
   }

type LinkedOutputsResult =
   { v :: Val 𝔹 -- selection on primary output
   , v' :: Val 𝔹 -- resulting selection on other output
   , v0' :: Val 𝔹 -- selection that arose on shared input
   }

type LinkedInputsResult =
   { v :: Val 𝔹 -- selection on primary input
   , v' :: Val 𝔹 -- resulting selection on other input
   , v0 :: Val 𝔹 -- selection that arose on shared output
   }

withShowError :: forall a. (a -> Effect Unit) -> Error + a → Effect Unit
withShowError _ (Left err) = log $ show err
withShowError f (Right x) = f x

runAffs_ :: forall a. (a -> Effect Unit) -> Array (Aff a) -> Effect Unit
runAffs_ f as = flip runAff_ (sequence as) $ withShowError ((_ <#> f) >>> sequence_)

split :: Selector Val + Selector Val -> Selector Val × Selector Val
split (Left δv) = δv × identity
split (Right δv) = identity × δv

drawLinkedOutputsFig :: LinkedOutputsFig -> Selector Val + Selector Val -> Effect Unit
drawLinkedOutputsFig fig@{ spec: { divId } } δv = do
   log $ "Redrawing " <> divId
   v1' × v2' × v0 <- linkedOutputsResult fig δv
   let δv1 × δv2 = split δv
   drawView divId (\δv' -> drawLinkedOutputsFig fig (Left $ δv1 >>> δv')) 2 $ view "left view" v1'
   drawView divId (\δv' -> drawLinkedOutputsFig fig (Right $ δv2 >>> δv')) 0 $ view "right view" v2'
   drawView divId doNothing 1 $ view "common data" v0

drawLinkedOutputsFig' :: LinkedOutputsFig -> Effect Unit
drawLinkedOutputsFig' fig = do
   drawLinkedOutputsFig fig (Left botOf)
   ed1 <- addEditorView $ codeMirrorDiv $ unwrap (fig.spec.file1)
   ed2 <- addEditorView $ codeMirrorDiv $ unwrap (fig.spec.file2)
   ed3 <- addEditorView $ codeMirrorDiv $ unwrap (fig.spec.dataFile)
   drawCode ed1 $ prettyP fig.s1
   drawCode ed2 $ prettyP fig.s2
   drawCode ed3 $ fig.dataFileStr

drawLinkedInputsFig :: LinkedInputsFig -> Selector Val + Selector Val -> Effect Unit
drawLinkedInputsFig fig@{ spec: { divId, x1, x2 } } δv = do
   log $ "Redrawing " <> divId
   v1' × v2' × v0 <- linkedInputsResult fig δv
   let δv1 × δv2 = split δv
   drawView divId doNothing 0 $ view "common output" v0
   drawView divId (\selector -> drawLinkedInputsFig fig (Left $ δv1 >>> selector)) 2 $ view x1 v1'
   drawView divId (\selector -> drawLinkedInputsFig fig (Right $ δv2 >>> selector)) 1 $ view x2 v2'

drawLinkedInputsFig' :: LinkedInputsFig -> Effect Unit
drawLinkedInputsFig' = flip drawLinkedInputsFig (Left topOf)

drawFig :: Fig -> EditorView -> Selector Val -> Effect Unit
drawFig fig@{ spec: { divId }, s0 } ed δv = do
   log $ "Redrawing " <> divId
   v_view × views <- figViews fig δv
   sequence_ $
      uncurry (drawView divId doNothing) <$> zip (range 0 (length views - 1)) views
   drawView divId (\selector -> drawFig fig ed (δv >>> selector)) (length views) v_view
   drawCode ed $ prettyP s0

drawFig' :: Fig -> Effect Unit
drawFig' fig =
   addEditorView (codeMirrorDiv fig.spec.divId) >>= flip (drawFig fig) botOf

drawCode :: EditorView -> String -> Effect Unit
drawCode ed s =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]

drawFiles :: Array (Folder × File) -> Effect Unit
drawFiles files =
   let qs = (files <#> \(folder × file) -> (file × _) <$> loadFile folder file) in
   flip runAffs_ qs \(file × src) ->
     addEditorView (codeMirrorDiv $ unwrap file) >>= flip drawCode src

varView :: forall m. MonadError Error m => Var -> Env 𝔹 -> m View
varView x γ = view x <$> (lookup x γ # orElse absurd)

-- For an output selection, views of corresponding input selections and output after round-trip.
figViews :: forall m. MonadError Error m => Fig -> Selector Val -> m (View × Array View)
figViews { spec: { xs }, γ0, γ, e, t, v } δv = do
   let
      γ0γ × e' × α = evalBwd (erase <$> (γ0 <+> γ)) (erase e) (δv v) t
   _ × v' <- eval γ0γ e' α
   views <- sequence (flip varView γ0γ <$> xs)
   pure $ view "output" v' × views

linkedOutputsResult :: forall m. MonadError Error m => LinkedOutputsFig -> Selector Val + Selector Val -> m (Val 𝔹 × Val 𝔹 × Val 𝔹)
linkedOutputsResult { spec: { x }, γ, e1, e2, t1, t2, v1, v2 } =
   case _ of
      Left δv1 -> do
         { v, v', v0' } <- result e1 e2 t1 (δv1 v1)
         pure $ v × v' × v0'
      Right δv2 -> do
         { v, v', v0' } <- result e2 e1 t2 (δv2 v2)
         pure $ v' × v × v0'
   where
   result :: Expr 𝔹 -> Expr 𝔹 -> Trace -> Val 𝔹 -> m LinkedOutputsResult
   result e e' t v = do
      let
         γ0γ' × _ = evalBwd (erase <$> γ) (erase e) v t
         γ0' × γ' = append_inv (S.singleton x) γ0γ'
      v0' <- lookup x γ' # orElse absurd
      -- make γ0 and e2 fully available
      v' <- eval (neg ((botOf <$> γ0') <+> γ')) (topOf e') true <#> snd >>> neg
      pure { v, v', v0' }

linkedInputsResult :: forall m. MonadError Error m => LinkedInputsFig -> Selector Val + Selector Val -> m (Val 𝔹 × Val 𝔹 × Val 𝔹)
linkedInputsResult { spec: { x1, x2 }, γ, e, t } =
   case _ of
      Left δv1 -> do
         { v, v', v0 } <- result x1 x2 δv1
         pure $ v × v' × v0
      Right δv2 -> do
         { v, v', v0 } <- result x2 x1 δv2
         pure $ v' × v × v0
   where
   result :: Var -> Var -> Selector Val -> m LinkedInputsResult
   result x x' δv = do
      let γ' = envVal x δv γ
      v0 <- eval (neg γ') (topOf e) true <#> snd >>> neg
      let γ'' × _ = evalBwd (erase <$> γ) (erase e) v0 t
      v <- lookup x γ' # orElse absurd
      v' <- lookup x' γ'' # orElse absurd
      pure { v, v', v0 }

loadFig :: forall m. FigSpec -> AffError m Fig
loadFig spec@{ file } = do
   { γ: γ' } <- defaultImports >>= initialConfig
   let γ0 = botOf <$> γ'
   s' <- open file
   let s0 = botOf s'
   { γ: γ1, s } <- splitDefs γ0 s0
   e <- desug s
   let γ = γ0 <+> γ1
   t × v <- eval γ e bot
   pure { spec, γ0, γ, s0, s, e, t, v }

loadLinkedInputsFig :: forall m. LinkedInputsFigSpec -> AffError m LinkedInputsFig
loadLinkedInputsFig spec@{ file } = do
   { γ: γ' } <- defaultImports >>= initialConfig
   let γ0 = botOf <$> γ'
   s' <- open $ File "linked-inputs/" <> file
   let s0 = botOf s'
   { γ: γ1, s } <- splitDefs γ0 s0
   e <- desug s
   let γ = γ0 <+> γ1
   t × v <- eval γ e bot
   pure { spec, γ0, γ, s0, s, e, t, v0: v }

loadLinkedOutputsFig :: forall m. LinkedOutputsFigSpec -> AffError m LinkedOutputsFig
loadLinkedOutputsFig spec@{ file1, file2, dataFile, x } = do
   let
      dir = File "linked-outputs/"
      name1 × name2 = (dir <> file1) × (dir <> file2)
      dataFile' = File "example/" <> dir <> dataFile
   -- views share ambient environment γ
   { γ: γ' } <- defaultImports >>= datasetAs dataFile' x >>= initialConfig
   s1' × s2' <- (×) <$> open name1 <*> open name2
   let
      γ = botOf <$> γ'
      s1 = botOf s1'
      s2 = botOf s2'
   dataFileStr <- loadFile (Folder "fluid") dataFile' -- TODO: use surface expression instead
   e1 × e2 <- (×) <$> desug s1 <*> desug s2
   t1 × v1 <- eval γ e1 bot
   t2 × v2 <- eval γ e2 bot
   let v0 = get x γ
   pure { spec, γ, s1, s2, e1, e2, t1, t2, v1, v2, v0, dataFileStr }
