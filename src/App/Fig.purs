module App.Fig where

import Prelude hiding (absurd)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (HTMLId, Sel(..), doNothing, toSel)
import App.Util.Selector (envVal)
import App.View (View, drawView, view)
import Bind (Bind, Var, (↦))
import Control.Monad.Error.Class (class MonadError)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Strong (first)
import Data.Traversable (sequence, sequence_)
import Data.Tuple (snd)
import Desugarable (desug)
import Dict (Dict, filterKeys, get, mapWithKey)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Eval (eval)
import EvalBwd (evalBwd)
import EvalGraph (GraphEval, graphGC)
import Expr (Expr)
import Foreign.Object (lookup)
import GaloisConnection (relatedInputs, relatedOutputs)
import Graph.GraphImpl (GraphImpl)
import Lattice (𝔹, Raw, bot, botOf, erase, neg, topOf)
import Module (File(..), Folder(..), initialConfig, loadFile, loadProgCxt, open)
import Pretty (prettyP)
import SExpr (Expr) as S
import Test.Util (Selector)
import Trace (Trace)
import Util (type (+), type (×), AffError, Endo, absurd, error, orElse, singleton, uncurry3, (×))
import Val (Env, Val, append_inv, (<+>))

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

type FigSpec =
   { divId :: HTMLId
   , imports :: Array String
   , datasets :: Array (Bind String)
   , file :: File
   , ins :: Array Var -- variables to be considered "inputs"
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

type LinkedOutputsFigSpec =
   { divId :: HTMLId
   , imports :: Array String
   , dataFile :: File
   , file1 :: File
   , file2 :: File
   , x :: Var
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

type LinkedInputsFigSpec =
   { divId :: HTMLId
   , file :: File
   , x1 :: Var
   , x1File :: File -- variables to be considered "inputs"
   , x2 :: Var
   , x2File :: File
   }

type LinkedInputsFig =
   { spec :: LinkedInputsFigSpec
   , γ :: Env 𝔹
   , s :: S.Expr 𝔹
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

runAffs_ :: forall a. (a -> Effect Unit) -> Array (Aff a) -> Effect Unit
runAffs_ f as = flip runAff_ (sequence as) case _ of
   Left err -> log $ show err
   Right as' -> as' <#> f # sequence_

split :: Selector Val + Selector Val -> Selector Val × Selector Val
split (Left δv) = δv × identity
split (Right δv) = identity × δv

drawLinkedOutputsFig :: LinkedOutputsFig -> Selector Val + Selector Val -> Effect Unit
drawLinkedOutputsFig fig@{ spec: { divId } } δv = do
   v1' × v2' × v0 <- linkedOutputsResult fig δv
   let δv1 × δv2 = split δv
   sequence_ $ uncurry3 (drawView divId) <$>
      [ "2" × ((δv1 >>> _) >>> Left >>> drawLinkedOutputsFig fig) × view "left view" (v1' <#> toSel)
      , "0" × ((δv2 >>> _) >>> Right >>> drawLinkedOutputsFig fig) × view "right view" (v2' <#> toSel)
      , "1" × doNothing × view "common data" (v0 <#> toSel)
      ]

drawLinkedOutputsFigWithCode :: LinkedOutputsFig -> Effect Unit
drawLinkedOutputsFigWithCode fig = do
   drawLinkedOutputsFig fig (Left botOf)
   sequence_ $ (\(File file × s) -> addEditorView (codeMirrorDiv file) >>= drawCode s) <$>
      [ fig.spec.file1 × prettyP fig.s1
      , fig.spec.file2 × prettyP fig.s2
      , fig.spec.dataFile × fig.dataFileStr
      ]

drawLinkedInputsFig :: LinkedInputsFig -> Selector Val + Selector Val -> Effect Unit
drawLinkedInputsFig fig@{ spec: { divId, x1, x2 } } δv = do
   v1' × v2' × v0 <- linkedInputsResult fig δv
   let δv1 × δv2 = split δv
   sequence_ $ uncurry3 (drawView divId) <$>
      [ "0" × doNothing × view "common output" (v0 <#> toSel)
      , "2" × ((δv1 >>> _) >>> Left >>> drawLinkedInputsFig fig) × view x1 (v1' <#> toSel)
      , "1" × ((δv2 >>> _) >>> Right >>> drawLinkedInputsFig fig) × view x2 (v2' <#> toSel)
      ]

drawFigWithCode :: Fig -> Effect Unit
drawFigWithCode fig = do
   drawFig fig
   drawCode (prettyP fig.s) =<< addEditorView (codeMirrorDiv fig.spec.divId)

-- Pseudo-variable to use as name of output view.
output :: String
output = "output"

drawFig :: Fig -> Effect Unit
drawFig fig@{ spec: { divId }, in_, out, dir } = do
   let out_view × in_views = figViews fig
   sequence_ $ mapWithKey (\x -> drawView divId x (onInSel x)) in_views
   drawView divId output onOutSel out_view
   where
   onOutSel :: Selector Val -> Effect Unit
   onOutSel δv = drawFig (fig { out = δv out, in_ = in', dir = LinkedOutputs })
      where
      -- TODO: replace (expensive) botOf γ by per-variable botOf
      in' = if dir == LinkedInputs then first botOf in_ else in_

   onInSel :: Var -> Selector Val -> Effect Unit
   onInSel x δv = drawFig (fig { in_ = first (envVal x δv) in_, out = out', dir = LinkedInputs })
      where
      out' = if dir == LinkedOutputs then botOf out else out

figViews :: Fig -> View × Dict View
figViews { spec: { ins }, gc: { gc }, out, dir: LinkedOutputs } =
   view output (asSel <$> out <*> out') ×
      mapWithKey (\x _ -> view x (toSel <$> get x γ)) (γ # filterKeys (_ `elem` ins))
   where
   out' × γ × _ = (unwrap (relatedOutputs gc)).fwd out
figViews { spec: { ins }, gc: { gc }, in_: γ × e, dir: LinkedInputs } =
   view output (toSel <$> out) ×
      mapWithKey (\x _ -> view x (asSel <$> get x γ <*> get x γ')) (γ # filterKeys (_ `elem` ins))
   where
   (γ' × _) × out = (unwrap (relatedInputs gc)).fwd (γ × e)

drawCode :: String -> EditorView -> Effect Unit
drawCode s ed =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]

drawFile :: File × String -> Effect Unit
drawFile (file × src) =
   addEditorView (codeMirrorDiv $ unwrap file) >>= drawCode src

asSel :: 𝔹 -> 𝔹 -> Sel
asSel false false = None
asSel false true = Secondary
asSel true false = Primary -- "costless output", but ignore those for now
asSel true true = Primary

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
         γ0' × γ' = append_inv (singleton x) γ0γ'
      v0' <- lookup x γ' # orElse absurd
      -- make γ0 and e2 fully available
      v' <- eval (neg ((botOf γ0') <+> γ')) (topOf e') true <#> snd >>> neg
      pure { v, v', v0' }

linkedInputsResult :: forall m. MonadEffect m => MonadError Error m => LinkedInputsFig -> Selector Val + Selector Val -> m (Val 𝔹 × Val 𝔹 × Val 𝔹)
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
      v0 <- eval (neg γ') (botOf e) true <#> snd >>> neg
      let γ'' × _ = evalBwd (erase <$> γ) (erase e) v0 t
      v <- lookup x γ' # orElse absurd
      v' <- lookup x' γ'' # orElse absurd
      pure { v, v', v0 }

linkedInputsResult2 :: forall m. MonadEffect m => MonadError Error m => Fig -> Bind (Selector Val) -> m (Env 𝔹 × Expr 𝔹)
linkedInputsResult2 = error "todo"

loadFig :: forall m. FigSpec -> AffError m Fig
loadFig spec@{ imports, file, datasets } = do
   s <- open file
   e <- desug s
   gconfig <- loadProgCxt imports datasets >>= initialConfig e
   gc <- graphGC gconfig e
   pure { spec, s, gc, in_: botOf gc.γα × topOf e, out: botOf gc.vα, dir: LinkedOutputs }

loadLinkedInputsFig :: forall m. LinkedInputsFigSpec -> AffError m LinkedInputsFig
loadLinkedInputsFig spec@{ file } = do
   let
      dir = File "example/linked-inputs/"
      datafile1 × datafile2 = (dir <> spec.x1File) × (dir <> spec.x2File)
   s <- botOf <$> open (File "linked-inputs/" <> file)
   e <- desug s
   { γ: γ' } <- loadProgCxt [] [ spec.x1 ↦ unwrap datafile1, spec.x2 ↦ unwrap datafile2 ] >>= initialConfig e
   let γ = botOf γ'
   t × v <- eval γ e bot
   pure { spec, γ, s, e, t, v0: v }

loadLinkedOutputsFig :: forall m. LinkedOutputsFigSpec -> AffError m LinkedOutputsFig
loadLinkedOutputsFig spec@{ imports, dataFile, file1, file2, x } = do
   let
      dir = File "linked-outputs/"
      dataFile' = File "example/" <> dir <> dataFile
      name1 × name2 = (dir <> file1) × (dir <> file2)
   -- views share ambient environment γ
   s1' × s2' <- (×) <$> open name1 <*> open name2
   let s1 × s2 = botOf s1' × botOf s2'
   e1 × e2 <- (×) <$> desug s1 <*> desug s2
   { γ: γ' } <- loadProgCxt imports [ x ↦ unwrap dataFile' ] >>= initialConfig (e1 × e2)
   let γ = botOf γ'
   dataFileStr <- loadFile (Folder "fluid") dataFile' -- TODO: use surface expression instead
   t1 × v1 <- eval γ e1 bot
   t2 × v2 <- eval γ e2 bot
   let v0 = get x γ
   pure { spec, γ, s1, s2, e1, e2, t1, t2, v1, v2, v0, dataFileStr }

-- ======================
-- boilerplate
-- ======================

derive instance Eq Direction
