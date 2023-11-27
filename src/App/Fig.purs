module App.Fig where

import Prelude hiding (absurd)

import App.CodeMirror (EditorView, addEditorView, dispatch, getContentsLength, update)
import App.Util (HTMLId, doNothing)
import App.Util.Select (envVal)
import App.View (View, drawView, view)
import Bindings (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Array (range, zip)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.List (singleton)
import Data.Newtype (unwrap)
import Data.Set (singleton) as S
import Data.Traversable (sequence, sequence_)
import Data.Tuple (snd, uncurry)
import Desugarable (desug)
import Dict (get)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Eval (eval, eval_module)
import EvalBwd (TracedEval, evalBwd, traceGC)
import Expr (Expr)
import Foreign.Object (lookup)
import GaloisConnection (GaloisConnection(..))
import Lattice (𝔹, Raw, bot, botOf, erase, neg, topOf)
import Module (File(..), Folder(..), initialConfig, datasetAs, defaultImports, loadFile, open)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import SExpr (desugarModuleFwd)
import Test.Util (Selector)
import Trace (Trace)
import Util (type (+), type (×), AffError, Endo, absurd, orElse, uncurry3, (×))
import Val (class Ann, Env, Val, append_inv, (<+>))

codeMirrorDiv :: Endo String
codeMirrorDiv = ("codemirror-" <> _)

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
   , s0 :: Raw S.Expr -- program that was originally "split"
   , s :: Raw S.Expr -- body of example
   , gc :: TracedEval 𝔹
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
   , x1 :: Var
   , x1File :: File -- variables to be considered "inputs"
   , x2 :: Var
   , x2File :: File
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
      [ 2 × ((δv1 >>> _) >>> Left >>> drawLinkedOutputsFig fig) × view "left view" v1'
      , 0 × ((δv2 >>> _) >>> Right >>> drawLinkedOutputsFig fig) × view "right view" v2'
      , 1 × doNothing × view "common data" v0
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
      [ 0 × doNothing × view "common output" v0
      , 2 × ((δv1 >>> _) >>> Left >>> drawLinkedInputsFig fig) × view x1 v1'
      , 1 × ((δv2 >>> _) >>> Right >>> drawLinkedInputsFig fig) × view x2 v2'
      ]

drawFig :: Fig -> EditorView -> Selector Val -> Effect Unit
drawFig fig@{ spec: { divId }, s0 } ed δv = do
   v_view × views <- figViews fig δv
   sequence_ $
      uncurry (flip (drawView divId) doNothing) <$> zip (range 0 (length views - 1)) views
   drawView divId (length views) ((δv >>> _) >>> drawFig fig ed) v_view
   drawCode (prettyP s0) ed

drawFigWithCode :: Fig -> Effect Unit
drawFigWithCode fig =
   addEditorView (codeMirrorDiv fig.spec.divId) >>= flip (drawFig fig) botOf

drawCode :: String -> EditorView -> Effect Unit
drawCode s ed =
   dispatch ed =<< update ed.state [ { changes: { from: 0, to: getContentsLength ed, insert: s } } ]

drawFile :: File × String -> Effect Unit
drawFile (file × src) =
   addEditorView (codeMirrorDiv $ unwrap file) >>= drawCode src

varView :: forall m. MonadError Error m => Var -> Env 𝔹 -> m View
varView x γ = view x <$> (lookup x γ # orElse absurd)

-- For an output selection, views of corresponding input selections and output after round-trip.
figViews :: forall m. MonadError Error m => Fig -> Selector Val -> m (View × Array View)
figViews { spec: { xs }, gc: { gc: GC { bwd, fwd }, v } } δv = do
   let
      γ0γ × e' × α = bwd (δv (botOf v))
      v' = fwd (γ0γ × e' × α)
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

loadFig :: forall m. FigSpec -> AffError m Fig
loadFig spec@{ file } = do
   { γ: γ' } <- defaultImports >>= initialConfig
   let γ0 = botOf <$> γ'
   s' <- open file
   let s0 = botOf s'
   { γ: γ1, s } <- splitDefs γ0 s0
   e <- desug s
   let γ = γ0 <+> γ1
   gc <- traceGC γ e
   pure { spec, s0, s, gc }

loadLinkedInputsFig :: forall m. LinkedInputsFigSpec -> AffError m LinkedInputsFig
loadLinkedInputsFig spec@{ file } = do
   let
      dir = File "example/linked-inputs/"
      datafile1 × datafile2 = (dir <> spec.x1File) × (dir <> spec.x2File)
   { γ: γ' } <- defaultImports >>= datasetAs datafile1 spec.x1 >>= datasetAs datafile2 spec.x2 >>= initialConfig
   let γ = botOf <$> γ'
   s <- botOf <$> open (File "linked-inputs/" <> file)
   e <- desug s
   t × v <- eval γ e bot
   pure { spec, γ, s, e, t, v0: v }

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
