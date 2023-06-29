module Test.Util where

import Prelude hiding (absurd)

import App.Fig (LinkFigSpec, linkResult, loadLinkFig)
import App.Util (Selector)
import Data.Either (Either(..))
import Data.List (elem)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd)
import DataType (dataTypeFor, typeName)
import Debug (trace)
import Desugarable (desugFwd', desugBwd')
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
--import Effect.Console (logShow)
import Eval (eval)
import EvalBwd (evalBwd)
import Lattice (𝔹, bot, erase)
import Module (File(..), Folder(..), loadFile, open, openDatasetAs, openWithDefaultImports, parse)
import Parse (program)
-- import Pretty (class Pretty, prettyP) as P 
import Pretty3 (pretty, class Pretty, prettyP)
import SExpr (Expr) as S
import Test.Spec (SpecT, before, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Mocha (runMocha)
import Util (type (×), (×), successful)
import Util.Pretty (render)
import Val (Env, Val(..), (<+>))

-- Don't enforce expected values for graphics tests (values too complex).
isGraphical :: forall a. Val a -> Boolean
isGraphical (Constr _ c _) = typeName (successful (dataTypeFor c)) `elem` [ "GraphicsElement", "Plot" ]
isGraphical _ = false

type Test a = SpecT Aff Unit Effect a

run :: forall a. Test a → Effect Unit
run = runMocha -- no reason at all to see the word "Mocha"

checkPretty :: forall a. Pretty a => String -> String -> a -> Aff Unit
checkPretty msg expected x =
   trace (msg <> ":\n") \_ ->
      prettyP x `shouldEqual` expected

-- testWithSetup :: Boolean -> File -> String -> Maybe (Selector × File) -> Aff (Env 𝔹 × S.Expr 𝔹) -> Test Unit
-- testWithSetup (bol) (File file) expected v_expect_opt setup =
--    before setup $
--       it file \(γ × s) -> do
--          let
--             e = successful (desugFwd' s)
--             t × v = successful (eval γ e bot)
--             v' = fromMaybe identity (fst <$> v_expect_opt) v
--             { γ: γ', e: e' } = evalBwd (erase <$> γ) (erase e) v' t
--             s' = desugBwd' e' :: S.Expr _
--             _ × v'' = successful (eval γ' (successful (desugFwd' s')) true)
--             src = render (pretty s)
--             srcExp = show (erase s)
--          trace ("1:\n" <> src) \_ -> do
--             liftEffect (log ("SRC\n" <> srcExp))
--             case parse src program of
--                Left msg -> fail msg
--                Right newProg -> do
--                   let newExp = show newProg
--                   --liftEffect (log ("SRC\n" <> srcExp))
--                   liftEffect (log ("NEW\n" <> newExp))
--                   liftEffect (logShow (eq (erase s) newProg))
--                   unless (isGraphical v'') (checkPretty "Value" expected v'')
--                   trace ("\n" <> src) \_ -> do
--                      unless (isGraphical v'') (checkPretty "Value" expected v'')
--                      case snd <$> v_expect_opt of
--                         Nothing -> pure unit
--                         Just file_expect ->
--                            loadFile (Folder "fluid/example") file_expect >>= flip (checkPretty "Source selection") s'

testWithSetup :: File -> String -> Maybe (Selector × File) -> Aff (Env 𝔹 × S.Expr 𝔹) -> Test Unit
testWithSetup (File file) expected v_expect_opt setup =
   before setup $
      it file \(γ × s) -> do
         let
            e = successful (desugFwd' s)
            t × v = successful (eval γ e bot)
            v' = fromMaybe identity (fst <$> v_expect_opt) v
            { γ: γ', e: e' } = evalBwd (erase <$> γ) (erase e) v' t
            s' = desugBwd' e' :: S.Expr _
            _ × v'' = successful (eval γ' (successful (desugFwd' s')) true)
            src = render (pretty s)
            srcExp = show (erase s)
         case parse src program of
            Left msg -> fail msg
            Right newProg -> do
               trace ("1:\n" <> src) \_ -> do
                  let newExp = show newProg
                  case (eq (erase s) newProg) of
                     false -> do
                        liftEffect (log ("SRC\n" <> srcExp))
                        liftEffect (log ("NEW\n" <> newExp))
                        fail "not equal"
                     true -> do
                        unless (isGraphical v'') (checkPretty "line103" expected v'')
                        trace ("\n" <> src) \_ -> do
                           unless (isGraphical v'') (checkPretty "line105" expected v'')
                           case snd <$> v_expect_opt of
                              Nothing -> pure unit
                              Just file_expect -> do
                                 expect <- loadFile (Folder "fluid/example") file_expect
                                 liftEffect (log ("SRC0\n" <> show s'))
                                 liftEffect (log ("SRC\n" <> (render (pretty s'))))
                                 liftEffect (log ("EXP\n" <> expect))
                                 checkPretty "Source selection" expect s'

test :: File -> String -> Test Unit
test file expected = testWithSetup file expected Nothing (openWithDefaultImports file)

testBwd :: File -> File -> Selector -> String -> Test Unit
testBwd file file_expect δv expected =
   testWithSetup file' expected (Just (δv × (folder <> file_expect))) (openWithDefaultImports file')
   where
   folder = File "slicing/"
   file' = folder <> file

testLink :: LinkFigSpec -> Selector -> String -> Test Unit
testLink spec@{ x } δv1 v2_expect =
   before (loadLinkFig spec) $
      it ("linking/" <> show spec.file1 <> " <-> " <> show spec.file2)
         \{ γ0, γ, e1, e2, t1, t2, v1 } ->
            let
               { v': v2' } = successful $ linkResult x γ0 γ e1 e2 t1 t2 (δv1 v1)
            in
               checkPretty "Linked output" v2_expect v2'

testWithDataset :: File -> File -> Test Unit
testWithDataset dataset file = do
   testWithSetup file "" Nothing $ do
      γ0 × γ <- openDatasetAs dataset "data"
      ((γ0 <+> γ) × _) <$> open file
