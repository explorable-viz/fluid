module App.Demo where

import Prelude hiding (absurd)
import Data.Array (zip)
import Data.Either (Either(..))
import Data.List (singleton)
import Data.Traversable (sequence)
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import App.Renderer (Fig, {-drawBarChart, -} drawFigure, matrixFig)
import Bindings (Var, (↦), find, update)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Lattice (𝔹, botOf, neg)
import Module (openWithDefaultImports, openDatasetAs)
import SExpr (Expr(..), Module(..)) as S
import Util (MayFail, type (×), (×), successful)
import Val (Env, Val(..), holeMatrix, insertMatrix)

selectCell :: Int -> Int -> Int -> Int -> Val 𝔹
selectCell i j i' j' = Matrix true (insertMatrix i j (Hole true) (holeMatrix i' j'))

-- Rewrite example of the form (let <defs> in expr) to a "module" and expr, so we can treat defs as part of
-- the environment that we can easily inspect.
splitDefs :: Partial => Env 𝔹 -> S.Expr 𝔹 -> MayFail (Env 𝔹 × S.Expr 𝔹)
splitDefs ρ (S.Let defs s) =
   (desugarModuleFwd (S.Module (singleton (Left defs))) >>= eval_module ρ) <#> (_ × s)

type Example = Env 𝔹 -> S.Expr 𝔹 -> MayFail (Array Fig)

example_needed :: Array Var -> Val 𝔹 -> Example
example_needed xs o' ρ s0 = do
   ρ' × s <- unsafePartial (splitDefs ρ s0)
   e <- desugarFwd s
   let ρρ' = ρ <> ρ'
   t × o <- eval ρρ' e
   let ρρ'' × _ × _ = evalBwd o' t
   vs <- sequence (flip find ρρ' <$> xs)
   vs' <- sequence (flip find ρρ'' <$> xs)
   pure $ [
      matrixFig "output" "LightGreen" (o' × o)
   ] <> (uncurry (flip matrixFig "Yellow") <$> zip xs (zip vs' vs))

example_neededBy :: Example
example_neededBy ρ s0 = do
   ρ' × s <- unsafePartial (splitDefs ρ s0)
   e <- desugarFwd s
   t × o <- eval (ρ <> ρ') e
   let ω' = selectCell 1 1 3 3
       ρ'' = update (botOf ρ') ("filter" ↦ ω')
       o' = neg (evalFwd (neg (botOf ρ <> ρ'')) (const true <$> e) true t)
   ω <- find "filter" ρ'
   i <- find "image" ρ'
   i' <- find "image" ρ''
   pure [
      matrixFig "output" "Yellow" (o' × o),
      matrixFig "filter" "LightGreen" (ω' × ω),
      matrixFig "input" "Yellow" (i' × i)
   ]

makeFigure :: String -> Example -> String -> Effect Unit
makeFigure file example divId =
   flip runAff_ (burble file)
   case _ of
      Left e -> log ("Open failed: " <> show e)
      Right (ρ × s) -> do
         drawFigure divId (successful (example ρ s))

-- TODO: consolidate with similar test util code; move to Module?
burble :: String -> Aff (Env 𝔹 × S.Expr 𝔹)
burble file = do
   ρ0 × s <- openWithDefaultImports file
   ρ <- openDatasetAs ("example/linking/" <> "renewables") "data"
   pure ((ρ0 <> ρ) × s)

main :: Effect Unit
main = do
--   drawBarChart "fig-bar-chart"
   makeFigure "linking/line-chart" (example_needed ["data"] (Hole false)) "table-1"
   makeFigure "slicing/conv-wrap" (example_needed ["filter", "image"] (selectCell 2 1 5 5)) "fig-1"
   makeFigure "slicing/conv-wrap" example_neededBy "fig-2"
   makeFigure "slicing/conv-zero" (example_needed ["filter", "image"] (selectCell 2 1 5 5)) "fig-3"
   makeFigure "slicing/conv-zero" example_neededBy "fig-4"
