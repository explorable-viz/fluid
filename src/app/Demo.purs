module App.Demo where

import Prelude hiding (absurd)
import Data.Either (Either(..))
import Data.List (singleton)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import App.Renderer (matrixFig, renderFigures)
import Bindings ((↦), find, update)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Lattice (𝔹, botOf, neg)
import Module (openWithDefaultImports)
import SExpr (Expr(..), Module(..)) as S
import Test.Util (desugarEval)
import Util (MayFail, type (×), (×), successful)
import Val (Env, Val(..), holeMatrix, insertMatrix)

selectCell :: Int -> Int -> Int -> Int -> Val 𝔹
selectCell i j i' j' = Matrix true (insertMatrix i j (Hole true) (holeMatrix i' j'))

-- Rewrite example of the form (let <defs> in expr) to a "module" and expr, so we can treat defs as part of
-- the environment that we can easily inspect.
splitDefs :: Partial => Env 𝔹 -> S.Expr 𝔹 -> MayFail (Env 𝔹 × S.Expr 𝔹)
splitDefs ρ (S.Let defs s) =
   (desugarModuleFwd (S.Module (singleton (Left defs))) >>= eval_module ρ) <#> (_ × s)

type ConvExample = Env 𝔹 -> S.Expr 𝔹 -> MayFail ((Val 𝔹 × Val 𝔹) × (Val 𝔹 × Val 𝔹) × (Val 𝔹 × Val 𝔹))

example_needed :: ConvExample
example_needed ρ s0 = do
   ρ' × s <- unsafePartial (splitDefs ρ s0)
   t × o <- desugarEval (ρ <> ρ') s
   let o' = selectCell 2 1 5 5
       ρρ' × _ × _ = evalBwd o' t
   ω <- find "filter" ρ'
   i <- find "image" ρ'
   ω' <- find "filter" ρρ'
   i' <- find "image" ρρ'
   pure ((o' × o) × (ω' × ω) × (i' × i))

example_neededBy :: ConvExample
example_neededBy ρ s0 = do
   ρ' × s <- unsafePartial (splitDefs ρ s0)
   e <- desugarFwd s
   t × o <- eval (ρ <> ρ') e
   let i' = selectCell 1 2 5 5
       ρ'' = update (botOf ρ') ("image" ↦ i')
       o' = neg (evalFwd (neg (botOf ρ <> ρ'')) (const true <$> e) true t)
   ω <- find "filter" ρ'
   i <- find "image" ρ'
   ω' <- find "filter" ρ''
   pure ((o' × o) × (ω' × ω) × (i' × i))

makeFigure :: String -> ConvExample -> String -> Effect Unit
makeFigure file example divId =
   flip runAff_ (openWithDefaultImports ("slicing/" <> file))
   case _ of
      Left e -> log ("Open failed: " <> show e)
      Right (ρ × s) -> do
         let (o' × o) × (ω' × ω) × (i' × i) = successful (example ρ s)
         renderFigures divId [
            matrixFig "input" "LightGreen" (i' × i),
            matrixFig "filter" "LightGreen" (ω' × ω),
            matrixFig "output" "Yellow" (o' × o)
         ]

main :: Effect Unit
main = do
   makeFigure "conv-wrap" example_needed "fig-1"
   makeFigure "conv-extend" example_neededBy "fig-2"
   makeFigure "conv-zero" example_needed "fig-3"
