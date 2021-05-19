module App.Demo where

import Prelude hiding (absurd)
import App.Renderer (renderFigure)
import Bindings (find)
import Data.Either (Either(..))
import Data.List (singleton)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import DesugarFwd (desugarModuleFwd)
import Eval (eval_module)
import EvalBwd (evalBwd)
import Lattice (𝔹)
import Module (openWithDefaultImports)
import SExpr (Expr(..), Module(..)) as S
import Test.Util (desugarEval)
import Util (MayFail, type (×), (×), successful)
import Val (Env, Val(..), holeMatrix, insertMatrix)

selectCell :: Int -> Int -> Int -> Int -> Val 𝔹
selectCell i j i' j' = Matrix true (insertMatrix i j (Hole true) (holeMatrix i' j'))

-- Rewrite example of the form (let <defs> in expr) to a "module" and expr, so we can treat defs as part of
-- the environment that we can easily inspect.
splitDefs :: Partial => S.Expr 𝔹 -> Env 𝔹 -> MayFail (Env 𝔹 × S.Expr 𝔹)
splitDefs (S.Let defs s) ρ =
   (desugarModuleFwd (S.Module (singleton (Left defs))) >>= eval_module ρ) <#> (_ × s)

example_needed :: Env 𝔹 -> S.Expr 𝔹 -> MayFail ((Val 𝔹 × Val 𝔹) × (Val 𝔹 × Val 𝔹) × (Val 𝔹 × Val 𝔹))
example_needed ρ1 s0 = do
   ρ2 × s <- unsafePartial (splitDefs s0 ρ1)
   ω <- find "filter" ρ2
   i <- find "image" ρ2
   t × o <- desugarEval (ρ1 <> ρ2) s
   let o' = selectCell 2 1 5 5
       ρ1ρ2 × _ × _ = evalBwd o' t
   ω' <- find "filter" ρ1ρ2
   i' <- find "image" ρ1ρ2
   pure ((o' × o) × (ω' × ω) × (i' × i))

-- Completely non-general, but fine for now.
makeFigure :: String -> String -> Effect Unit
makeFigure file divId =
   flip runAff_ (openWithDefaultImports ("slicing/" <> file)) \result ->
   case result of
      Left e -> log ("Open failed: " <> show e)
      Right (ρ × s) -> do
         let (o' × o) × (ω' × ω) × (i' × i) = successful (example_needed ρ s)
         renderFigure divId (o' × o) (ω' × ω) (i' × i)

main :: Effect Unit
main = do
   makeFigure "conv-wrap" "fig-1"
   makeFigure "conv-extend" "fig-2"
   makeFigure "conv-zero" "fig-3"
