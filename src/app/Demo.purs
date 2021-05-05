module App.Demo where

import Prelude hiding (absurd)
import App.Renderer (renderFigure)
import Bindings (find)
import Data.Either (Either(..))
import Data.List (singleton)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import DesugarFwd (desugarModuleFwd)
import Eval (eval_module)
import Lattice (𝔹)
import Module (openWithDefaultImports)
import SExpr (Expr(..), Module(..)) as S
import Test.Util (desugarEval, desugarEval_bwd)
import Util (MayFail, type (×), (×), absurd, error, successful)
import Val (Env, Val(..), holeMatrix, insertMatrix)

-- We require examples to be of the form (let <defs> in expr), and rewrite them to a "module" and expr, so
-- we can treat the defs as part of the environment that we can easily inspect.
splitDefs :: S.Expr 𝔹 -> Env 𝔹 -> MayFail (Env 𝔹 × S.Expr 𝔹)
splitDefs (S.Let defs s) ρ = do
   ρ' <- desugarModuleFwd (S.Module (singleton (Left defs))) >>= eval_module ρ
   pure (ρ' × s)
splitDefs _ _ = error absurd

-- This is completely non-general, but that's fine for now.
makeFigure :: String -> String -> Effect Unit
makeFigure file divId =
   flip runAff_ (openWithDefaultImports ("slicing/" <> file)) \result ->
   case result of
      Left e -> log ("Open failed: " <> show e)
      Right (ρ1 × s0) ->
         let ρ2 × s = successful (splitDefs s0 ρ1)
             filter = successful (find "filter" ρ2)
             input = successful (find "image" ρ2) in
         case desugarEval (ρ1 <> ρ2) s of
            Left msg -> log ("Execution failed: " <> msg)
            Right (t × output) -> do
               let output' = Matrix true (insertMatrix 2 1 (Hole true) (holeMatrix 5 5))
                   ρ1ρ2 × s' = desugarEval_bwd (t × s) output'
                   filter' = successful (find "filter" ρ1ρ2)
                   input' = successful (find "image" ρ1ρ2)
               renderFigure divId (input' × input) (filter' × filter) (output' × output)

main :: Effect Unit
main = makeFigure "conv-extend" "fig-2"
