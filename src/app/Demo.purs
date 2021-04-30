module App.Demo where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import App.Renderer (renderMatrix)
import Module (openWithDefaultImports)
import Primitive (match)
import Test.Util (desugarEval, desugarEval_bwd, desugarEval_fwd)
import Util ((×))
import Val (Val(..), holeMatrix, insertMatrix)

main :: Effect Unit
main =
   flip runAff_ (openWithDefaultImports "slicing/conv-extend") \result ->
   case result of
      Left e -> log ("Open failed: " <> show e)
      Right (ρ × s) -> case desugarEval ρ s of
         Left msg -> log ("Execution failed: " <> msg)
         Right (t × _) -> do
            let v = Matrix true (insertMatrix 2 2 (Hole true) (holeMatrix 5 5))
                ρ' × s' = desugarEval_bwd (t × s) v
                v' = desugarEval_fwd ρ' s' t
            renderMatrix (match v')
