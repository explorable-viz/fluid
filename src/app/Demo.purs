module App.Demo where

import Prelude hiding (absurd)
import Data.Array (zip)
import Data.Either (Either(..))
import Data.List (singleton)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import App.Renderer (Fig, MakeFig, barChart, drawFigure, energyTable, matrixFig)
import Bindings (Var, (↦), find, update)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Lattice (𝔹, botOf, neg)
import Module (openWithDefaultImports, openDatasetAs)
import Primitive (Slice)
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import Util (MayFail, type (×), (×), type (+), error, successful)
import Val (Env, Val(..), holeMatrix, insertMatrix)

selectCell :: Int -> Int -> Int -> Int -> Val 𝔹
selectCell i j i' j' = Matrix true (insertMatrix i j (Hole true) (holeMatrix i' j'))

-- Rewrite example of the form (let <defs> in expr) to a "module" and expr, so we can treat defs as part of
-- the environment that we can easily inspect.
splitDefs :: Partial => Env 𝔹 -> S.Expr 𝔹 -> MayFail (Env 𝔹 × S.Expr 𝔹)
splitDefs ρ s' =
   let defs × s = unpack s' in
   (desugarModuleFwd (S.Module (singleton defs)) >>= eval_module ρ) <#> (_ × s)
   where unpack :: S.Expr 𝔹 -> (S.VarDefs 𝔹 + S.RecDefs 𝔹) × S.Expr 𝔹
         unpack (S.LetRec defs s) = Right defs × s
         unpack (S.Let defs s) = Left defs × s

type Example = Env 𝔹 -> S.Expr 𝔹 -> MayFail (Array Fig)
type VarSpec = {
   var :: Var,
   fig :: MakeFig
}

example_needed :: Array VarSpec -> MakeFig -> Val 𝔹 -> Example
example_needed x_figs o_fig o' ρ s0 = do
   ρ' × s <- unsafePartial (splitDefs ρ s0)
   e <- desugarFwd s
   let ρρ' = ρ <> ρ'
   t × o <- eval ρρ' e
   let ρρ'' × _ × _ = evalBwd o' t
       xs = _.var <$> x_figs
   vs <- sequence (flip find ρρ' <$> xs)
   vs' <- sequence (flip find ρρ'' <$> xs)
   pure $ [ unsafePartial o_fig "output" "LightGreen" (o' × o) ] <> (varFig <$> zip x_figs (zip vs' vs))
   where
      varFig :: VarSpec × Slice (Val 𝔹) -> Fig
      varFig ({var: x, fig} × (v × u)) = unsafePartial (fig x "Yellow" (v × u))

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
      unsafePartial matrixFig "output" "Yellow" (o' × o),
      unsafePartial matrixFig "filter" "LightGreen" (ω' × ω),
      unsafePartial matrixFig "input" "Yellow" (i' × i)
   ]

makeFigure :: String -> Example -> String -> Effect Unit
makeFigure file example divId =
   flip runAff_ (burble file)
   case _ of
      Left e -> log ("Open failed: " <> show e)
      Right (ρ × s) -> do
         drawFigure divId (successful (example ρ s))

-- TODO: rename; consolidate with similar test util code/move to Module; not every example should run in
-- context of renewables data.
burble :: String -> Aff (Env 𝔹 × S.Expr 𝔹)
burble file = do
   ρ0 × s <- openWithDefaultImports file
   ρ <- openDatasetAs ("example/linking/" <> "renewables") "data"
   pure ((ρ0 <> ρ) × s)

convolutionFigs :: Effect Unit
convolutionFigs = do
   makeFigure "slicing/conv-wrap"
              (example_needed [{ var: "filter", fig: matrixFig }, { var: "image", fig: matrixFig } ]
              matrixFig
              (selectCell 2 1 5 5))
              "fig-1"
   makeFigure "slicing/conv-wrap" example_neededBy "fig-2"
   makeFigure "slicing/conv-zero"
              (example_needed [{ var: "filter", fig: matrixFig }, { var: "image", fig: matrixFig } ]
              matrixFig
              (selectCell 2 1 5 5))
              "fig-3"
   makeFigure "slicing/conv-zero" example_neededBy "fig-4"

linkingFigs :: Effect Unit
linkingFigs = do
   makeFigure "linking/bar-chart"
              (example_needed [{ var: "data", fig: energyTable } ] barChart (Hole false)) "table-1"

main :: Effect Unit
main = do
   linkingFigs
--   convolutionFigs
