module App.Demo where

import Prelude hiding (absurd)
import Data.Array (zip)
import Data.Either (Either(..))
import Data.List (List(..), (:), singleton)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import App.Renderer (Fig, MakeFig, drawFigure, makeBarChart, makeEnergyTable, matrixFig)
import Bindings (Bind, Var, (↦), find, update)
import DataType (cBarChart, cCons)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Lattice (𝔹, botOf, neg)
import Primitive (Slice)
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import Test.Util (openFileWithDataset)
import Util (Endo, MayFail, type (×), (×), type (+), successful)
import Util.SnocList (SnocList(..), (:-))
import Val (Env, Val(..), holeMatrix, insertMatrix)

selectCell :: Int -> Int -> Int -> Int -> Val 𝔹
selectCell i j i' j' = Matrix false (insertMatrix i j (Hole true) (holeMatrix i' j'))

selectNth :: Int -> Val 𝔹 -> Val 𝔹
selectNth 0 v = Constr false cCons (v : Hole false : Nil)
selectNth n v = Constr false cCons (Hole false : selectNth (n - 1) v : Nil)

select_y :: Val 𝔹
select_y = Record false (Lin :- "x" ↦ Hole false :- "y" ↦ Hole true)

select_barChart_data :: Val 𝔹 -> Val 𝔹
select_barChart_data v = Constr false cBarChart (Record false (Lin :- "caption" ↦ Hole false :- "data" ↦ v) : Nil)

-- Example assumed to be of the form (let <defs> in expr), so we can treat defs as part of the environment that
-- we can easily inspect.
type Example = {
   ρ :: Env 𝔹,       -- module environment
   ρ' :: Env 𝔹,      -- additional bindings introduce by "let" at beginning of example
   s :: S.Expr 𝔹     -- body of let
}

-- Extract the ρ' and s components of an example s'.
splitDefs :: Partial => Env 𝔹 -> S.Expr 𝔹 -> MayFail Example
splitDefs ρ s' = do
   let defs × s = unpack s'
   ρ' <- desugarModuleFwd (S.Module (singleton defs)) >>= eval_module ρ
   pure { ρ, ρ', s }
   where unpack :: S.Expr 𝔹 -> (S.VarDefs 𝔹 + S.RecDefs 𝔹) × S.Expr 𝔹
         unpack (S.LetRec defs s)   = Right defs × s
         unpack (S.Let defs s)      = Left defs × s

type VarSpec = {
   var :: Var,
   fig :: MakeFig
}

varFig :: Partial => VarSpec × Slice (Val 𝔹) -> Fig
varFig ({var: x, fig} × uv) = fig { title: x, uv }

example_needed :: Partial => Array VarSpec -> MakeFig -> Val 𝔹 -> Example -> MayFail (Array Fig)
example_needed x_figs o_fig o' {ρ, ρ', s} = do
   e <- desugarFwd s
   let ρρ' = ρ <> ρ'
   t × o <- eval ρρ' e
   let ρρ'' × _ × _ = evalBwd o' t
       xs = _.var <$> x_figs
   vs <- sequence (flip find ρρ' <$> xs)
   vs' <- sequence (flip find ρρ'' <$> xs)
   pure $ [ o_fig { title: "output", uv: o' × o } ] <> (varFig <$> zip x_figs (zip vs' vs))

example_neededBy :: Partial => MakeFig -> Val 𝔹 -> Example -> MayFail (Array Fig)
example_neededBy o_fig ω' {ρ, ρ', s} = do
   e <- desugarFwd s
   let ρρ' = ρ <> ρ'
       ρ'' = selectOnly ("filter" ↦ ω') ρ'
   t × o <- eval ρρ' e
   let o' = neg (evalFwd (neg (botOf ρ <> ρ'')) (const true <$> e) true t)
   let x_figs = [ { var: "filter", fig: matrixFig }, { var: "image", fig: matrixFig } ] :: Array VarSpec
       xs = _.var <$> x_figs
   vs <- sequence (flip find ρ' <$> xs)
   vs' <- sequence (flip find ρ'' <$> xs)
   pure $ [ o_fig { title: "output", uv: o' × o } ] <> (varFig <$> zip x_figs (zip vs' vs))

selectOnly :: Bind (Val 𝔹) -> Endo (Env 𝔹)
selectOnly xv ρ = update (botOf ρ) xv

makeFigures :: Partial => String -> (Example -> MayFail (Array Fig)) -> String -> Effect Unit
makeFigures file makeFigs divId =
   flip runAff_ (openFileWithDataset "example/linking/renewables" file)
   case _ of
      Left e -> log ("Open failed: " <> show e)
      Right (ρ × s) -> do
         drawFigure divId (successful (splitDefs ρ s >>= makeFigs))

-- selectOnly ("filter" ↦ selectCell 1 1 3 3) ρ'

-- TODO: not every example should run in context of renewables data.
convolutionFigs :: Partial => Effect Unit
convolutionFigs = do
   makeFigures "slicing/conv-wrap"
               (example_needed [{ var: "filter", fig: matrixFig }, { var: "image", fig: matrixFig }]
                               matrixFig
                               (selectCell 2 1 5 5))
               "fig-1"
   makeFigures "slicing/conv-wrap" (example_neededBy matrixFig (selectCell 1 1 3 3)) "fig-2"
   makeFigures "slicing/conv-zero"
               (example_needed [{ var: "filter", fig: matrixFig }, { var: "image", fig: matrixFig }]
                               matrixFig
                               (selectCell 2 1 5 5))
               "fig-3"
   makeFigures "slicing/conv-zero" (example_neededBy matrixFig (selectCell 1 1 3 3)) "fig-4"

linkingFigs :: Partial => Effect Unit
linkingFigs = do
   makeFigures "linking/bar-chart"
               (example_needed [{ var: "data", fig: makeEnergyTable }]
                               makeBarChart
                               (select_barChart_data (selectNth 1 (select_y))))
               "table-1"
   makeFigures "linking/bar-chart"
               (example_needed [{ var: "data", fig: makeEnergyTable }]
                               makeBarChart
                               (select_barChart_data (selectNth 0 (select_y))))
               "table-2"

main :: Effect Unit
main = unsafePartial $ do
   linkingFigs
   convolutionFigs
