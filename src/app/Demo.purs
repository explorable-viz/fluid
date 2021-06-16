module App.Demo where

import Prelude hiding (absurd)
import Data.Array (zip)
import Data.Either (Either(..))
import Data.List (List(..), (:), singleton)
import Data.Foldable (length)
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import App.Renderer (Fig, MakeSubFig, SubFig, drawFig, makeBarChart, makeEnergyTable, matrixFig)
import Bindings (Bind, Var, (↦), find, update)
import DataType (cBarChart, cCons)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Expl (Expl)
import Expr (Expr)
import Lattice (𝔹, botOf, neg)
import Module (open, openDatasetAs)
import Primitive (Slice)
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import Util (Endo, MayFail, type (×), (×), type (+), successful)
import Util.SnocList (SnocList(..), (:-), splitAt)
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

-- Example assumed to be of the form (let <defs> in expr).
type Example = {
   ρ0 :: Env 𝔹,     -- ambient env (default imports)
   ρ :: Env 𝔹,      -- local env (loaded dataset, if any, plus additional let bindings at beginning of ex)
   s :: S.Expr 𝔹    -- body of example
}

-- Expect a program to be an "example" in the sense above.
splitDefs :: Partial => Env 𝔹 -> S.Expr 𝔹 -> MayFail Example
splitDefs ρ0 s' = do
   let defs × s = unpack s'
   ρ <- desugarModuleFwd (S.Module (singleton defs)) >>= eval_module ρ0
   pure { ρ0, ρ, s }
   where unpack :: S.Expr 𝔹 -> (S.VarDefs 𝔹 + S.RecDefs 𝔹) × S.Expr 𝔹
         unpack (S.LetRec defs s)   = Right defs × s
         unpack (S.Let defs s)      = Left defs × s

type View = {
   ρ :: Env 𝔹,      -- "local" env (additional bindings introduce by "let" at beginning of ex)
   s :: S.Expr 𝔹    -- body of let
}

splitDefs2 :: Env 𝔹 -> S.Expr 𝔹 -> MayFail View
splitDefs2 ρ0 s' = unsafePartial $ do
   let defs × s = unpack s'
   ρ <- desugarModuleFwd (S.Module (singleton defs)) >>= eval_module ρ0
   pure { ρ, s }
   where unpack :: Partial => S.Expr 𝔹 -> (S.VarDefs 𝔹 + S.RecDefs 𝔹) × S.Expr 𝔹
         unpack (S.LetRec defs s)   = Right defs × s
         unpack (S.Let defs s)      = Left defs × s

type VarSpec = {
   var :: Var,
   makeFig :: MakeSubFig
}

varFig :: Partial => VarSpec × Slice (Val 𝔹) -> SubFig
varFig ({ var: x, makeFig } × uv) = makeFig { title: x, uv }

type ExampleEval = {
   e :: Expr 𝔹,
   ρ0ρ :: Env 𝔹,
   t :: Expl 𝔹,
   o :: Val 𝔹
}

evalExample :: Example -> MayFail ExampleEval
evalExample { ρ0, ρ, s } = do
   e <- desugarFwd s
   let ρ0ρ = ρ0 <> ρ
   t × o <- eval ρ0ρ e
   pure { e, ρ0ρ, t, o }

varFigs :: ExampleEval -> NeededSpec -> Env 𝔹 -> Env 𝔹 -> MayFail (Array SubFig)
varFigs q { vars, o_fig, o' } ρ ρ' = do
   let xs = _.var <$> vars
   vs <- sequence (flip find ρ <$> xs)
   vs' <- sequence (flip find ρ' <$> xs)
   unsafePartial $ pure $
      [ o_fig { title: "output", uv: o' × q.o } ] <> (varFig <$> zip vars (zip vs' vs))

type NeededSpec = {
   vars     :: Array VarSpec,    -- variables we want subfigs for
   o_fig    :: MakeSubFig,       -- for output
   o'       :: Val 𝔹             -- selection on output
}

type NeededResult = {
   ρ0'       :: Env 𝔹,            -- selection on ambient environment
   ρ'        :: Env 𝔹             -- selection on local environment
}

needed :: NeededSpec -> Example -> MayFail (NeededResult × Array SubFig)
needed spec { ρ0, ρ, s } = do
   q <- evalExample { ρ0, ρ, s }
   let ρ0ρ' × _ × _ = evalBwd spec.o' q.t
       ρ0' × ρ' = splitAt (length ρ0) ρ0ρ'
   ({ ρ0', ρ' } × _) <$> varFigs q spec q.ρ0ρ ρ0ρ'

type NeededBySpec = {
   vars     :: Array VarSpec,    -- variables we want subfigs for
   o_fig    :: MakeSubFig,       -- for output
   ρ'       :: Env 𝔹             -- selection on local env
}

neededBy :: NeededBySpec -> Example -> MayFail (Unit × Array SubFig)
neededBy { vars, o_fig, ρ' } { ρ0, ρ, s } = do
   q <- evalExample { ρ0, ρ, s }
   let o' = neg (evalFwd (neg (botOf ρ0 <> ρ')) (const true <$> q.e) true q.t)
       xs = _.var <$> vars
   (unit × _) <$> varFigs q { vars, o_fig, o' } ρ ρ'

selectOnly :: Bind (Val 𝔹) -> Endo (Env 𝔹)
selectOnly xv ρ = update (botOf ρ) xv

type FigSpec a = {
   file :: String,
   makeSubfigs :: Example -> MayFail (a × Array SubFig)
}

-- TODO: not every example should run in context of renewables data.
fig :: forall a . Partial => String -> FigSpec a -> Aff Fig
fig divId { file, makeSubfigs } = do
   ρ0 × ρ <- openDatasetAs "example/linking/renewables" "data"
   { ρ: ρ1, s: s1 } <- (successful <<< splitDefs2 (ρ0 <> ρ)) <$> open file
   let _ × subfigs = successful (makeSubfigs { ρ0, ρ: ρ <> ρ1, s: s1 })
   pure { divId , subfigs }

fig2 :: String -> String -> NeededSpec -> String -> String -> Aff Fig
fig2 divId1 divId2 spec file1 file2 = do
   let x = "data"
   ρ0 × ρ <- openDatasetAs "example/linking/renewables" x
   { ρ: ρ1, s: s1 } <- (successful <<< splitDefs2 (ρ0 <> ρ)) <$> open file1
   { ρ: ρ2, s: s2 } <- (successful <<< splitDefs2 (ρ0 <> ρ)) <$> open file2
   let { ρ0', ρ': ρρ1' } × subfigs = successful (needed spec { ρ0, ρ: ρ <> ρ1, s: s1 })
   pure { divId: divId1, subfigs }

convolutionFigs :: Partial => Aff (Array Fig)
convolutionFigs = do
   let vars = [{ var: "filter", makeFig: matrixFig }, { var: "image", makeFig: matrixFig }] :: Array VarSpec
   sequence [
      fig "fig-1" {
         file: "slicing/conv-wrap",
         makeSubfigs: needed { vars, o_fig: matrixFig, o': selectCell 2 1 5 5 }
      },
      fig "fig-2" {
         file: "slicing/conv-wrap",
         makeSubfigs: \ex -> neededBy { vars, o_fig: matrixFig, ρ': selectOnly ("filter" ↦ selectCell 1 1 3 3) ex.ρ } ex
      },
      fig "fig-3" {
         file: "slicing/conv-zero",
         makeSubfigs: needed { vars, o_fig: matrixFig, o': selectCell 2 1 5 5 }
      },
      fig "fig-4" {
         file: "slicing/conv-zero",
         makeSubfigs: \ex -> neededBy { vars, o_fig: matrixFig, ρ': selectOnly ("filter" ↦ selectCell 1 1 3 3) ex.ρ } ex
      }
   ]

linkingFigs :: Partial => Aff (Array Fig)
linkingFigs = do
   let vars = [{ var: "data", makeFig: makeEnergyTable }] :: Array VarSpec
   sequence [
      fig "table-1" {
         file: "linking/bar-chart",
         makeSubfigs: needed { vars, o_fig: makeBarChart, o': select_barChart_data (selectNth 1 (select_y)) }
      },
      fig "table-2" {
         file: "linking/bar-chart",
         makeSubfigs: needed { vars, o_fig: makeBarChart, o': select_barChart_data (selectNth 0 (select_y)) }
      }
   ]

main :: Effect Unit
main = unsafePartial $
   flip runAff_ ((<>) <$> convolutionFigs <*> linkingFigs)
   case _ of
      Left err -> log ("Open failed: " <> show err)
      Right figs ->
         sequence_ $ drawFig <$> figs
