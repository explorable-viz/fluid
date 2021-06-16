module App.Demo where

import Prelude hiding (absurd)
import Data.Array (zip)
import Data.Either (Either(..))
import Data.List (List(..), (:), singleton)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import App.Renderer (Fig, MakeFig, drawFigs, makeBarChart, makeEnergyTable, matrixFig)
import Bindings (Bind, Var, (↦), find, update)
import DataType (cBarChart, cCons)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Expl (Expl)
import Expr (Expr)
import Lattice (𝔹, botOf, neg)
import Module (openDatasetAs, openIn)
import Primitive (Slice)
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import Test.Util (openFileWithDataset)
import Util (Endo, MayFail, type (×), (×), type (+), error, successful)
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
   ρ0 :: Env 𝔹,     -- ambient environment, including any dataset loaded
   ρ :: Env 𝔹,      -- "local" env (additional bindings introduce by "let" at beginning of ex)
   s :: S.Expr 𝔹    -- body of let
}

type View = {
   ρ :: Env 𝔹,      -- "local" env (additional bindings introduce by "let" at beginning of ex)
   s :: S.Expr 𝔹    -- body of let
}

type VarSpec = {
   var :: Var,
   fig :: MakeFig
}

type NeededSpec = {
   x_figs   :: Array VarSpec,    -- one for each variable we want a figure for
   o_fig    :: MakeFig,          -- for output
   o'       :: Val 𝔹             -- selection on output
}

type NeededBySpec = {
   x_figs   :: Array VarSpec,    -- one for each variable we want a figure for
   o_fig    :: MakeFig,          -- for output
   ρ'       :: Env 𝔹             -- selection on local env
}

-- Expect a program to be an "example" as defined above.
splitDefs :: Partial => Env 𝔹 -> S.Expr 𝔹 -> MayFail Example
splitDefs ρ0 s' = do
   let defs × s = unpack s'
   ρ <- desugarModuleFwd (S.Module (singleton defs)) >>= eval_module ρ0
   pure { ρ0, ρ, s }
   where unpack :: S.Expr 𝔹 -> (S.VarDefs 𝔹 + S.RecDefs 𝔹) × S.Expr 𝔹
         unpack (S.LetRec defs s)   = Right defs × s
         unpack (S.Let defs s)      = Left defs × s

splitDefs2 :: Env 𝔹 -> S.Expr 𝔹 -> MayFail View
splitDefs2 ρ0 s' = unsafePartial $ do
   let defs × s = unpack s'
   ρ <- desugarModuleFwd (S.Module (singleton defs)) >>= eval_module ρ0
   pure { ρ, s }
   where unpack :: Partial => S.Expr 𝔹 -> (S.VarDefs 𝔹 + S.RecDefs 𝔹) × S.Expr 𝔹
         unpack (S.LetRec defs s)   = Right defs × s
         unpack (S.Let defs s)      = Left defs × s

varFig :: Partial => VarSpec × Slice (Val 𝔹) -> Fig
varFig ({var: x, fig} × uv) = fig { title: x, uv }

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

varFigs :: ExampleEval -> NeededSpec -> Env 𝔹 -> Env 𝔹 -> MayFail (Array Fig)
varFigs q { x_figs, o_fig, o' } ρ ρ' = do
   let xs = _.var <$> x_figs
   vs <- sequence (flip find ρ <$> xs)
   vs' <- sequence (flip find ρ' <$> xs)
   unsafePartial $ pure $
      [ o_fig { title: "output", uv: o' × q.o } ] <> (varFig <$> zip x_figs (zip vs' vs))

needed :: NeededSpec -> Example -> MayFail (Env 𝔹 × Array Fig)
needed spec { ρ0, ρ, s } = do
   q <- evalExample { ρ0, ρ, s }
   let ρ0ρ' × _ × _ = evalBwd spec.o' q.t
   (ρ0ρ' × _) <$> varFigs q spec q.ρ0ρ ρ0ρ'

neededBy :: NeededBySpec -> Example -> MayFail (ExampleEval × Array Fig)
neededBy { x_figs, o_fig, ρ' } { ρ0, ρ, s } = do
   q <- evalExample { ρ0, ρ, s }
   let o' = neg (evalFwd (neg (botOf ρ0 <> ρ')) (const true <$> q.e) true q.t)
       xs = _.var <$> x_figs
   (q × _) <$> varFigs q { x_figs, o_fig, o' } ρ ρ'

selectOnly :: Bind (Val 𝔹) -> Endo (Env 𝔹)
selectOnly xv ρ = update (botOf ρ) xv

type FigsSpec a = {
   file :: String,
   makeFigs :: Example -> MayFail (a × Array Fig)
}

-- TODO: not every example should run in context of renewables data.
figs :: forall a . Partial => String -> FigsSpec a -> Effect Unit
figs divId { file, makeFigs } =
   flip runAff_ (openFileWithDataset "example/linking/renewables" file)
   case _ of
      Left err -> log ("Open failed: " <> show err)
      Right (ρ × s) ->
         let _ × figs = successful (splitDefs ρ s >>= makeFigs) in
         drawFigs { divId, figs }

figs2 :: String -> String -> NeededSpec -> String -> String -> Effect Unit
figs2 divId1 divId2 spec file1 file2 =
   flip runAff_ (do
      ρ0 × ρ <- openDatasetAs "example/linking/renewables" "data"
      let ρ0' = ρ0 <> ρ
      view1 <- (successful <<< splitDefs2 ρ0') <$> openIn file1 ρ0'
      view2 <- (successful <<< splitDefs2 ρ0') <$> openIn file2 ρ0'
      pure $ ρ0' × view1 × view2 :: Aff (Env 𝔹 × View × View)
   )
   case _ of
      Left err -> log ("Open failed: " <> show err)
      Right (ρ0 × { ρ: ρ1, s: s1 } × { ρ: ρ2, s: s2 }) -> do
         let q × figs1 = successful (needed spec { ρ0, ρ: ρ1, s: s1 })
         drawFigs { divId: divId1, figs: figs1 }

convolutionFigs :: Partial => Effect Unit
convolutionFigs = do
   let x_figs = [{ var: "filter", fig: matrixFig }, { var: "image", fig: matrixFig }] :: Array VarSpec
   figs "fig-1" {
      file: "slicing/conv-wrap",
      makeFigs: needed { x_figs, o_fig: matrixFig, o': selectCell 2 1 5 5 }
   }

   figs "fig-2" {
      file: "slicing/conv-wrap",
      makeFigs: \ex -> neededBy { x_figs, o_fig: matrixFig, ρ': selectOnly ("filter" ↦ selectCell 1 1 3 3) ex.ρ } ex
   }

   figs "fig-3" {
      file: "slicing/conv-zero",
      makeFigs: needed { x_figs, o_fig: matrixFig, o': selectCell 2 1 5 5 }
   }

   figs "fig-4" {
      file: "slicing/conv-zero",
      makeFigs: \ex -> neededBy { x_figs, o_fig: matrixFig, ρ': selectOnly ("filter" ↦ selectCell 1 1 3 3) ex.ρ } ex
   }

linkingFigs :: Partial => Effect Unit
linkingFigs = do
   let x_figs = [{ var: "data", fig: makeEnergyTable }] :: Array VarSpec
   figs "table-1" {
      file: "linking/bar-chart",
      makeFigs: needed { x_figs, o_fig: makeBarChart, o': select_barChart_data (selectNth 1 (select_y)) }
   }
   figs "table-2" {
      file: "linking/bar-chart",
      makeFigs: needed { x_figs, o_fig: makeBarChart, o': select_barChart_data (selectNth 0 (select_y)) }
   }

main :: Effect Unit
main = unsafePartial $ do
   convolutionFigs
   linkingFigs
