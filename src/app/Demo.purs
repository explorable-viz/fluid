module App.Demo where

import Prelude hiding (absurd)
import Data.Either (Either(..))
import Data.List (List(..), (:), singleton)
import Data.Foldable (length)
import Data.Maybe
import Data.Traversable (sequence, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import DataType
import App.Renderer (Fig, MakeSubFig, SubFig, drawFig, makeBarChart, makeEnergyTable, makeLineChart, matrixFig)
import Bindings (Bind, Var, (↦), find, update)
import DesugarFwd (desugarFwd, desugarModuleFwd)
import DesugarBwd (desugarBwd)
import Eval (eval, eval_module)
import EvalBwd (evalBwd)
import EvalFwd (evalFwd)
import Expl (Expl)
import Expr (Expr)
import Lattice (𝔹, botOf, neg)
import Module (File(..), open, openDatasetAs)
import Primitive (Slice)
import SExpr (Expr(..), Module(..), RecDefs, VarDefs) as S
import Test.Util (LinkConfig, doLink, selectBarChart_data, selectCell, selectNthCons, selectNth, select_y, selectTree)
import Util (Endo, MayFail, type (×), (×), type (+), successful)
import Util.SnocList (splitAt)
import Val (Env, Val(..))
import Pretty (prettyP)

type Example = {
   ρ0 :: Env 𝔹,     -- ambient env (default imports)
   ρ :: Env 𝔹,      -- local env (loaded dataset, if any, plus additional let bindings at beginning of ex)
   s :: S.Expr 𝔹    -- body of example
}

-- Example assumed to be of the form (let <defs> in expr).
type View = {
   ρ :: Env 𝔹,      -- local env (additional let bindings at beginning of ex)
   s :: S.Expr 𝔹    -- body of example
}

-- Interpret a program as a "view" in the sense above. TODO: generalise to sequence of let/let recs, rather than one.
splitDefs :: Env 𝔹 -> S.Expr 𝔹 -> MayFail View
splitDefs ρ0 s = unsafePartial $ do
   let defs × s = fromJust (unpack s)
   ρ0ρ <- desugarModuleFwd (S.Module defs) >>= eval_module ρ0
   let _ × ρ = splitAt (length ρ0ρ - length ρ0) ρ0ρ
   pure { ρ, s }
   where unpack :: Partial => S.Expr 𝔹 -> Maybe (List (S.VarDefs 𝔹 + S.RecDefs 𝔹) × S.Expr 𝔹)
         unpack (S.LetRec defs s')   = do
            case unpack s' of
               Nothing           -> Just ((Right defs:Nil) × s')
               Just (defs' × s'') -> Just (((Right defs) : defs') × s'')
         unpack (S.Let defs s') = do
            case unpack s' of
               Nothing           -> Just ((Left defs:Nil) × s')
               Just (defs' × s'') -> Just (((Left defs) : defs') × s'')
         unpack s' = Nothing

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

varFig' :: VarSpec -> Slice (Env 𝔹) -> MayFail SubFig
varFig' spec (ρ' × ρ) = do
   v <- find spec.var ρ
   v' <- find spec.var ρ'
   unsafePartial $ pure $ varFig (spec × (v' × v))

valFigs :: ExampleEval -> NeedsSpec -> Slice (Env 𝔹) -> MayFail (Array SubFig)
valFigs q { vars, o_fig, o' } (ρ' × ρ) = do
   figs <- sequence (flip varFig' (ρ' × ρ) <$> vars)
   unsafePartial $ pure $
      [ o_fig { title: "output", uv: o' × q.o } ] <> figs

type NeedsSpec = {
   vars  :: Array VarSpec, -- variables we want subfigs for
   o_fig :: MakeSubFig,    -- for output
   o'    :: Val 𝔹          -- selection on output
}

type NeedsResult = {
   ρ0'   :: Env 𝔹,         -- selection on ambient environment
   ρ'    :: Env 𝔹          -- selection on local environment
}

needs :: NeedsSpec -> Example -> MayFail (NeedsResult × Array SubFig)
needs spec { ρ0, ρ, s } = do
   q <- evalExample { ρ0, ρ, s }
   let ρ0ρ' × _ × _ = evalBwd spec.o' q.t
       ρ0' × ρ' = splitAt (length ρ) ρ0ρ'
   ({ ρ0', ρ' } × _) <$> valFigs q spec (ρ0ρ' × q.ρ0ρ)

type NeededBySpec = {
   vars     :: Array VarSpec,    -- variables we want subfigs for
   o_fig    :: MakeSubFig,       -- for output
   ρ'       :: Env 𝔹             -- selection on local env
}

neededBy :: NeededBySpec -> Example -> MayFail (Unit × Array SubFig)
neededBy { vars, o_fig, ρ' } { ρ0, ρ, s } = do
   q <- evalExample { ρ0, ρ, s }
   let o' = neg (evalFwd (neg (botOf ρ0 <> ρ')) (const true <$> q.e) true q.t)
   (unit × _) <$> valFigs q { vars, o_fig, o' } (ρ' × ρ)

selectOnly :: Bind (Val 𝔹) -> Endo (Env 𝔹)
selectOnly xv ρ = update (botOf ρ) xv

type FigSpec a = {
   file :: File,
   makeSubfigs :: Example -> MayFail (a × Array SubFig)
}

-- TODO: not every example should run with this dataset.
fig :: forall a . String -> FigSpec a -> Aff Fig
fig divId { file, makeSubfigs } = do
   ρ0 × ρ <- openDatasetAs (File "example/linking/renewables") "data"
   { ρ: ρ1, s: s1 } <- (successful <<< splitDefs (ρ0 <> ρ)) <$> open file
   let _ × subfigs = successful (makeSubfigs { ρ0, ρ: ρ <> ρ1, s: s1 })
   pure { divId , subfigs }

srcFig :: Val 𝔹 -> File -> Aff String
srcFig o' file = do
   -- ρ0 : Default imports, ρ : environment referred to as variable "data"
   ρ0 × ρ <- openDatasetAs (File "example/linking/renewables") "data"
   -- ρ1 : Local environment of program, s1: Main body of program (without bindings)
   s1  <- (open file) -- SExpr Bool
   -- e1 : Desugared program
   let e = successful (desugarFwd s1)
   -- pure $ (prettyP $ ρ <> ρ1)
   -- ρ0ρ : Default imports + data imports + local environment
   let ρ0ρ = ρ0 <> ρ
   -- -- t1 : Trace from evaluating program, o1 : Output from evaluating program
   let t × o = successful (eval ρ0ρ e)
   let -- ρ0ρ' : environment from bwd slicing over trace t with selected output o'
       ρ0ρ' × e' × _ = evalBwd o' t
       s1' = desugarBwd e' s1
      --  v  = successful (find "countryData" ρ0ρ)
      --  v' = successful (find "countryData" ρ0ρ')
   pure $ (prettyP o <> "\n" <> prettyP s1')

linkFig :: String -> LinkConfig -> MakeSubFig -> MakeSubFig -> MakeSubFig -> Aff Fig
linkFig divId config o1_fig o2_fig data_fig = do
   link <- doLink config
   pure { divId, subfigs: [
      o1_fig { title: "primary view", uv: config.v1_sel × link.v1 },
      o2_fig { title: "linked view", uv: link.v2 },
      data_fig { title: "common data", uv: link.data_sel }
   ] }

convolutionFigs :: Partial => Aff (Array Fig)
convolutionFigs = do
   let vars = [{ var: "filter", makeFig: matrixFig }, { var: "image", makeFig: matrixFig }] :: Array VarSpec
   sequence [
      fig "fig-1" {
         file: File "slicing/conv-wrap",
         makeSubfigs: needs { vars, o_fig: matrixFig, o': selectCell 2 1 5 5 }
      },
      fig "fig-2" {
         file: File "slicing/conv-wrap",
         makeSubfigs: \ex -> neededBy { vars, o_fig: matrixFig, ρ': selectOnly ("filter" ↦ selectCell 1 1 3 3) ex.ρ } ex
      },
      fig "fig-3" {
         file: File "slicing/conv-zero",
         makeSubfigs: needs { vars, o_fig: matrixFig, o': selectCell 2 1 5 5 }
      },
      fig "fig-4" {
         file: File "slicing/conv-zero",
         makeSubfigs: \ex -> neededBy { vars, o_fig: matrixFig, ρ': selectOnly ("filter" ↦ selectCell 1 1 3 3) ex.ρ } ex
      }
   ]

linkingFigs :: Partial => Aff (Array Fig)
linkingFigs = do
   let vars = [{ var: "data", makeFig: makeEnergyTable }] :: Array VarSpec
   sequence [
      linkFig "fig-5" {
         file1: File "bar-chart",
         file2: File "line-chart",
         dataFile: File "renewables",
         dataVar: "data",
         v1_sel: selectBarChart_data (selectNth 1 (select_y))
       } makeBarChart makeLineChart makeEnergyTable,
      fig "fig-6" {
         file: File "linking/bar-chart",
         makeSubfigs: needs { vars, o_fig: makeBarChart, o': selectBarChart_data (selectNth 0 (select_y)) }
      }
   ]

testTree :: Effect Unit
testTree = do
   let o' = selectTree ((true:Nil) :: List Boolean)
   unsafePartial $
      flip runAff_ (srcFig o' (File "slicing/tree"))
      case _ of
         Left err -> log $ show err
         Right str -> log $ str

testListComp :: Effect Unit
testListComp = do
   let o' = selectNth 0 (Constr true (Ctr "Pair") (Str true "China":Hole false:Nil))
   unsafePartial $
      flip runAff_ (srcFig o' (File "slicing/list-comp"))
      case _ of
         Left err -> log $ show err
         Right str -> log $ prettyP o' <> "\n" <> str

main :: Effect Unit
main = do
   testListComp
   -- unsafePartial $
   --    flip runAff_ ((<>) <$> convolutionFigs <*> linkingFigs)
   --    case _ of
   --       Left err -> log $ show err
   --       Right figs ->
   --          sequence_ $ drawFig <$> figs
