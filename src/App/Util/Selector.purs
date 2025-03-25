module App.Util.Selector where

import Prelude hiding (absurd)

import App.Util (SelState(..), SelStates, Selection, SelectionType(..), SetSel, mergeSelStates, persist, splitSelStates)
import Bind (Var)
import Data.List (List(..), (:), (!!), updateAt)
import Data.Profunctor.Strong (first, second)
import DataType (Ctr, cBarChart, cCons, cLineChart, cLinePlot, cParagraph, cMultiView, cNil, cPair, cScatterPlot, cSome, f_bars, f_points, f_stackedBars, f_z)
import Lattice (𝔹)
import Partial.Unsafe (unsafePartial)
import Util (Endo, Setter, absurd, assert, definitely, error, (×), type (×))
import Util.Map (lookup, update)
import Util.Set ((∈))
import Val (BaseVal(..), DictRep(..), Env, Val(..), matrixGet, matrixPut)

-- Selection setters.
type SelSetter (f :: Type -> Type) (g :: Type -> Type) = Setter (f (SelStates 𝔹)) (g (SelStates 𝔹))

type Setter' b a = SetSel a -> SetSel b

type ViewSelSetter' a = a -> Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))

type ViewSelSetter a = a -> SelSetter Val Val -- convert mouse event data to view selector

fst :: SelSetter Val Val
fst = constrArg cPair 0

fst' :: Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
fst' = constrArg' cPair 0

snd :: SelSetter Val Val
snd = constrArg cPair 1

snd' :: Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
snd' = constrArg' cPair 1

some :: Setter (Val (SelStates 𝔹)) 𝔹
some = constr cSome

some' :: Setter (Selection (Val (SelState 𝔹))) 𝔹
some' = constr' cSome

multiView :: SelSetter Val Val
multiView = constrArg cMultiView 0

multiView' :: Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
multiView' = constrArg' cMultiView 0

multiViewEntry :: String -> SelSetter Val Val
multiViewEntry x = dictVal x >>> multiView

multiViewEntry' :: String -> Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
multiViewEntry' x = dictVal' x >>> multiView'

lineChart :: SelSetter Val Val
lineChart = constrArg cLineChart 0

lineChart' :: Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
lineChart' = constrArg' cLineChart 0

linePoint :: Int -> SelSetter Val Val
linePoint i = listElement i >>> dictVal f_points >>> constrArg cLinePlot 0

linePoint' :: Int -> Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
linePoint' i = listElement' i >>> dictVal' f_points >>> constrArg' cLinePlot 0

barChart :: SelSetter Val Val
barChart = constrArg cBarChart 0

barChart' :: Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
barChart' = constrArg' cBarChart 0

scatterPlot :: SelSetter Val Val
scatterPlot = constrArg cScatterPlot 0

scatterPlot' :: Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
scatterPlot' = constrArg' cScatterPlot 0

scatterPoint :: Int -> SelSetter Val Val
scatterPoint i = listElement i >>> dictVal f_points

scatterPoint' :: Int -> Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
scatterPoint' i = listElement' i >>> dictVal' f_points

barSegment :: Int -> Int -> SelSetter Val Val
barSegment i j =
   dictVal f_z >>> listElement j >>> dictVal f_bars >>> listElement i >>> dictVal f_stackedBars

barSegment' :: Int -> Int -> Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
barSegment' i j =
   dictVal' f_z >>> listElement' j >>> dictVal' f_bars >>> listElement' i >>> dictVal' f_stackedBars

paragraph :: SelSetter Val Val
paragraph = constrArg cParagraph 0

paragraph' :: Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
paragraph' = constrArg' cParagraph 0

matrixElement :: Int -> Int -> SelSetter Val Val
matrixElement i j δv (Val α (Matrix r)) = Val α $ Matrix $ matrixPut i j δv r
matrixElement _ _ _ _ = error absurd

matrixElement' :: Int -> Int -> Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
matrixElement' i j δv (Val α (Matrix r)) = (Val α $ Matrix r') × selType
   where
   r' × selType = definitely "matrixElement failed" do
      let new_r × selType' = δv (matrixGet i j r)
      pure $ matrixPut i j (const new_r) r × selType'
matrixElement' _ _ _ _ = error absurd

listElement :: Int -> SelSetter Val Val
listElement n δv = unsafePartial $ case _ of
   Val α (Constr c (v : v' : Nil)) | n == 0 && c == cCons -> Val α (Constr c (δv v : v' : Nil))
   Val α (Constr c (v : v' : Nil)) | c == cCons -> Val α (Constr c (v : listElement (n - 1) δv v' : Nil))

listElement' :: Int -> Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
listElement' n δv = unsafePartial $ case _ of
   Val α (Constr c (v : v' : Nil)) | n == 0 && c == cCons -> let new_v × selType = δv v in Val α (Constr c (new_v : v' : Nil)) × selType
   Val α (Constr c (v : v' : Nil)) | c == cCons -> let new_v × selType = listElement' (n - 1) δv v' in Val α (Constr c (v : new_v : Nil)) × selType

constrArg :: Ctr -> Int -> SelSetter Val Val
constrArg c n δv = unsafePartial $ case _ of
   Val α (Constr c' us) | c == c' ->
      Val α (Constr c us')
      where
      us' = definitely "constrArg out of bounds" do
         u1 <- us !! n
         updateAt n (δv u1) us

constrArg' :: Ctr -> Int -> Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
constrArg' c n δv = unsafePartial $ case _ of
   Val α (Constr c' us) | c == c' ->
      Val α (Constr c us') × selType
      where
      us' × selType = first (definitely "constrArg out of bounds") $ definitely "selection failed" do
         u1 <- us !! n
         let new_u × selType' = δv u1
         pure $ updateAt n new_u us × selType'

constr :: Ctr -> Setter (Val (SelStates 𝔹)) 𝔹
constr c' δα = unsafePartial $ case _ of
   Val α (Constr c vs) | c == c' -> Val (persist δα α) (Constr c vs)

constr' :: Ctr -> Setter (Selection (Val (SelState 𝔹))) 𝔹
constr' c' δα = lift (constr c' δα)

dict :: Setter (Val (SelStates 𝔹)) 𝔹
dict δα = unsafePartial $ case _ of
   Val α (Dictionary d) -> Val (persist δα α) (Dictionary d)

dict' :: Setter' (Val (SelState 𝔹)) 𝔹
dict' δα = unsafePartial $ case _ of
   Val α (Dictionary d) -> let α' × selType = persist' δα α in Val α' (Dictionary d) × selType

dictKey :: String -> Setter (Val (SelStates 𝔹)) 𝔹
dictKey s δα = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> Val α $ Dictionary $ DictRep $ update (first $ persist δα) s d

dictKey' :: String -> Setter' (Val (SelState 𝔹)) (SelState 𝔹)
dictKey' s δα = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> (Val α $ Dictionary $ DictRep d') × selType
      where
      d' × selType = definitely "dictKey failed" do
         α × _ <- lookup s d
         let new_u × selType' = δα α
         pure $ update (first $ const new_u) s d × selType'

dictVal :: String -> SelSetter Val Val
dictVal s δv = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> Val α $ Dictionary $ DictRep $ update (second δv) s d

dictVal' :: String -> Setter' (Val (SelState 𝔹)) (Val (SelState 𝔹))
dictVal' s δv = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> (Val α $ Dictionary $ DictRep d') × selType
      where
      d' × selType = definitely "dictVal failed" do
         _ × u <- lookup s d
         let new_u × selType' = δv u
         pure $ update (second $ const new_u) s d × selType'

envVal :: Var -> Setter (Env (SelStates 𝔹)) (Val (SelStates 𝔹))
envVal x δv γ =
   assert (x ∈ γ) $ update δv x γ

envVal' :: Var -> Setter' (Env (SelState 𝔹)) (Val (SelState 𝔹))
envVal' x δv γ =
   assert (x ∈ γ) $ definitely "envVal' failed" do
      v <- lookup x γ
      let new_v × selType = δv v
      pure $ update (const new_v) x γ × selType

listCell :: Int -> Setter (Val (SelStates 𝔹)) 𝔹
listCell n δα = unsafePartial $ case _ of
   Val α (Constr c Nil) | n == 0 && c == cNil -> Val (persist δα α) (Constr c Nil)
   Val α (Constr c (v : v' : Nil)) | c == cCons ->
      if n == 0 then Val (persist δα α) (Constr c (v : v' : Nil))
      else Val α (Constr c (v : listCell (n - 1) δα v' : Nil))

listCell' :: Int -> Setter' (Val (SelState 𝔹)) 𝔹
listCell' n δα = unsafePartial $ case _ of
   Val α (Constr c Nil) | n == 0 && c == cNil -> let new_α × selType = (persist' δα α) in Val new_α (Constr c Nil) × selType
   Val α (Constr c (v : v' : Nil)) | c == cCons ->
      if n == 0 then let new_α × selType = persist' δα α in Val new_α (Constr c (v : v' : Nil)) × selType
      else let new_v × selType = listCell' (n - 1) δα v' in Val α (Constr c (v : new_v : Nil)) × selType

lift :: forall f. Functor f => Apply f => Endo (f (SelStates 𝔹)) -> Endo (Selection (f (SelState 𝔹)))
lift δv = splitSelStates <<< δv <<< mergeSelStates

persist' :: forall a. Setter' (SelState a) a
persist' δα = mapδ
   where
   mapδ :: SelState a -> SelState a × SelectionType
   mapδ Inert = Inert × Unselectable
   mapδ (Reactive s) = let new_s × selType = δα s in Reactive new_s × selType
