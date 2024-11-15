module App.Util.Selector where

import Prelude hiding (absurd)

import App.Util (SelState, persist)
import Bind (Var)
import Data.List (List(..), (:), (!!), updateAt)
import Data.Profunctor.Strong (first, second)
import DataType (Ctr, cBarChart, cCons, cLineChart, cLinePlot, cLinkedText, cMultiView, cNil, cPair, cScatterPlot, cSome, f_bars, f_points, f_stackedBars, f_z)
import Lattice (𝔹)
import Partial.Unsafe (unsafePartial)
import Util (Setter, absurd, assert, definitely, error)
import Util.Map (update)
import Util.Set ((∈))
import Val (BaseVal(..), DictRep(..), Val(..), matrixPut, Env)

-- Selection setters.
type SelSetter (f :: Type -> Type) (g :: Type -> Type) = Setter (f (SelState 𝔹)) (g (SelState 𝔹))
type ViewSelSetter a = a -> SelSetter Val Val -- convert mouse event data to view selector

fst :: SelSetter Val Val
fst = constrArg cPair 0

snd :: SelSetter Val Val
snd = constrArg cPair 1

some :: Setter (Val (SelState 𝔹)) 𝔹
some = constr cSome

multiView :: SelSetter Val Val
multiView = constrArg cMultiView 0

multiViewEntry :: String -> SelSetter Val Val
multiViewEntry x = dictVal x >>> multiView

lineChart :: SelSetter Val Val
lineChart = constrArg cLineChart 0

linePoint :: Int -> SelSetter Val Val
linePoint i = listElement i >>> dictVal f_points >>> constrArg cLinePlot 0

barChart :: SelSetter Val Val
barChart = constrArg cBarChart 0

scatterPlot :: SelSetter Val Val
scatterPlot = constrArg cScatterPlot 0

scatterPoint :: Int -> SelSetter Val Val
scatterPoint i = listElement i >>> dictVal f_points

barSegment :: Int -> Int -> SelSetter Val Val
barSegment i j =
   dictVal f_z >>> listElement j >>> dictVal f_bars >>> listElement i >>> dictVal f_stackedBars

linkedText :: SelSetter Val Val
linkedText = constrArg cLinkedText 0

matrixElement :: Int -> Int -> SelSetter Val Val
matrixElement i j δv (Val α (Matrix r)) = Val α $ Matrix $ matrixPut i j δv r
matrixElement _ _ _ _ = error absurd

listElement :: Int -> SelSetter Val Val
listElement n δv = unsafePartial $ case _ of
   Val α (Constr c (v : v' : Nil)) | n == 0 && c == cCons -> Val α (Constr c (δv v : v' : Nil))
   Val α (Constr c (v : v' : Nil)) | c == cCons -> Val α (Constr c (v : listElement (n - 1) δv v' : Nil))

constrArg :: Ctr -> Int -> SelSetter Val Val
constrArg c n δv = unsafePartial $ case _ of
   Val α (Constr c' us) | c == c' ->
      Val α (Constr c us')
      where
      us' = definitely "constrArg out of bounds" do
         u1 <- us !! n
         updateAt n (δv u1) us

constr :: Ctr -> Setter (Val (SelState 𝔹)) 𝔹
constr c' δα = unsafePartial $ case _ of
   Val α (Constr c vs) | c == c' -> Val (persist δα α) (Constr c vs)

dict :: Setter (Val (SelState 𝔹)) 𝔹
dict δα = unsafePartial $ case _ of
   Val α (Dictionary d) -> Val (persist δα α) (Dictionary d)

dictKey :: String -> Setter (Val (SelState 𝔹)) 𝔹
dictKey s δα = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> Val α $ Dictionary $ DictRep $ update (first $ persist δα) s d

dictVal :: String -> SelSetter Val Val
dictVal s δv = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> Val α $ Dictionary $ DictRep $ update (second δv) s d

envVal :: Var -> Setter (Env (SelState 𝔹)) (Val (SelState 𝔹))
envVal x δv γ =
   assert (x ∈ γ) $ update δv x γ

listCell :: Int -> Setter (Val (SelState 𝔹)) 𝔹
listCell n δα = unsafePartial $ case _ of
   Val α (Constr c Nil) | n == 0 && c == cNil -> Val (persist δα α) (Constr c Nil)
   Val α (Constr c (v : v' : Nil)) | c == cCons ->
      if n == 0 then Val (persist δα α) (Constr c (v : v' : Nil))
      else Val α (Constr c (v : listCell (n - 1) δα v' : Nil))
