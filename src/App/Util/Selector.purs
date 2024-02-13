module App.Util.Selector where

import Prelude hiding (absurd)

import Bind (Var)
import Data.List (List(..), (:), (!!), updateAt)
import Data.Profunctor.Strong (first, second)
import DataType (Ctr, cBarChart, cBubbleChart, cCons, cLineChart, cLinePlot, cMultiPlot, cNil, cPair, cScatterPlot, cSome, f_bars, f_data, f_z)
import Lattice (𝔹, neg)
import Partial.Unsafe (unsafePartial)
import Test.Util (Selector)
import Util (Endo, absurd, assert, definitely', error)
import Util.Map (update)
import Util.Set ((∈))
import Val (BaseVal(..), DictRep(..), Val(..), matrixPut, Env)

-- Selection helpers. TODO: turn into lenses/prisms.
fst :: Endo (Selector Val)
fst = constrArg cPair 0

snd :: Endo (Selector Val)
snd = constrArg cPair 1

some :: Endo 𝔹 -> Selector Val
some = constr cSome

bubbleChart :: Endo (Selector Val)
bubbleChart = constrArg cBubbleChart 0

multiPlot :: Endo (Selector Val)
multiPlot = constrArg cMultiPlot 0

multiPlotEntry :: String -> Endo (Selector Val)
multiPlotEntry x = dictVal x >>> multiPlot

lineChart :: Endo (Selector Val)
lineChart = constrArg cLineChart 0

linePoint :: Int -> Endo (Selector Val)
linePoint i = constrArg cLinePlot 0 <<< field f_data <<< listElement i

barChart :: Endo (Selector Val)
barChart = constrArg cBarChart 0

scatterPlot :: Endo (Selector Val)
scatterPlot = constrArg cScatterPlot 0

scatterPoint :: Int -> Endo (Selector Val)
scatterPoint i = field f_data <<< listElement i

barSegment :: Int -> Int -> Selector Val
barSegment i j =
   field f_data (listElement i (field f_bars (listElement j (field f_z neg))))

matrixElement :: Int -> Int -> Endo (Selector Val)
matrixElement i j δv (Val α (Matrix r)) = Val α $ Matrix $ matrixPut i j δv r
matrixElement _ _ _ _ = error absurd

listElement :: Int -> Endo (Selector Val)
listElement n δv = unsafePartial $ case _ of
   Val α (Constr c (v : v' : Nil)) | n == 0 && c == cCons -> Val α (Constr c (δv v : v' : Nil))
   Val α (Constr c (v : v' : Nil)) | c == cCons -> Val α (Constr c (v : listElement (n - 1) δv v' : Nil))

field :: Var -> Endo (Selector Val)
field f δv = unsafePartial $ case _ of
   Val α (Record r) -> Val α $ Record $ update δv f r

constrArg :: Ctr -> Int -> Endo (Selector Val)
constrArg c n δv = unsafePartial $ case _ of
   Val α (Constr c' us) | c == c' ->
      Val α (Constr c us')
      where
      us' = definitely' do
         u1 <- us !! n
         updateAt n (δv u1) us

constr :: Ctr -> Endo 𝔹 -> Selector Val
constr c' δα = unsafePartial $ case _ of
   Val α (Constr c vs) | c == c' -> Val (δα α) (Constr c vs)

dict :: Endo 𝔹 -> Selector Val
dict δα = unsafePartial $ case _ of
   Val α (Dictionary d) -> Val (δα α) (Dictionary d)

dictKey :: String -> Endo 𝔹 -> Selector Val
dictKey s δα = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> Val α $ Dictionary $ DictRep $ update (first δα) s d

dictVal :: String -> Endo (Selector Val)
dictVal s δv = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> Val α $ Dictionary $ DictRep $ update (second δv) s d

envVal :: Var -> Selector Val -> Selector Env
envVal x δv γ =
   assert (x ∈ γ) $ update δv x γ

listCell :: Int -> Endo 𝔹 -> Selector Val
listCell n δα = unsafePartial $ case _ of
   Val α (Constr c Nil) | n == 0 && c == cNil -> Val (δα α) (Constr c Nil)
   Val α (Constr c (v : v' : Nil)) | n == 0 && c == cCons -> Val (δα α) (Constr c (v : v' : Nil))
   Val α (Constr c (v : v' : Nil)) | c == cCons -> Val α (Constr c (v : listCell (n - 1) δα v' : Nil))
