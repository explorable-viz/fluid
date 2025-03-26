module App.Util.Selector where

import Prelude hiding (absurd)

import App.Util (SelState(..), SelStates, SelectionType(..), SetSel, persist)
import Bind (Var)
import Data.List (List(..), (:), (!!), updateAt)
import Data.Maybe (fromJust)
import Data.Profunctor.Strong (first, second)
import DataType (Ctr, cBarChart, cCons, cLineChart, cLinePlot, cParagraph, cMultiView, cNil, cPair, cScatterPlot, cSome, f_bars, f_points, f_stackedBars, f_z)
import Lattice (𝔹)
import Partial.Unsafe (unsafePartial)
import Util (Setter, absurd, assert, definitely, error, (×))
import Util.Map (get, insert, lookup, update)
import Util.Set ((∈))
import Val (BaseVal(..), DictRep(..), Env, Val(..), matrixGet, matrixPut)

-- Selection setters.
type SelSetter (f :: Type -> Type) (g :: Type -> Type) = Setter (f (SelStates 𝔹)) (g (SelStates 𝔹))

type Setter' b a = SetSel a -> SetSel b

type SelSetter' g f = Setter' (f (SelStates 𝔹)) (g (SelStates 𝔹))

type ViewSelSetter' a = a -> SelSetter' Val Val

type ViewSelSetter a = a -> SelSetter Val Val -- convert mouse event data to view selector

-- TODO: rename
persist' :: forall a. Setter' (SelState a) a
persist' _ Inert = Inert × Unselectable
persist' δα (Reactive s) = first Reactive (δα s)

fst :: SelSetter Val Val
fst = constrArg cPair 0

fst' :: SelSetter' Val Val
fst' = constrArg' cPair 0

snd :: SelSetter Val Val
snd = constrArg cPair 1

snd' :: SelSetter' Val Val
snd' = constrArg' cPair 1

some :: Setter (Val (SelStates 𝔹)) 𝔹
some = constr cSome

multiView :: SelSetter Val Val
multiView = constrArg cMultiView 0

multiView' :: SelSetter' Val Val
multiView' = constrArg' cMultiView 0

multiViewEntry :: String -> SelSetter Val Val
multiViewEntry x = dictVal x >>> multiView

multiViewEntry' :: String -> SelSetter' Val Val
multiViewEntry' x = dictVal' x >>> multiView'

lineChart :: SelSetter Val Val
lineChart = constrArg cLineChart 0

lineChart' :: SelSetter' Val Val
lineChart' = constrArg' cLineChart 0

linePoint :: Int -> SelSetter Val Val
linePoint i = listElement i >>> dictVal f_points >>> constrArg cLinePlot 0

linePoint' :: Int -> SelSetter' Val Val
linePoint' i = listElement' i >>> dictVal' f_points >>> constrArg' cLinePlot 0

barChart :: SelSetter Val Val
barChart = constrArg cBarChart 0

barChart' :: SelSetter' Val Val
barChart' = constrArg' cBarChart 0

scatterPlot :: SelSetter Val Val
scatterPlot = constrArg cScatterPlot 0

scatterPlot' :: SelSetter' Val Val
scatterPlot' = constrArg' cScatterPlot 0

scatterPoint :: Int -> SelSetter Val Val
scatterPoint i = listElement i >>> dictVal f_points

scatterPoint' :: Int -> Setter' (Val (SelStates 𝔹)) (Val (SelStates 𝔹))
scatterPoint' i = listElement' i >>> dictVal' f_points

barSegment :: Int -> Int -> SelSetter Val Val
barSegment i j =
   dictVal f_z >>> listElement j >>> dictVal f_bars >>> listElement i >>> dictVal f_stackedBars

barSegment' :: Int -> Int -> SelSetter' Val Val
barSegment' i j =
   dictVal' f_z >>> listElement' j >>> dictVal' f_bars >>> listElement' i >>> dictVal' f_stackedBars

paragraph :: SelSetter Val Val
paragraph = constrArg cParagraph 0

paragraph' :: SelSetter' Val Val
paragraph' = constrArg' cParagraph 0

matrixElement :: Int -> Int -> SelSetter Val Val
matrixElement i j δv (Val α (Matrix r)) = Val α $ Matrix $ matrixPut i j δv r
matrixElement _ _ _ _ = error absurd

matrixElement' :: Int -> Int -> SelSetter' Val Val
matrixElement' i j δv (Val α (Matrix r)) =
   first (\r' -> Val α $ Matrix $ matrixPut i j (const r') r) (δv (matrixGet i j r))
matrixElement' _ _ _ _ = error absurd

listElement :: Int -> SelSetter Val Val
listElement n δv = unsafePartial $ case _ of
   Val α (Constr c (v : v' : Nil)) | n == 0 && c == cCons -> Val α (Constr c (δv v : v' : Nil))
   Val α (Constr c (v : v' : Nil)) | c == cCons -> Val α (Constr c (v : listElement (n - 1) δv v' : Nil))

listElement' :: Int -> SelSetter' Val Val
listElement' n δv = unsafePartial $ case _ of
   Val α (Constr c (v : u : Nil)) | n == 0 && c == cCons ->
      first (\v' -> Val α (Constr c (v' : u : Nil))) (δv v)
   Val α (Constr c (v : u : Nil)) | c == cCons ->
      first (\u' -> Val α (Constr c (v : u' : Nil))) (listElement' (n - 1) δv u)

constrArg :: Ctr -> Int -> SelSetter Val Val
constrArg c n δv = unsafePartial $ case _ of
   Val α (Constr c' us) | c == c' ->
      Val α (Constr c us')
      where
      us' = definitely "constrArg out of bounds" do
         u1 <- us !! n
         updateAt n (δv u1) us

constrArg' :: Ctr -> Int -> SelSetter' Val Val
constrArg' c n δv = unsafePartial $ case _ of
   Val α (Constr c' us) | c == c' ->
      Val α (Constr c' us') × selType
      where
      us' × selType = definitely "constrArg out of bounds" $
         first (\u' -> fromJust (updateAt n u' us)) <$> δv <$> (us !! n)

constr :: Ctr -> Setter (Val (SelStates 𝔹)) 𝔹
constr c' δα = unsafePartial $ case _ of
   Val α (Constr c vs) | c == c' -> Val (persist δα α) (Constr c vs)

dict :: Setter (Val (SelStates 𝔹)) 𝔹
dict δα = unsafePartial $ case _ of
   Val α (Dictionary d) -> Val (persist δα α) (Dictionary d)

dict' :: Setter' (Val (SelState 𝔹)) 𝔹
dict' δα = unsafePartial $ case _ of
   Val α (Dictionary d) -> first (\α' -> Val α' (Dictionary d)) (persist' δα α)

dictKey :: String -> Setter (Val (SelStates 𝔹)) 𝔹
dictKey s δα = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> Val α $ Dictionary $ DictRep $ update (first $ persist δα) s d

-- TODO: cleanup
dictKey' :: String -> Setter' (Val (SelState 𝔹)) (SelState 𝔹)
dictKey' s δα = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) ->
      (Val α $ Dictionary $ DictRep d') × selType
      where
      d' × selType =
         first (\α' -> insert s (α' × v) d) (δα α)
         where
         α × v = fromJust (lookup s d)

dictVal :: String -> SelSetter Val Val
dictVal s δv = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> Val α $ Dictionary $ DictRep $ update (second δv) s d

-- TODO: cleanup
dictVal' :: String -> SelSetter' Val Val
dictVal' s δv = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> (Val α $ Dictionary $ DictRep d') × selType
      where
      d' × selType =
         first (\u' -> insert s (α × u') d) (δv u)
         where
         α × u = fromJust (lookup s d)

envVal :: Var -> Setter (Env (SelStates 𝔹)) (Val (SelStates 𝔹))
envVal x δv γ =
   assert (x ∈ γ) $ update δv x γ

envVal' :: Var -> Setter' (Env (SelStates 𝔹)) (Val (SelStates 𝔹))
envVal' x δv γ =
   assert (x ∈ γ) $ first (flip (insert x) γ) (δv (get x γ))

listCell :: Int -> Setter (Val (SelStates 𝔹)) 𝔹
listCell n δα = unsafePartial $ case _ of
   Val α (Constr c Nil) | n == 0 && c == cNil -> Val (persist δα α) (Constr c Nil)
   Val α (Constr c (v : v' : Nil)) | c == cCons ->
      if n == 0 then Val (persist δα α) (Constr c (v : v' : Nil))
      else Val α (Constr c (v : listCell (n - 1) δα v' : Nil))

listCell' :: Int -> Setter' (Val (SelState 𝔹)) 𝔹
listCell' n δα = unsafePartial $ case _ of
   Val α (Constr c Nil) | n == 0 && c == cNil ->
      first (\α' -> Val α' (Constr c Nil)) (persist' δα α)
   Val α (Constr c (v : u : Nil)) | c == cCons ->
      if n == 0 then first (\α' -> Val α' (Constr c (v : u : Nil))) (persist' δα α)
      else first (\u' -> Val α (Constr c (v : u' : Nil))) (listCell' (n - 1) δα u)
