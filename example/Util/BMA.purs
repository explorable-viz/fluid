module Example.Util.BMA where

import Prelude

import Data.Array (cons, head, mapMaybe, range, tail, uncons, (!!))
import Data.FastVect.FastVect (Vect)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Util (type (×), (×))

product :: forall a len. Semiring a => Vect len a -> a
product v = foldl (*) one v 

sum :: forall a len. Semiring a => Vect len a -> a
sum v = foldl (+) zero v

vlen :: forall a len. Vect len a -> Int
vlen xs = foldl (\count _x -> (+) 1 count) 0 xs

vlenN :: forall a len. Vect len a -> Number
vlenN = toNumber <<< vlen

mean :: forall len. Number -> Vect len Number -> Number
mean 0.0 xs = product xs `pow` (1.0 / vlenN xs)
mean p xs = (1.0 / vlenN xs * sum (map (pow p) xs)) `pow` (1.0/p)

type Matrix a = Array (Array a)

data IntInf = IInt Int | Infty
instance Show IntInf where
   show (IInt x) = "IInt" <> show x
   show (Infty) = "Infty"


matIndex :: forall a. Matrix a -> Int -> Int -> Maybe a
matIndex mat row col = case mat !! row of
                            Nothing  -> Nothing
                            Just arr -> arr !! col

matOfInds :: Int -> Int -> Matrix (Int × Int)
matOfInds nrows ncols = matrix
   where
   rowInds = range 1 nrows
   zipRow :: forall a. a -> Int -> Array (a × Int)
   zipRow datum num = map (\x -> datum × x) (range 1 num)
   matrix = map (\x -> zipRow x ncols) rowInds

genMat :: forall a. (Int × Int -> a) -> Int -> Int -> Matrix a
genMat f nrows ncols = f' matrix
   where
   f' = map (\row -> map (\x -> f x) row)
   matrix = matOfInds nrows ncols

mapIndMat ∷ ∀ (f71 ∷ Type -> Type) (f74 ∷ Type -> Type) (a75 ∷ Type) (b76 ∷ Type). Functor f71 ⇒ Functor f74 ⇒ (a75 → b76) → f71 (f74 a75) → f71 (f74 b76)
mapIndMat f = map (\y -> map (\x -> f x) y)

bandMatrix :: Matrix (Int × Int) -> Int -> Matrix IntInf
bandMatrix indexMat slack = mapIndMat withinBand indexMat 
   where
   withinBand :: (Int × Int) -> IntInf
   withinBand (x × y) = if (abs $ x - y) <= slack then IInt 1 else Infty

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose xs =
  case uncons xs of
    Nothing ->
      xs
    Just { head: h, tail: xss } ->
      case uncons h of
        Nothing ->
          transpose xss
        Just { head: x, tail: xs' } ->
          (x `cons` mapMaybe head xss) `cons` transpose (xs' `cons` mapMaybe tail xss)


main :: Effect Unit
main = do
   logShow (genMat (\(x × y) -> if (abs $ x - y) <= 3 then IInt 1 else Infty) 10 10)
   let newMat = (genMat (\(x × y) -> x + y) 3 4)
   log $ "newMat: " <> (show newMat)
   log $ "transposed: " <> (show (transpose newMat))

