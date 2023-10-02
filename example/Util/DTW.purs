module Example.Util.DTW
   ( NumInf(..)
   , distEuclid
   , distanceDTWWindow
   ) where

import Prelude

import Data.Array (modifyAtIndices, range, (!!), (..))
import Data.Foldable (foldl)
import Data.List (List(..), index, length)
import Data.Maybe (fromJust)
import Data.Ord (abs)
import Partial.Unsafe (unsafePartial)
import Util (type (×), (×))

----------------------------------------
-- Dynamic Time Warp Core
----------------------------------------

costMatrixInit :: Int -> Int -> Int -> Matrix NumInf
costMatrixInit rows cols window = mapMatrix init indexMat
   where
   indexMat = matOfInds rows cols

   init :: Int × Int -> NumInf
   init (0 × 0) = FNum 0.0
   init (0 × _) = Infty
   init (_ × 0) = Infty
   init (x × y) = if (abs $ x - y) <= window then FNum 0.0 else Infty

distanceDTWWindow :: Partial => List Number -> List Number -> Int -> (Number -> Number -> NumInf) -> Matrix NumInf × List (Int × Int)
distanceDTWWindow seq1 seq2 window cost = result × (extractPath priorcells (n × m))
   where
   n = length seq1
   m = length seq2
   init = costMatrixInit n m window

   nextIndices = do
      i <- 1 .. n
      j <- (max 1 (i - window)) .. (min m (i + window))
      [ (i × j) ]

   worker :: Matrix NumInf × Matrix (Int × Int) -> (Int × Int) -> Matrix NumInf × Matrix (Int × Int)
   worker (dists × inds) (i' × j') =
      let
         im1j = dists ! i' - 1 ! j'
         ijm1 = dists ! i' ! j' - 1
         im1jm1 = dists ! i' - 1 ! j' - 1
         minim × prev = costAndPrevD (i' × j') im1j ijm1 im1jm1
         costij = cost (seq1 ? i' - 1) (seq2 ? j' - 1) `plus` minim
      in
         updateAt i' j' dists (const costij) × updateAt i' j' inds (const prev)

   (result × priorcells) = foldl worker (init × (matOfInds n m)) nextIndices

costAndPrevD :: (Int × Int) -> NumInf -> NumInf -> NumInf -> NumInf × Int × Int
costAndPrevD (i × j) im1j ijm1 im1jm1 =
   let
      minimal = min im1j $ min ijm1 im1jm1
   in
      if minimal == im1j then
         im1j × (i - 1) × j
      else if minimal == ijm1 then
         ijm1 × i × (j - 1)
      else -- minimal == im1jm1

         im1jm1 × (i - 1) × (j - 1)

extractPath :: Matrix (Int × Int) -> (Int × Int) -> List (Int × Int)
extractPath matrix (n × m) = traverser n m matrix Nil
   where

   traverser :: Int -> Int -> Matrix (Int × Int) -> List (Int × Int) -> List (Int × Int)
   traverser 0 0 _ accum = accum
   traverser x y mat accum = traverser nextX nextY mat newPath
      where
      newPath = Cons (x × y) accum
      (nextX × nextY) = mat ! x ! y

distEuclid :: Number -> Number -> NumInf
distEuclid x y = FNum ((x - y) * (x - y))

----------------------------------------
-- Matrices and associated Utils
----------------------------------------

type Matrix a = Array (Array a)

unsafeArrayInd :: forall a. Array a -> Int -> a
unsafeArrayInd arr ind = unsafePartial $ fromJust (arr !! ind)

infixl 5 unsafeArrayInd as !

unsafeListInd :: forall a. List a -> Int -> a
unsafeListInd list ind = unsafePartial $ fromJust (index list ind)

infixl 5 unsafeListInd as ?

mapMatrix :: forall a b. (a -> b) -> Matrix a -> Matrix b
mapMatrix f m = map (\row -> map f row) m

matOfInds :: Int -> Int -> Matrix (Int × Int)
matOfInds nrows ncols = matrix
   where
   rowInds = range 0 nrows

   zipRow :: forall a. a -> Int -> Array (a × Int)
   zipRow datum num = map (\x -> datum × x) (range 0 num)
   matrix = map (\x -> zipRow x ncols) rowInds

updateAt :: forall a. Int -> Int -> Matrix a -> (a -> a) -> Matrix a
updateAt i j matrix f = modifyAtIndices [ i ] (modifyAtIndices [ j ] f) matrix

----------------------------------------
-- Ints extended with Infinity, need ot be made into numbers not just ints
----------------------------------------

data NumInf = FNum Number | Infty

instance Show NumInf where
   show (FNum x) = "FNum " <> show x
   show (Infty) = "Infty"

plus :: NumInf -> NumInf -> NumInf
plus Infty _ = Infty
plus _ Infty = Infty
plus (FNum x) (FNum y) = FNum (x + y)

instance Eq NumInf where
   eq Infty Infty = true
   eq Infty (FNum _) = false
   eq (FNum _) Infty = false
   eq (FNum x) (FNum y) = eq x y

instance Ord NumInf where
   compare Infty Infty = EQ
   compare Infty (FNum _) = GT
   compare (FNum _) Infty = LT
   compare (FNum x) (FNum y) = compare x y