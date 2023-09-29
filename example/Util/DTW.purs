module Example.Util.DTW
   ( NumInf(..)
   , distEuclid
   , distanceDTWWindow
   ) where

import Prelude

import Data.Array (elemIndex, length, modifyAtIndices, range, unsafeIndex, (!!), (..))
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Partial.Unsafe (unsafePartial)
import Util (type (×), error, (×))

----------------------------------------
-- Dynamic Time Warp Core
----------------------------------------

costMatrixInit :: Int -> Int -> Int -> Matrix NumInf
costMatrixInit rows cols window = mapMatrix withinBand indexMat
   where
   indexMat = matOfInds rows cols

   withinBand :: (Int × Int) -> NumInf
   withinBand (0 × 0) = FNum 0.0
   withinBand (0 × _) = Infty
   withinBand (_ × 0) = Infty
   withinBand (x × y) = if (abs $ x - y) <= window then FNum 0.0 else Infty

distanceDTWWindow :: forall a. Partial => Array a -> Array a -> Int -> (a -> a -> NumInf) -> Matrix NumInf × (List (Int × Int))
distanceDTWWindow seq1 seq2 window cost = result × (extractPath priorcells)
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
         im1j = indexInfty (i' - 1) j' dists
         ijm1 = indexInfty i' (j' - 1) dists
         im1jm1 = indexInfty (i' - 1) (j' - 1) dists
         minim × prev = costAndPrevD (i' × j') im1j ijm1 im1jm1
         costij = (cost (unsafeIndex seq1 (i' - 1)) (unsafeIndex seq2 (j' - 1))) + minim
      in
         (updateAt i' j' dists (\_ -> costij)) × (updateAt i' j' inds (\_ -> prev))

   (result × priorcells) = foldl worker (init × (matOfInds n m)) nextIndices

costAndPrevD :: (Int × Int) -> NumInf -> NumInf -> NumInf -> NumInf × (Int × Int)
costAndPrevD (i × j) im1j ijm1 im1jm1 =
   let
      minimal = min im1j $ min ijm1 im1jm1
      ind = elemIndex minimal [ im1j, ijm1, im1jm1 ]
   in
      case ind of
         Nothing -> error "error, cannot occur"
         Just y -> case y of 
                   0 -> (im1j × ((i - 1) × j))
                   1 -> (ijm1 × (i × (j - 1)))
                   2 -> (im1jm1 × ((i - 1) × (j - 1)))
                   _ -> error "cannot occur"

extractPath :: Matrix (Int × Int) -> List (Int × Int)
extractPath matrix = traverser i j matrix Nil
   where
   i = length matrix - 1
   j = length (unsafePartial $ unsafeIndex matrix 1) - 1

   traverser :: Int -> Int -> Matrix (Int × Int) -> List (Int × Int) -> List (Int × Int)
   traverser 0 0  _  accum = accum
   traverser x y mat accum = traverser nextX nextY mat newPath
      where
      newPath = Cons (x × y) accum
      (nextX × nextY) = unsafeMatrixInd x y mat

indexInfty :: Int -> Int -> Matrix NumInf -> NumInf
indexInfty i j matrix = fromMaybe Infty (matIndex matrix i j)

distEuclid :: Number -> Number -> NumInf
distEuclid x y = FNum ((x - y) * (x - y))

----------------------------------------
-- Matrices and associated Utils
----------------------------------------

type Matrix a = Array (Array a)

matIndex :: forall a. Matrix a -> Int -> Int -> Maybe a
matIndex mat row col = case mat !! row of
   Nothing -> Nothing
   Just arr -> arr !! col

unsafeMatrixInd :: forall a. Int -> Int -> Matrix a -> a
unsafeMatrixInd x y mat = unsafePartial $
   if x < length mat then
      let
         xRow = unsafeIndex mat x
      in
         if y < length xRow then
            unsafeIndex xRow y
         else
            error "index out of bounds"
   else error "index out of bounds"

mapMatrix :: forall a b. (a -> b) -> Matrix a -> Matrix b
mapMatrix f m = map (\row -> map f row) m

matOfInds :: Int -> Int -> Matrix (Int × Int)
matOfInds nrows ncols = matrix
   where
   rowInds = range 0 nrows

   zipRow :: forall a. a -> Int -> Array (a × Int)
   zipRow datum num = map (\x -> datum × x) (range 0 num)
   matrix = map (\x -> zipRow x ncols) rowInds

updateAt :: forall a. Partial => Int -> Int -> Matrix a -> (a -> a) -> Matrix a
updateAt i j matrix f = case matIndex matrix i j of
   -- Nothing -> matrix
   Just _ -> modifyAtIndices [ i ] (\row -> modifyAtIndices [ j ] f row) matrix

----------------------------------------
-- Ints extended with Infinity, need ot be made into numbers not just ints
----------------------------------------

data NumInf = FNum Number | Infty

instance Show NumInf where
   show (FNum x) = "FNum " <> show x
   show (Infty) = "Infty"

instance Semiring NumInf where
   add Infty _ = Infty
   add _ Infty = Infty
   add (FNum x) (FNum y) = FNum (x + y)
   zero = FNum 0.0
   one = FNum 1.0
   mul Infty _ = Infty
   mul _ Infty = Infty
   mul (FNum x) (FNum y) = FNum (x * y)

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