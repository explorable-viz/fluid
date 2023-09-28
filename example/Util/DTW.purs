module Example.Util.DTW where

import Prelude

import Data.Array (concat, concatMap, cons, drop, elemIndex, head, length, mapMaybe, modifyAtIndices, range, replicate, sort, tail, take, uncons, unsafeIndex, zip, zipWith, (!!), (..))
import Data.FastVect.FastVect (Vect)
import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (findWithIndex)
import Data.Int (pow) as I
import Data.Int (toNumber)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (pow)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Util (type (×), error, (×))

product :: forall a len. Semiring a => Vect len a -> a
product v = foldl (*) one v

vsum :: forall a len. Semiring a => Vect len a -> a
vsum v = foldl (+) zero v

sum :: forall f a. Foldable f => Semiring a => f a -> a
sum xs = foldl (+) zero xs

vlen :: forall a len. Vect len a -> Int
vlen xs = foldl (\count _x -> (+) 1 count) 0 xs

vlenN :: forall a len. Vect len a -> Number
vlenN = toNumber <<< vlen

mean :: forall len. Number -> Vect len Number -> Number
mean 0.0 xs = product xs `pow` (1.0 / vlenN xs)
mean p xs = (1.0 / vlenN xs * vsum (map (pow p) xs)) `pow` (1.0 / p)

firstJust :: forall a. Array (Maybe a) -> Maybe a
firstJust aas =
   case uncons aas of
      Nothing -> Nothing
      Just { head: a, tail: as } ->
         case a of
            Nothing -> firstJust as
            Just _ -> a

type Matrix a = Array (Array a)

data IntInf = IInt Int | Infty

instance Show IntInf where
   show (IInt x) = "IInt" <> show x
   show (Infty) = "Infty"

instance Semiring IntInf where
   add Infty _ = Infty
   add _ Infty = Infty
   add (IInt x) (IInt y) = IInt (x + y)
   zero = IInt 0
   one = IInt 1
   mul Infty _ = Infty
   mul _ Infty = Infty
   mul (IInt x) (IInt y) = IInt (x * y)

instance Ring IntInf where -- seems potentially dangerous?
   sub Infty _ = Infty
   sub _ Infty = Infty
   sub (IInt x) (IInt y) = IInt (x - y)

instance Eq IntInf where
   eq Infty Infty = true
   eq Infty (IInt _) = false
   eq (IInt _) Infty = false
   eq (IInt x) (IInt y) = eq x y

instance Ord IntInf where
   compare Infty Infty = EQ
   compare Infty (IInt _) = GT
   compare (IInt _) Infty = LT
   compare (IInt x) (IInt y) = compare x y

ipow :: IntInf -> IntInf -> IntInf
ipow Infty _ = Infty
ipow _ Infty = Infty
ipow (IInt x) (IInt y) = IInt (x `I.pow` y)

matIndex :: forall a. Matrix a -> Int -> Int -> Maybe a
matIndex mat row col = case mat !! row of
   Nothing -> Nothing
   Just arr -> arr !! col

matOfInds :: Int -> Int -> Matrix (Int × Int)
matOfInds nrows ncols = matrix
   where
   rowInds = range 0 nrows

   zipRow :: forall a. a -> Int -> Array (a × Int)
   zipRow datum num = map (\x -> datum × x) (range 0 num)
   matrix = map (\x -> zipRow x ncols) rowInds

genMat :: forall a. (Int × Int -> a) -> Int -> Int -> Matrix a
genMat f nrows ncols = f' matrix
   where
   f' = map (\row -> map (\x -> f x) row)
   matrix = matOfInds nrows ncols

mapIndMat ∷ forall a b. (a -> b) -> Matrix a -> Matrix b
mapIndMat f = map (\y -> map (\x -> f x) y)

bandMatrix' :: Matrix (Int × Int) -> Int -> Matrix IntInf
bandMatrix' indexMat slack = mapIndMat withinBand indexMat
   where
   withinBand :: (Int × Int) -> IntInf
   withinBand (x × y) = if ((x /= 0) && (y /= 0) || (x == 0 && y == 0)) && (abs $ x - y) <= slack then IInt 0 else Infty

bandMatrix :: Int -> Int -> Int -> Matrix IntInf
bandMatrix rows cols window = bandMatrix' (matOfInds rows cols) window

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

arrayProduct :: forall a b. Array a -> Array b -> Array (a × b)
arrayProduct arr1 arr2 = concatMap (\y -> pairify y arr2) arr1
   where
   pairify :: a -> Array b -> Array (a × b)
   pairify elem arr = map (\x -> elem × x) arr

mMult :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
mMult x y = do
   ar <- x
   bc <- (transpose y)
   pure $ [ (sum $ zipWith (*) ar bc) ]

mAdd :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
mAdd x y = map (\(xR × yR) -> zipWith (+) xR yR) (zip x y)

mSub :: forall a. Ring a => Matrix a -> Matrix a -> Matrix a
mSub x y = map (\(xR × yR) -> zipWith (-) xR yR) (zip x y)

mapMatrix :: forall a b. (a -> b) -> Matrix a -> Matrix b
mapMatrix f m = map (\row -> map f row) m

matSquared :: Matrix IntInf -> Matrix IntInf
matSquared mat = mapMatrix (\x -> x `ipow` (IInt 2)) mat

mergeUnion :: Array Int -> Array Int -> Array Int
mergeUnion xxs yys =
   case uncons xxs of
      Nothing -> yys
      Just { head: x, tail: xs } ->
         case uncons yys of
            Nothing -> xxs
            Just { head: y, tail: ys } ->
               case compare x y of
                  LT -> x `cons` mergeUnion xs yys
                  EQ -> x `cons` mergeUnion xs ys
                  GT -> y `cons` mergeUnion xxs ys

nonnegRows :: Matrix IntInf -> Matrix IntInf
nonnegRows mat = map normedRow mat
   where
   normedRow arr = let y = rowMin arr in map (\x -> x - y) arr

rowMin :: Array IntInf -> IntInf
rowMin arr = foldl min Infty arr

nonnegColumns :: Matrix IntInf -> Matrix IntInf
nonnegColumns = transpose <<< nonnegRows <<< transpose

-- unsure what the point of this is
complement :: Int -> Array Int -> Array Int
complement n arr = worker 1 arr
   where
   worker :: Int -> Array Int -> Array Int
   worker k xxs =
      if k > n then []
      else
         case uncons xxs of
            Nothing -> k .. n
            Just { head: x, tail: xs } ->
               case compare k x of
                  EQ -> worker (k + 1) xs
                  LT -> k `cons` worker (k + 1) xxs
                  GT -> worker k xs

-- Unsure what this is going to do in reference implementation

remove :: forall a. Eq a => a -> Array a -> Array a
remove elem arr =
   case findWithIndex (\_ x -> x == elem) arr of
      Nothing -> arr
      Just { index: ind, value: _ } ->
         concat [ (take ind arr), (drop (ind + 1) arr) ]


distanceDTWWindow :: forall a. Partial => Array a -> Array a -> Int -> (a -> a -> IntInf) -> Matrix IntInf × (List (Int × Int))
distanceDTWWindow seq1 seq2 window cost = result × (extractPath priorcells)
   where
   n = length seq1
   m = length seq2
   init = bandMatrix n m window
   nextIndices = sort $ concat (map (\i -> mkRowIndices i (range (max 1 (i - window)) (min m (i + window)))) (range 1 n))

   worker :: Matrix IntInf × Matrix (Int × Int) -> (Int × Int) -> Matrix IntInf × Matrix (Int × Int)
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

costAndPrevD :: (Int × Int) -> IntInf -> IntInf -> IntInf -> IntInf × (Int × Int)
costAndPrevD (i × j) im1j ijm1 im1jm1 =
   let
      minimal = min im1j $ min ijm1 im1jm1
      ind = elemIndex minimal [ im1j, ijm1, im1jm1 ]
   in
      case ind of
         Nothing -> error "error, cannot occur"
         Just y ->
            if y == 0 then (im1j × ((i - 1) × j))
            else if y == 1 then (ijm1 × (i × (j - 1)))
            else if y == 2 then (im1jm1 × ((i - 1) × (j - 1)))
            else error "cannot occur"

extractPath :: Matrix (Int × Int) -> List (Int × Int)
extractPath matrix = traverser i j matrix Nil
   where
   i = length matrix - 1
   j = length (unsafePartial $ unsafeIndex matrix 1) - 1

   traverser :: Int -> Int -> Matrix (Int × Int) -> List (Int × Int) -> List (Int × Int)
   traverser x y mat accum =
      if x == y && y == 0 then
         accum
      else
         traverser nextX nextY mat newPath
      where
      newPath = Cons (x × y) accum
      (nextX × nextY) = unsafeMatrixInd x y mat

updateAt :: forall a. Partial => Int -> Int -> Matrix a -> (a -> a) -> Matrix a
updateAt i j matrix f = case matIndex matrix i j of
   Nothing -> matrix
   Just _ -> modifyAtIndices [ i ] (\row -> modifyAtIndices [ j ] f row) matrix

indexInfty :: Int -> Int -> Matrix IntInf -> IntInf
indexInfty i j matrix = fromMaybe Infty (matIndex matrix i j)

mkRowIndices :: Int -> Array Int -> Array (Int × Int)
mkRowIndices i js = zip (replicate (length js) i) js

distEuclid :: IntInf -> IntInf -> IntInf
distEuclid x y = (x - y) * (x - y)

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