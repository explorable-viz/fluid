module Example.Util.BMA where

import Prelude

import Data.Array (concat, concatMap, cons, drop, head, insert, length, mapMaybe, range, sort, tail, take, uncons, zip, zipWith, (!!), (..))
import Data.FastVect.FastVect (Vect)
import Data.Foldable (class Foldable, find, foldl)
import Data.FoldableWithIndex (findWithIndex)
import Data.Int (pow) as I
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.Ord (abs)
import Data.Traversable (for)
import Data.Tuple (snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
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
mean p xs = (1.0 / vlenN xs * vsum (map (pow p) xs)) `pow` (1.0/p)

firstJust :: forall a. Array (Maybe a) -> Maybe a
firstJust aas = 
   case uncons aas of
   Nothing -> Nothing
   Just {head: a, tail: as} ->
      case a of
         Nothing -> firstJust as
         Just _  -> a

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


arrayProduct :: forall a b. Array a -> Array b -> Array (a × b)
arrayProduct arr1 arr2 = concatMap (\y -> pairify y arr2) arr1
   where
     pairify :: a -> Array b -> Array (a × b)
     pairify elem arr = map (\x -> elem × x) arr
     
mMult :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
mMult x y = do
   ar <- x
   bc <- (transpose y)
   pure $ [(sum $ zipWith (*) ar bc)]

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
      Just {head: x, tail: xs} ->
         case uncons yys of
            Nothing -> xxs
            Just {head: y, tail: ys} ->
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
   worker k xxs = if k > n then []
                  else
                     case uncons xxs of
                        Nothing -> k..n
                        Just {head: x, tail: xs} ->
                           case compare k x of
                              EQ -> worker (k+1) xs
                              LT -> k `cons` worker (k+1) xxs
                              GT -> worker k xs

step3 :: Int -> Array (Int × Int) -> Array (Int × Int) ->Array Int -> Array Int -> Matrix IntInf -> Array (Int × Int)
step3 dim starred primed coveredRows coveredCols matrix = 
   let colsC = mergeUnion coveredCols (sort $ map snd starred) in
      if length colsC == (length matrix) then starred 
      else
         step4 dim starred primed coveredRows coveredCols matrix

-- Unsure what this is going to do in reference implementation
step4 :: Int -> Array (Int × Int) -> Array (Int × Int) -> Array Int -> Array Int -> Matrix IntInf -> Array (Int × Int)
step4 dim starred primed coveredRows coveredCols matrix = 
   let rowsNC = complement dim coveredRows
       colsNC = complement dim coveredCols
       f :: Int × Int -> Maybe (Int × Int)
       f (i × j)   = 
         case matIndex matrix i j of
            Nothing -> Nothing
            Just iinf -> if iinf == IInt 0 then Just (i × j) else Nothing
       uncovered = arrayProduct rowsNC colsNC
       mp = firstJust (map f uncovered)
   in
      case mp of
         Nothing -> let es = for uncovered (\(x × y) -> matIndex matrix x y) in
                     case es of
                        Just es' -> step6 (rowMin es')
                        Nothing  -> error "Not sure how I got here"
         Just ij@(i × _) -> let newPrim = cons ij primed in
                              case find (\(p × _) -> p == i) starred of
                                 Nothing      -> step5 ij
                                 Just (_ × q) -> step4 dim starred newPrim (insert i coveredRows) (remove q coveredCols) matrix

remove :: forall a. Eq a => a -> Array a -> Array a
remove elem arr = 
   case findWithIndex (\_ x -> x == elem) arr of
      Nothing -> arr
      Just {index: ind, value: _} ->
         concat [(take ind arr), (drop (ind + 1) arr)]

step5 = error "todo"
step6 = error "todo"
main :: Effect Unit
main = do
   logShow (genMat (\(x × y) -> if (abs $ x - y) <= 3 then IInt 1 else Infty) 10 10)
   let newMat = (genMat (\(x × y) -> x + y) 3 4)
   log $ "newMat: " <> (show newMat)
   log $ "transposed: " <> (show (transpose newMat))

   let testMul = [[1, 2],[3, 4]] `mMult` [[5, 6], [7, 8]]
   logShow testMul
   let testAdd = [[1,0], [0, 1]] `mSub` [[0, 1], [1,0]]
   logShow testAdd
   let testnonnegRows = nonnegRows [[IInt 1, IInt 2, IInt 3], 
                                    [IInt 2, IInt 3, IInt 4], 
                                    [IInt 3, IInt 4, IInt 5]]
   logShow testnonnegRows

