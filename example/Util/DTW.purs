module Example.Util.DTW where

import Prelude

import Data.Array (concat, elemIndex, foldl, length, modifyAtIndices, range, replicate, sort, unsafeIndex, zip)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Example.Util.BMA (IntInf(..), Matrix, bandMatrix, matIndex, matOfInds)
import Partial.Unsafe (unsafePartial)
import Util (type (×), (×), error)

distanceDTWWindow :: forall a. Partial => Array a -> Array a -> Int -> (a -> a -> IntInf) -> Matrix IntInf × (List (Int × Int))
distanceDTWWindow seq1 seq2 window cost = 
    let n = length seq1
        m = length seq2
        init = bandMatrix n m window
        mappedIndices = sort $ concat (map (\i -> indexIndices i (range (max 1 (i - window)) (min m (i + window)))) (range 1 n))
        (result × priorcells) = foldl worker (init × (matOfInds n m)) mappedIndices
    in
        result × (extractPath priorcells)
    where
      worker :: Matrix IntInf × Matrix (Int × Int) -> (Int × Int) -> Matrix IntInf × Matrix (Int × Int)
      worker (dists × inds) (i' × j') = 
        let 
            im1j   = indexInfty (i'-1) j' dists
            ijm1   = indexInfty i' (j'-1) dists 
            im1jm1 = indexInfty (i'-1) (j'-1) dists
            minim × prev = prevDirection (i' × j') im1j ijm1 im1jm1 
            costij = (cost (unsafeIndex seq1 (i'-1)) (unsafeIndex seq2 (j'-1))) + minim
        in 
            (updateAt i' j' dists (\_ -> costij)) × (updateAt i' j' inds (\_ -> prev))

prevDirection :: (Int × Int) -> IntInf -> IntInf -> IntInf -> IntInf × (Int × Int)
prevDirection (i × j) im1j ijm1 im1jm1 = 
    let minimal = min im1j $ min ijm1 im1jm1
        ind = elemIndex minimal [im1j, ijm1, im1jm1]
    in
        case ind of
        Nothing -> error "error, cannot occur"
        Just y  -> if      y == 0 then (im1j × ((i-1)×j))
                   else if y == 1 then (ijm1 × (i×(j-1)))
                   else if y == 2 then (im1jm1 × ((i-1)×(j-1)))
                   else error "cannot occur"

extractPath :: Matrix (Int × Int) -> List (Int × Int)
extractPath matrix = 
    let i = length matrix - 1
        j = length (unsafePartial $ unsafeIndex matrix 1) - 1
    in 
        traverser i j matrix Nil
    where
        traverser :: Int -> Int -> Matrix (Int × Int) -> List (Int × Int) -> List (Int × Int)
        traverser x y mat accum = 
            if x == y && y == 0 then 
                accum 
            else
                let newPath = Cons (x × y) accum
                    (nextX × nextY) = unsafeMatrixInd x y mat
                in traverser nextX nextY mat newPath
updateAt :: forall a. Partial => Int -> Int -> Matrix a -> (a -> a) -> Matrix a 
updateAt i j matrix f = case matIndex matrix i j of
                        Nothing -> matrix
                        Just _  -> modifyAtIndices [i] (\row -> modifyAtIndices [j] f row) matrix
indexInfty :: Int -> Int -> Matrix IntInf -> IntInf
indexInfty i j matrix = fromMaybe Infty (matIndex matrix i j)
indexIndices :: Int -> Array Int -> Array (Int × Int)
indexIndices i js = zip (replicate (length js) i) js

distEuclid :: IntInf -> IntInf -> IntInf
distEuclid x y = (x - y) * (x - y) 

unsafeMatrixInd :: forall a. Int -> Int -> Matrix a -> a
unsafeMatrixInd x y mat = unsafePartial $ if x < length mat then let xRow = unsafeIndex mat x in if y < length xRow then unsafeIndex xRow y else error "index out of bounds" else error "index out of bounds"