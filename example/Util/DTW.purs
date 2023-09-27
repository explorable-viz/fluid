module Example.Util.DTW where

import Prelude

import Data.Array (concat, concatMap, drop, foldl, length, mapWithIndex, range, replicate, sort, take, unsafeIndex, zip)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Example.Util.BMA (IntInf(..), Matrix, bandMatrix, matIndex)
import Util (type (×), (×))

distanceDTW :: forall a. Partial => Array a -> Array a -> Int -> (a -> a -> IntInf) -> Matrix IntInf
distanceDTW seq1 seq2 window cost = 
    let init = bandMatrix (length seq1) (length seq2) window
        mappedIndices = sort (concatMap (\i -> indexIndices i (range (max 1 (i - window)) (min (length seq2) (i + window)))) (range 1 (length seq1)))
    in
        foldl worker init mappedIndices
    where
      worker :: Matrix IntInf -> (Int × Int) -> Matrix IntInf
      worker matrix (i × j) = 
        let im1j   = indexInfty (i-1) j matrix
            ijm1   = indexInfty i (j-1) matrix 
            im1jm1 = indexInfty (i-1) (j-1) matrix
            minim = min im1jm1 $ min im1j ijm1 
            costij = (cost (unsafeIndex seq1 i) (unsafeIndex seq2 j)) + minim
        in 
            updateAt i j matrix (\_ -> costij) 

updateAt :: forall a. Partial => Int -> Int -> Matrix a -> (a -> a) -> Matrix a 
updateAt i j matrix f = case matIndex matrix i j of
                        Nothing -> matrix
                        Just x  -> concat [take (i-1) matrix,[ mapWithIndex (\ind x -> if ind == j then f x else x ) (unsafeIndex matrix i)], drop i matrix] 
indexInfty :: Int -> Int -> Matrix IntInf -> IntInf
indexInfty i j matrix = fromMaybe Infty (matIndex matrix i j)
indexIndices :: Int -> Array Int -> Array (Int × Int)
indexIndices i js = zip (replicate (length js) i) js

distEuclid :: IntInf -> IntInf -> IntInf
distEuclid x y = abs $ x - y