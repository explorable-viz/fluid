module Primitive2 where

import Data.Int (toNumber)
import Data.Either (Either(..))
import Util (type (+))

union2 :: (Int -> Int -> Int) -> (Number -> Number -> Number) -> Int + Number -> Int + Number -> Int + Number
union2 f _ (Left x) (Left y)     = Left (f x y)
union2 _ f (Left x) (Right y)    = Right (f (toNumber x) y)
union2 _ f (Right x) (Right y)   = Right (f x y)
union2 _ f (Right x) (Left y)    = Right (f x (toNumber y))
