module Val2 where

import Prelude hiding (absurd)
import Data.List (List)
import DataType (Ctr)
import Lattice (𝔹)
import Util (Endo, type (×), absurd, error)

type Op a = a × 𝔹 -> Val 𝔹
type MatrixRep a = Array (Array (Val a)) × (Int × a) × (Int × a)

data Val a =
   Int a Int |
   Float a Number |
   Str a String |
   Constr a Ctr (List (Val a)) |
   Matrix a (MatrixRep a) |
   Primitive  (Val 𝔹 -> Val 𝔹)

instance showVal :: Show (Val Boolean) where
   show (Int α n)       = show n <> "_" <> show α
   show (Float α n)     = show n <> "_" <> show α
   show (Str α str)     = show str <> "_" <> show α
   show (Constr _ _ _)  = error "todo"
   show (Matrix _ _)    = error "todo"
   show (Primitive op)  = error "todo"

getα :: Val 𝔹 -> 𝔹
getα (Int α _)       = α
getα (Float α _)     = α
getα (Str α _)       = α
getα (Constr α _ _)  = α
getα _         = error absurd

setα :: 𝔹 -> Endo (Val 𝔹)
setα α (Int _ n)        = Int α n
setα α (Float _ n)      = Float α n
setα α (Str _ str)      = Str α str
setα α (Constr _ c vs)  = Constr α c vs
setα _ _                = error absurd
