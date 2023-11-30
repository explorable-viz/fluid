module App.Util.Select where

import Prelude hiding (absurd)

import Bindings (Var)
import Data.List (List(..), (:), (!!), updateAt)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first, second)
import DataType (Ctr, cCons, cNil)
import Foreign.Object (member, update)
import Lattice (𝔹)
import Partial.Unsafe (unsafePartial)
import Test.Util (Selector)
import Util (Endo, absurd, assert, definitely', error)
import Val (BaseVal(..), DictRep(..), Val(..), matrixPut, Env)

-- Selection helpers. TODO: turn into lenses/prisms.
matrixElement :: Int -> Int -> Endo (Selector Val)
matrixElement i j δv (Val α (Matrix r)) = Val α $ Matrix $ matrixPut i j δv r
matrixElement _ _ _ _ = error absurd

listElement :: Int -> Endo (Selector Val)
listElement n δv = unsafePartial $ case _ of
   Val α (Constr c (v : v' : Nil)) | n == 0 && c == cCons -> Val α (Constr c (δv v : v' : Nil))
   Val α (Constr c (v : v' : Nil)) | c == cCons -> Val α (Constr c (v : listElement (n - 1) δv v' : Nil))

field :: Var -> Endo (Selector Val)
field f δv = unsafePartial $ case _ of
   Val α (Record r) -> Val α $ Record $ update (δv >>> Just) f r

constrArg :: Ctr -> Int -> Endo (Selector Val)
constrArg c n δv = unsafePartial $ case _ of
   Val α (Constr c' us) | c == c' ->
      let
         us' = definitely' do
            u1 <- us !! n
            updateAt n (δv u1) us
      in
         Val α (Constr c us')

constr :: Ctr -> Endo 𝔹 -> Selector Val
constr c' δα = unsafePartial $ case _ of
   Val α (Constr c vs) | c == c' -> Val (δα α) (Constr c vs)

dict :: Endo 𝔹 -> Selector Val
dict δα = unsafePartial $ case _ of
   Val α (Dictionary d) -> Val (δα α) (Dictionary d)

dictKey :: String -> Endo 𝔹 -> Selector Val
dictKey s δα = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> Val α $ Dictionary $ DictRep $ update (first δα >>> Just) s d

dictVal :: String -> Endo (Selector Val)
dictVal s δv = unsafePartial $ case _ of
   Val α (Dictionary (DictRep d)) -> Val α $ Dictionary $ DictRep $ update (second δv >>> Just) s d

envVal :: Var -> Selector Val -> Selector Env
envVal x δv γ =
   assert (x `member` γ) $ update (δv >>> Just) x γ

listCell :: Int -> Endo 𝔹 -> Selector Val
listCell n δα = unsafePartial $ case _ of
   Val α (Constr c Nil) | n == 0 && c == cNil -> Val (δα α) (Constr c Nil)
   Val α (Constr c (v : v' : Nil)) | n == 0 && c == cCons -> Val (δα α) (Constr c (v : v' : Nil))
   Val α (Constr c (v : v' : Nil)) | c == cCons -> Val α (Constr c (v : listCell (n - 1) δα v' : Nil))
