module App.Util.Select where

import Prelude hiding (absurd)
import Bindings (Var)
import App.Util (Selector)
import Data.List (List(..), (:), (!!), updateAt)
import Data.Maybe (Maybe(..))
import DataType (Ctr, cCons, cNil)
import Foreign.Object (update)
import Lattice (𝔹)
import Partial.Unsafe (unsafePartial)
import Util (Endo, absurd, error, definitely')
import Val (Val(..), matrixUpdate)

-- Selection helpers. TODO: turn into lenses/prisms.
matrixElement :: Int -> Int -> Endo (Selector Val)
matrixElement i j δv (Matrix α r) = Matrix α $ matrixUpdate i j δv r
matrixElement _ _ _ _ = error absurd

listElement :: Int -> Endo (Selector Val)
listElement 0 δv (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (δv v : v' : Nil)
listElement n δv (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (v : listElement (n - 1) δv v' : Nil)
listElement _ _ _ = error absurd

listCell :: Int -> Endo 𝔹 -> Selector Val
listCell 0 δα (Constr α c Nil) | c == cNil = Constr (δα α) c Nil
listCell 0 δα (Constr α c (v : v' : Nil)) | c == cCons = Constr (δα α) c (v : v' : Nil)
listCell n δα (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (v : listCell (n - 1) δα v' : Nil)
listCell _ _ _ = error absurd

constr :: Ctr -> Selector Val
constr c' = unsafePartial $ case _ of
   Constr _ c vs | c == c' -> Constr true c vs

field :: Var -> Endo (Selector Val)
field f δv = unsafePartial $ case _ of
   Record α r -> Record α $ update (δv >>> Just) f r

constrArg :: Ctr -> Int -> Endo (Selector Val)
constrArg c n δv = unsafePartial $ case _ of
   Constr α c' us | c == c' ->
      let
         us' = definitely' $ do
            u1 <- us !! n
            updateAt n (δv u1) us
      in
         Constr α c us'
