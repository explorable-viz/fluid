module App.View.Util.Orientation where

import Prelude

import Data.List (List(..))
import DataType (cDefault, cRotated)
import Primitive (ToFrom, typeError)
import Val (BaseVal(..))

-- Separate module to avoid cyclic module/orphan instance issue.
data Orientation
   = Default
   | Rotated

-- ======================
-- boilerplate
-- ======================

derive instance Eq Orientation

-- Hefty amount of boilerplate just for a type isomorphic to Bool :-o
orientation :: forall a. ToFrom Orientation a
orientation =
   { pack: case _ of
        Default -> Constr cDefault Nil
        Rotated -> Constr cRotated Nil
   , unpack: case _ of
        Constr c Nil
           | c == cDefault -> Default
           | c == cRotated -> Rotated
        v -> typeError v "Orientation"
   }
