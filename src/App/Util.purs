module App.Util where

import Prelude hiding (absurd)

import Bindings (Var)
import Data.Array ((:)) as A
import Data.List (List(..), (:))
import Data.Profunctor.Strong (first)
import DataType (cCons, cNil)
import Dict (Dict, get)
import Effect (Effect)
import Lattice (𝔹)
import Primitive (as, intOrNumber, unpack)
import Primitive as P
import Test.Util (Selector)
import Util (type (×))
import Val (BaseVal(..), Val(..))
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)

type HTMLId = String
type Renderer a = HTMLId -> Int -> a -> EventListener -> Effect Unit
type OnSel = Selector Val -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector Val

doNothing :: OnSel
doNothing = const $ pure unit

get_intOrNumber :: Var -> Dict (Val 𝔹) -> Number × 𝔹
get_intOrNumber x r = first as (unpack intOrNumber (get x r))

-- Assumes fields are all of primitive type.
record :: forall a. (Dict (Val 𝔹) -> a) -> Val 𝔹 -> a
record toRecord (Val _ v) = toRecord (P.record2.unpack v)

class Reflect a b where
   from :: Partial => a -> b

-- Discard any constructor-level annotations.
instance Reflect (Val Boolean) (Array (Val Boolean)) where
   from (Val _ (Constr c Nil)) | c == cNil = []
   from (Val _ (Constr c (u1 : u2 : Nil))) | c == cCons = u1 A.: from u2
