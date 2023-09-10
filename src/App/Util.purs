module App.Util where

import Prelude hiding (absurd)

import Bindings (Var)
import Data.Array ((:)) as A
import Data.List (List(..), (:))
import Data.Profunctor.Strong (first)
import Data.Tuple (fst)
import DataType (cCons, cNil)
import Dict (Dict, get)
import Effect (Effect)
import Lattice (𝔹)
import Primitive (as, intOrNumber)
import Primitive (record) as P
import Util (type (×))
import Val (Val(..))
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)

type HTMLId = String
type Renderer a = HTMLId -> Int -> a -> EventListener -> Effect Unit
type Selector f = f 𝔹 -> f 𝔹 -- modifies selection state
type OnSel = Selector Val -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector Val

doNothing :: OnSel
doNothing = const $ pure unit

get_intOrNumber :: Var -> Dict (Val 𝔹) -> Number × 𝔹
get_intOrNumber x r = first as (intOrNumber.match (get x r))

-- Assumes fields are all of primitive type.
record :: forall a. (Dict (Val 𝔹) -> a) -> Val 𝔹 -> a
record toRecord u = toRecord (fst (P.record.match u))

class Reflect a b where
   from :: Partial => a -> b

-- Discard any constructor-level annotations.
instance Reflect (Val Boolean) (Array (Val Boolean)) where
   from (Constr _ c Nil) | c == cNil = []
   from (Constr _ c (u1 : u2 : Nil)) | c == cCons = u1 A.: from u2
