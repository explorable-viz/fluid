module App.Util where

import Prelude
import Control.Apply (lift2)
import Data.Array ((:)) as A
import Data.List (List(..), (:))
import Data.Profunctor.Strong (first)
import Data.Tuple (fst)
import Effect (Effect)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)
import Bindings (Bindings, Var, find)
import DataType (cCons, cNil)
import Lattice (𝔹, expand)
import Primitive (Slice, class ToFrom, as, match, match_fwd)
import Util (type (×), type (+), (×), successful)
import Val (Val)
import Val (Val(..)) as V

type HTMLId = String
type Renderer a = HTMLId -> Int -> a -> EventListener -> Effect Unit
type Redraw = Unit -> Effect Unit
type Handler = Redraw -> Event -> Effect Unit

get_prim :: forall a . ToFrom a => Var -> Slice (Bindings (Val 𝔹)) -> a × 𝔹
get_prim x = match_fwd <<< get x

get_intOrNumber :: Var -> Slice (Bindings (Val 𝔹)) -> Number × 𝔹
get_intOrNumber x r = first as (get_prim x r :: (Int + Number) × 𝔹)

get :: Var -> Slice (Bindings (Val 𝔹)) -> Slice (Val 𝔹)
get x (r × r') = successful $ find x r `lift2 (×)` find x r'

-- Assumes fields are all of primitive type.
record :: forall a . (Slice (Bindings (Val 𝔹)) -> a) -> Slice (Val 𝔹) -> a
record toRecord (u × v) = toRecord (fst (match_fwd (u × v)) × fst (match v))

class Reflect a b where
   from :: Partial => Slice a -> b

-- Perform hole expansion as necessary, and discard any constructor-level annotations.
instance reflectArray :: Reflect (Val Boolean) (Array (Val Boolean × Val Boolean)) where
   from (vs × V.Constr _ c Nil) | c == cNil =
      case expand vs (V.Constr false cNil Nil) of
         V.Constr _ _ Nil -> []
   from (us × V.Constr _ c (v1 : v2 : Nil)) | c == cCons =
      case expand us (V.Constr false cCons (V.Hole false : V.Hole false : Nil)) of
         V.Constr _ _ (u1 : u2 : Nil) -> (u1 × v1) A.: from (u2 × v2)
