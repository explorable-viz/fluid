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
import Bindings (Bindings, Var, (↦), find)
import DataType (cBarChart, cCons, cNil, cPair)
import Lattice (Slice, 𝔹, expand)
import Primitive (class ToFrom, as, match, match_fwd)
import Util (type (×), type (+), (×), successful)
import Util.SnocList (SnocList(..), (:-))
import Val (Val(..), holeMatrix, insertMatrix)

type HTMLId = String
type Renderer a = HTMLId -> Int -> a -> EventListener -> Effect Unit
type OnSel = (Slice (Val 𝔹) -> Val 𝔹) -> Effect Unit -- redraw based on modified output selection
type Handler = OnSel -> Event -> Effect Unit
type Handler2 = Event -> Slice (Val 𝔹) -> Val 𝔹

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
   from (vs × Constr _ c Nil) | c == cNil =
      case expand vs (Constr false cNil Nil) of
         Constr _ _ Nil -> []
   from (us × Constr _ c (v1 : v2 : Nil)) | c == cCons =
      case expand us (Constr false cCons (Hole false : Hole false : Nil)) of
         Constr _ _ (u1 : u2 : Nil) -> (u1 × v1) A.: from (u2 × v2)

-- Selection helpers.
selectCell :: 𝔹 -> Int -> Int -> Int -> Int -> Val 𝔹
selectCell α i j i' j' = Matrix false (insertMatrix i j (Hole α) (holeMatrix i' j'))

selectNth :: Int -> Val 𝔹 -> Val 𝔹
selectNth 0 v = Constr false cCons (v : Hole false : Nil)
selectNth n v = Constr false cCons (Hole false : selectNth (n - 1) v : Nil)

select_y :: Val 𝔹
select_y = Record false (Lin :- "x" ↦ Hole false :- "y" ↦ Hole true)

selectBarChart_data :: Val 𝔹 -> Val 𝔹
selectBarChart_data v = Constr false cBarChart (Record false (Lin :- "caption" ↦ Hole false :- "data" ↦ v) : Nil)

selectPair :: 𝔹 -> Val 𝔹 -> Val 𝔹 -> Val 𝔹
selectPair α v1 v2 = Constr α cPair (v1 : v2 : Nil)
