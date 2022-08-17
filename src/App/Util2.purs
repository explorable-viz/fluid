module App.Util2 where

import Prelude hiding (absurd)
import Data.Array ((:)) as A
import Data.List (List(..), (:), (!!), updateAt)
import Data.Profunctor.Strong (first)
import Data.Tuple (fst)
import Effect (Effect)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)
import Bindings2 (Bindings, Var, (↦), find, update)
import DataType2 (Ctr, cBarChart, cCons, cNil, cPair, f_caption, f_data, f_x, f_y)
import Lattice2 (𝔹, botOf, neg)
import Primitive2 (class ToFrom, as, match_fwd)
import Util2 (type (×), type (+), (×), (!), absurd, error, definitely', successful, unimplemented)
import Util.SnocList2 (SnocList(..), (:-))
import Val2 (Val(..), insertMatrix)

type HTMLId = String
type Renderer a = HTMLId -> Int -> a -> EventListener -> Effect Unit
type Selector = Val 𝔹 -> Val 𝔹
type OnSel = Selector -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector

doNothing :: OnSel
doNothing = const $ pure unit

get_prim :: forall a . ToFrom a => Var -> Bindings (Val 𝔹) -> a × 𝔹
get_prim x = match_fwd <<< get x

get_intOrNumber :: Var -> Bindings (Val 𝔹) -> Number × 𝔹
get_intOrNumber x r = first as (get_prim x r :: (Int + Number) × 𝔹)

get :: Var -> Bindings (Val 𝔹) -> Val 𝔹
get x r = successful $ find x r

-- Assumes fields are all of primitive type.
record :: forall a . (Bindings (Val 𝔹) -> a) -> Val 𝔹 -> a
record toRecord u = toRecord (fst (match_fwd u))

class Reflect a b where
   from :: Partial => a -> b

-- Discard any constructor-level annotations.
instance reflectArray :: Reflect (Val Boolean) (Array (Val Boolean)) where
   from (Constr _ c Nil) | c == cNil = []
   from (Constr _ c (u1 : u2 : Nil)) | c == cCons = u1 A.: from u2

-- Selection helpers.
selectCell :: 𝔹 -> Int -> Int -> Int -> Int -> Val 𝔹
selectCell _ i j _ _ = Matrix false (insertMatrix i j (error unimplemented) (error unimplemented))

selectNth :: Int -> Val 𝔹 -> Val 𝔹
selectNth 0 v = Constr false cCons (v : error unimplemented : Nil)
selectNth n v = Constr false cCons (error unimplemented : selectNth (n - 1) v : Nil)

selectNth2 :: Int -> Selector -> Selector
selectNth2 0 δv (Constr _ c (v : v' : Nil)) | c == cCons =
   Constr false cCons (δv v : botOf v' : Nil)
selectNth2 n δv (Constr _ c (v : v' : Nil)) | c == cCons =
   Constr false cCons (botOf v : selectNth2 (n - 1) δv v' : Nil)
selectNth2 _ _ _ = error absurd

selectNthNode :: Int -> Selector
selectNthNode 0 (Constr _ c Nil) | c == cNil =
   Constr true cNil Nil
selectNthNode 0 (Constr _ c (v : v' : Nil)) | c == cCons =
   Constr true cCons (botOf v : botOf v' : Nil)
selectNthNode n (Constr _ c (v : v' : Nil)) | c == cCons =
   Constr false cCons (botOf v : selectNthNode (n - 1) v' : Nil)
selectNthNode _ _ = error absurd

select_y :: Val 𝔹
select_y = Record false (Lin :- f_x ↦ error unimplemented :- f_y ↦ error unimplemented)

selectBarChart_data :: Val 𝔹 -> Val 𝔹
selectBarChart_data v =
   Constr false cBarChart (Record false (Lin :- f_caption ↦ error unimplemented :- f_data ↦ v) : Nil)

selectPair :: 𝔹 -> Val 𝔹 -> Val 𝔹 -> Val 𝔹
selectPair α v1 v2 = Constr α cPair (v1 : v2 : Nil)

-- Togglers.
toggleCell :: Int -> Int -> Selector
toggleCell i j (Matrix α (vss × (i' × β) × (j' × β'))) =
   Matrix α (insertMatrix i j (neg vss!(i - 1)!(j - 1)) (vss × (i' × β) × (j' × β')))
toggleCell _ _ _ = error absurd

toggleNth :: Int -> Selector -> Selector
toggleNth n selector (Constr α c (u1 : u2 : Nil)) | c == cCons =
   case n of
      0 -> Constr α c (selector u1 : u2 : Nil)
      _ -> Constr α c (u1 : toggleNth (n - 1) selector u2 : Nil)
toggleNth _ _ _ = error absurd

toggleField :: Var -> Selector -> Selector
toggleField f selector (Record α xus) =
   Record α (update xus (f ↦ selector (get f xus)))
toggleField _ _ _ = error absurd

toggleConstrArg :: Ctr -> Int -> Selector -> Selector
toggleConstrArg c n selector (Constr α c' us) | c == c' =
   definitely' $ do
      u1 <- us !! n
      us' <- updateAt n (selector u1) us
      pure $ Constr α c us'
toggleConstrArg _ _ _ _ = error absurd
