module App.Util where

import Prelude hiding (absurd)
import Data.Array ((:)) as A
import Data.List (List(..), (:), (!!), updateAt)
import Data.Map (singleton)
import Data.Profunctor.Strong (first)
import Data.Tuple (fst)
import Effect (Effect)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)
import Bindings (Bindings, Var, (↦), find)
import DataType (Ctr, cBarChart, cCons, cNil, cPair, cSome)
import Lattice (𝔹, botOf, neg)
import Primitive (class ToFrom, as, match_fwd)
import Util (Endo, type (×), type (+), (×), absurd, error, definitely', successful)
import Util.SnocList (SnocList(..), (:-))
import Val (Val(..), update, updateMatrix)

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
selectCell :: Int -> Int -> Endo Selector
selectCell i j δv (Matrix α r)  = Matrix α $ updateMatrix i j δv r
selectCell _ _ _ _              = error absurd

selectNth :: Int -> Endo Selector
selectNth 0 δv (Constr α c (v : v' : Nil)) | c == cCons  = Constr α c (δv v : v' : Nil)
selectNth n δv (Constr α c (v : v' : Nil)) | c == cCons  = Constr α c (v : selectNth (n - 1) δv v' : Nil)
selectNth _ _ _                                          = error absurd

selectNthNode :: Int -> Endo 𝔹 -> Selector
selectNthNode 0 δα (Constr α c Nil) | c == cNil             = Constr (δα α) c Nil
selectNthNode 0 δα (Constr α c (v : v' : Nil)) | c == cCons = Constr (δα α) c (v : v' : Nil)
selectNthNode n δα (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (v : selectNthNode (n - 1) δα v' : Nil)
selectNthNode _ _ _                                         = error absurd

selectSome :: Selector
selectSome (Constr _ c v) | c == cSome = Constr true c (botOf v)
selectSome _                           = error absurd

select_y :: Selector -> Selector
select_y δv (Record α (Lin :- f_x ↦ u :- f_y ↦ v)) =
   Record α (Lin :- f_x ↦ u :- f_y ↦ δv v)
select_y _ _ = error absurd

selectBarChart_data :: Endo Selector
selectBarChart_data δv (Constr α c (Record β (Lin :- f_caption ↦ u :- f_data ↦ v) : Nil)) | c == cBarChart =
   Constr α c (Record β (Lin :- f_caption ↦ u :- f_data ↦ δv v) : Nil)
selectBarChart_data _ _ = error absurd

selectPair :: Endo 𝔹 -> Selector -> Selector -> Selector
selectPair δα δv1 δv2 (Constr α c (v1 : v2 : Nil)) | c == cPair = Constr (δα α) c (δv1 v1 : δv2 v2 : Nil)
selectPair _ _ _ _ = error absurd

-- Togglers. TODO: subsumed by selectors now?
toggleCell :: Int -> Int -> Selector
toggleCell i j (Matrix α (vss × (i' × β) × (j' × β'))) =
   Matrix α (updateMatrix i j neg (vss × (i' × β) × (j' × β')))
toggleCell _ _ _ = error absurd

toggleField :: Var -> Selector -> Selector
toggleField f selector (Record α xus) =
   Record α (xus `update` singleton f (selector (get f xus)))
toggleField _ _ _ = error absurd

toggleConstrArg :: Ctr -> Int -> Selector -> Selector
toggleConstrArg c n selector (Constr α c' us) | c == c' =
   definitely' $ do
      u1 <- us !! n
      us' <- updateAt n (selector u1) us
      pure $ Constr α c us'
toggleConstrArg _ _ _ _ = error absurd
