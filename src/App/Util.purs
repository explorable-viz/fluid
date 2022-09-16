module App.Util where

import Prelude hiding (absurd)
import Data.Array ((:)) as A
import Data.List (List(..), (:), (!!), updateAt)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.Tuple (fst)
import Effect (Effect)
import Foreign.Object (update)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)
import Bindings (Dict, Var)
import DataType (Ctr, cBarChart, cCons, cNil, cPair, cSome, f_data, f_y)
import Lattice (𝔹, botOf, neg)
import Primitive (class ToFrom, as, match_fwd)
import Util (Endo, type (×), type (+), (×), absurd, error, definitely', get)
import Val (Val(..), updateMatrix)

type HTMLId = String
type Renderer a = HTMLId -> Int -> a -> EventListener -> Effect Unit
type Selector = Val 𝔹 -> Val 𝔹
type OnSel = Selector -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector

doNothing :: OnSel
doNothing = const $ pure unit

get_prim :: forall a . ToFrom a => Var -> Dict (Val 𝔹) -> a × 𝔹
get_prim x = match_fwd <<< get x

get_intOrNumber :: Var -> Dict (Val 𝔹) -> Number × 𝔹
get_intOrNumber x r = first as (get_prim x r :: (Int + Number) × 𝔹)

-- Assumes fields are all of primitive type.
record :: forall a . (Dict (Val 𝔹) -> a) -> Val 𝔹 -> a
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
selectSome (Constr _ c vs) | c == cSome   = Constr true c (botOf <$> vs)
selectSome _                              = error absurd

select_y :: Selector -> Selector
select_y δv (Record α r) = Record α $ update (δv >>> Just) f_y r
select_y _ _ = error absurd

selectBarChart_data :: Endo Selector
selectBarChart_data δv (Constr α c (Record β r : Nil)) | c == cBarChart =
   Constr α c (Record β (update (δv >>> Just) f_data r) : Nil)
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
toggleField f selector (Record α r) = Record α $ update (selector >>> Just) f r
toggleField _ _ _ = error absurd

toggleConstrArg :: Ctr -> Int -> Selector -> Selector
toggleConstrArg c n selector (Constr α c' us) | c == c' =
   definitely' $ do
      u1 <- us !! n
      us' <- updateAt n (selector u1) us
      pure $ Constr α c us'
toggleConstrArg _ _ _ _ = error absurd
