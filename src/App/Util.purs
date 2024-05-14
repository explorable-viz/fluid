module App.Util where

import Prelude hiding (absurd)

import Bind (Var)
import Data.Array ((:)) as A
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Profunctor.Strong (first)
import Data.Traversable (sequence, sequence_)
import Data.Tuple (snd, uncurry)
import DataType (cCons, cNil)
import Dict (Dict)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Lattice (𝔹, botOf, neg, topOf)
import Primitive (as, intOrNumber, unpack)
import Primitive as P
import Test.Util (Selector)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), definitely', dup, error)
import Util.Map (get)
import Val (BaseVal(..), DictRep(..), Val(..))
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventListener, EventTarget)

type HTMLId = String
type Renderer a = HTMLId -> String -> a -> EventListener -> Effect Unit
type OnSel = Selector Val -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector Val

-- Selection has two dimensions: persistent/transient and primary/secondary
type SelState a =
   { persistent :: a
   , transient :: a
   }

data 𝕊 = None | Primary | Secondary
type Selectable a = a × 𝕊

to𝔹 :: 𝕊 -> 𝔹
to𝔹 None = false
to𝔹 Primary = true
to𝔹 Secondary = true

to𝕊 :: 𝔹 -> 𝕊
to𝕊 = dup >>> uncurry as𝕊

-- Turn previous selection state and new state obtained via related outputs/inputs into primary/secondary
as𝕊 :: 𝔹 -> 𝔹 -> 𝕊
as𝕊 false false = None
as𝕊 false true = Secondary
as𝕊 true false = Primary -- "costless output", but ignore those for now
as𝕊 true true = Primary

as𝕊' :: SelState 𝔹 -> SelState 𝔹 -> SelState 𝕊
as𝕊' { persistent: b1, transient: b2 } { persistent: b1', transient: b2' } =
   { persistent: as𝕊 b1 b1'
   , transient: as𝕊 b2 b2'
   }

doNothing :: OnSel
doNothing = const $ pure unit

get_intOrNumber :: Var -> Dict (Val 𝕊) -> Selectable Number
get_intOrNumber x r = first as (unpack intOrNumber (get x r))

-- Assumes fields are all of primitive type.
record :: forall a. (Dict (Val 𝕊) -> a) -> Val 𝕊 -> a
record toRecord (Val _ v) = toRecord (P.record2.unpack v)

class Reflect a b where
   from :: Partial => a -> b

-- Discard any constructor-level annotations.
instance Reflect (Val 𝕊) (Array (Val 𝕊)) where
   from (Val _ (Constr c Nil)) | c == cNil = []
   from (Val _ (Constr c (u1 : u2 : Nil))) | c == cCons = u1 A.: from u2

-- Discard both constructor-level annotations and key annotations.
instance Reflect (Val 𝕊) (Dict (Val 𝕊)) where
   from (Val _ (Dictionary (DictRep d))) = d <#> snd

runAffs_ :: forall a. (a -> Effect Unit) -> Array (Aff a) -> Effect Unit
runAffs_ f as = flip runAff_ (sequence as) case _ of
   Left err -> log $ show err
   Right as' -> as' <#> f # sequence_

-- Unpack d3.js data associated with mouse event target.
unsafeEventData :: forall a. Maybe EventTarget -> a
unsafeEventData target = (unsafeCoerce $ definitely' target).__data__

selector :: EventType -> Selector Val
selector (EventType "mousedown") = neg
selector (EventType "mouseenter") = topOf
selector (EventType "mouseleave") = botOf
selector (EventType _) = error "Unsupported event type"
