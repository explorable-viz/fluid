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
import Lattice (𝔹)
import Primitive (as, intOrNumber, unpack)
import Primitive as P
import Test.Util (Selector)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), definitely', dup)
import Util.Map (get)
import Val (BaseVal(..), DictRep(..), Val(..))
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener, EventTarget)

type HTMLId = String
type Renderer a = HTMLId -> String -> a -> EventListener -> Effect Unit
type OnSel = Selector Val -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector Val

data Sel = None | Primary | Secondary
type Selectable a = a × Sel

to𝔹 :: Sel -> 𝔹
to𝔹 None = false
to𝔹 Primary = true
to𝔹 Secondary = true

toSel :: 𝔹 -> Sel
toSel = dup >>> uncurry asSel

asSel :: 𝔹 -> 𝔹 -> Sel
asSel false false = None
asSel false true = Secondary
asSel true false = Primary -- "costless output", but ignore those for now
asSel true true = Primary

doNothing :: OnSel
doNothing = const $ pure unit

get_intOrNumber :: Var -> Dict (Val Sel) -> Selectable Number
get_intOrNumber x r = first as (unpack intOrNumber (get x r))

-- Assumes fields are all of primitive type.
record :: forall a. (Dict (Val Sel) -> a) -> Val Sel -> a
record toRecord (Val _ v) = toRecord (P.record2.unpack v)

class Reflect a b where
   from :: Partial => a -> b

-- Discard any constructor-level annotations.
instance Reflect (Val Sel) (Array (Val Sel)) where
   from (Val _ (Constr c Nil)) | c == cNil = []
   from (Val _ (Constr c (u1 : u2 : Nil))) | c == cCons = u1 A.: from u2

-- Discard both constructor-level annotations and key annotations.
instance Reflect (Val Sel) (Dict (Val Sel)) where
   from (Val _ (Dictionary (DictRep d))) = d <#> snd

runAffs_ :: forall a. (a -> Effect Unit) -> Array (Aff a) -> Effect Unit
runAffs_ f as = flip runAff_ (sequence as) case _ of
   Left err -> log $ show err
   Right as' -> as' <#> f # sequence_

-- Unpack d3.js data associated with mouse event target.
unsafeEventData :: forall a. Maybe EventTarget -> a
unsafeEventData target = (unsafeCoerce $ definitely' target).__data__
