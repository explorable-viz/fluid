module App.Util where

import Prelude hiding (absurd)

import Bind (Var)
import Data.Array ((:)) as A
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, over2, unwrap)
import Data.Profunctor.Strong (first)
import Data.Traversable (sequence, sequence_)
import Data.Tuple (snd, uncurry)
import DataType (cCons, cNil)
import Dict (Dict)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Lattice (class BoundedJoinSemilattice, class JoinSemilattice, 𝔹, bot, neg, (∨))
import Primitive (as, intOrNumber, unpack)
import Primitive as P
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), Endo, definitely', dup, error)
import Util.Map (get)
import Val (class Highlightable, BaseVal(..), DictRep(..), Val(..), highlightIf)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventListener, EventTarget)

type Selector (f :: Type -> Type) = Endo (f (SelState 𝔹)) -- modifies selection state
type HTMLId = String
type Renderer a = HTMLId -> String -> a -> EventListener -> Effect Unit
type OnSel = Selector Val -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector Val

-- Selection has two dimensions: persistent/transient and primary/secondary
newtype SelState a = SelState
   { persistent :: a
   , transient :: a
   }

instance (Highlightable a, JoinSemilattice a) => Highlightable (SelState a) where
   highlightIf (SelState s) = highlightIf (s.persistent ∨ s.transient)

persist :: forall a. Endo a -> Endo (SelState a)
persist δα = over SelState \s -> s { persistent = δα s.persistent }

selState :: forall a. a -> a -> SelState a
selState b1 b2 = SelState { persistent: b1, transient: b2 }

persistent :: forall a. SelState a -> a
persistent = unwrap >>> _.persistent

transient :: forall a. SelState a -> a
transient = unwrap >>> _.transient

data 𝕊 = None | Primary | Secondary
type Selectable a = a × SelState 𝕊

to𝔹 :: 𝕊 -> 𝔹
to𝔹 None = false
to𝔹 Primary = true
to𝔹 Secondary = true

to𝔹' :: SelState 𝕊 -> SelState 𝔹
to𝔹' = over SelState \s -> { persistent: to𝔹 s.persistent, transient: to𝔹 s.transient }

to𝕊 :: 𝔹 -> 𝕊
to𝕊 = dup >>> uncurry as𝕊

to𝕊' :: SelState 𝔹 -> SelState 𝕊
to𝕊' = over SelState \s -> { persistent: to𝕊 s.persistent, transient: to𝕊 s.transient }

as𝕊 :: 𝔹 -> 𝔹 -> 𝕊
as𝕊 false false = None
as𝕊 false true = Secondary
as𝕊 true false = Primary -- "costless output", but ignore those for now
as𝕊 true true = Primary

-- Turn previous selection state and new state obtained via related outputs/inputs into primary/secondary
as𝕊' :: SelState 𝔹 -> SelState 𝔹 -> SelState 𝕊
as𝕊' (SelState s1) (SelState s2) =
   SelState { persistent: as𝕊 s1.persistent s2.persistent, transient: as𝕊 s1.transient s2.transient }

doNothing :: OnSel
doNothing = const $ pure unit

get_intOrNumber :: Var -> Dict (Val (SelState 𝕊)) -> Selectable Number
get_intOrNumber x r = first as (unpack intOrNumber (get x r))

-- Assumes fields are all of primitive type.
record :: forall a. (Dict (Val (SelState 𝕊)) -> a) -> Val (SelState 𝕊) -> a
record toRecord (Val _ v) = toRecord (P.record2.unpack v)

class Reflect a b where
   from :: Partial => a -> b

-- Discard any constructor-level annotations.
instance Reflect (Val (SelState 𝕊)) (Array (Val (SelState 𝕊))) where
   from (Val _ (Constr c Nil)) | c == cNil = []
   from (Val _ (Constr c (u1 : u2 : Nil))) | c == cCons = u1 A.: from u2

-- Discard both constructor-level annotations and key annotations.
instance Reflect (Val (SelState 𝕊)) (Dict (Val (SelState 𝕊))) where
   from (Val _ (Dictionary (DictRep d))) = d <#> snd

runAffs_ :: forall a. (a -> Effect Unit) -> Array (Aff a) -> Effect Unit
runAffs_ f as = flip runAff_ (sequence as) case _ of
   Left err -> log $ show err
   Right as' -> as' <#> f # sequence_

-- Unpack d3.js data associated with mouse event target.
unsafeEventData :: forall a. Maybe EventTarget -> a
unsafeEventData target = (unsafeCoerce $ definitely' target).__data__

selector :: EventType -> Selector Val
selector (EventType "mousedown") = (over SelState (\s -> s { persistent = neg s.persistent }) <$> _)
selector (EventType "mouseenter") = (over SelState (_ { transient = true }) <$> _)
selector (EventType "mouseleave") = (over SelState (_ { transient = false }) <$> _)
selector (EventType _) = error "Unsupported event type"

-- ======================
-- boilerplate
-- ======================
derive instance Newtype (SelState a) _
derive instance Functor SelState

derive instance Eq a => Eq (SelState a)

instance Apply SelState where
   apply (SelState fs) (SelState s) =
      SelState { persistent: fs.persistent s.persistent, transient: fs.transient s.transient }

instance JoinSemilattice a => JoinSemilattice (SelState a) where
   join = over2 SelState \s1 s2 ->
      { persistent: s1.persistent ∨ s2.persistent, transient: s1.transient ∨ s2.transient }

instance BoundedJoinSemilattice a => BoundedJoinSemilattice (SelState a) where
   bot = SelState { persistent: bot, transient: bot }
