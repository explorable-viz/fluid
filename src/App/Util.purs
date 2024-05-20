module App.Util where

import Prelude hiding (absurd)

import Bind (Var)
import Control.Apply (lift2)
import Data.Array ((:)) as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int (fromStringAs, hexadecimal, toStringAs)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, over2)
import Data.Profunctor.Strong (first)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (drop, take)
import Data.Traversable (sequence, sequence_)
import Data.Tuple (fst, snd)
import DataType (cCons, cNil)
import Dict (Dict)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Lattice (class BoundedJoinSemilattice, class JoinSemilattice, 𝔹, bot, neg, (∨))
import Primitive (as, intOrNumber, unpack)
import Primitive as P
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), (×), Endo, definitely', error)
import Util.Map (filterKeys, get)
import Util.Set (isEmpty)
import Val (class Highlightable, BaseVal(..), DictRep(..), Val(..), highlightIf)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventListener, EventTarget)

type Selector (f :: Type -> Type) = Endo (f (SelState 𝔹)) -- modifies selection state
type HTMLId = String
type Renderer a = RendererSpec a -> EventListener -> Effect Unit
type OnSel = Selector Val -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector Val

-- Heavily curried type isn't convenient for FFI
type RendererSpec a =
   { uiHelpers :: UIHelpers
   , divId :: HTMLId
   , suffix :: String
   , view :: a
   }

-- Selection has two dimensions: persistent/transient and primary/secondary
newtype SelState a = SelState
   { persistent :: a
   , transient :: a
   }

instance (Highlightable a, JoinSemilattice a) => Highlightable (SelState a) where
   highlightIf (SelState { persistent, transient }) = highlightIf (persistent ∨ transient)

persist :: forall a. Endo a -> Endo (SelState a)
persist δα = over SelState \s -> s { persistent = δα s.persistent }

selState :: forall a. a -> a -> SelState a
selState b1 b2 = SelState { persistent: b1, transient: b2 }

selected :: forall a. JoinSemilattice a => SelState a -> a
selected (SelState { persistent, transient }) = persistent ∨ transient

isNone𝕊 :: 𝕊 -> Boolean
isNone𝕊 None = true
isNone𝕊 _ = false

isPrimary𝕊 :: 𝕊 -> Boolean
isPrimary𝕊 Primary = true
isPrimary𝕊 _ = false

isSecondary𝕊 :: 𝕊 -> Boolean
isSecondary𝕊 Secondary = true
isSecondary𝕊 _ = false

-- https://stackoverflow.com/questions/5560248
colorShade :: String -> Int -> String
colorShade col n =
   -- remove and reinstate leading "#"
   "#" <> shade (take 2 $ drop 1 col) <> shade (take 2 $ drop 3 col) <> shade (take 2 $ drop 5 col)
   where
   shade :: String -> String
   shade rgbComponent =
      definitely' (fromStringAs hexadecimal rgbComponent) + n
         # clamp 0 255
         # toStringAs hexadecimal

-- TODO: lift more UI logic to PureScript.
bar_fill :: SelState 𝕊 -> Endo String
bar_fill s col = case s of
   SelState { persistent: None } -> col
   _ -> colorShade col (-20)

bar_stroke :: SelState 𝕊 -> Endo String
bar_stroke (SelState { persistent, transient }) col =
   case persistent × transient of
      None × None -> col
      _ -> colorShade col (-70)

indexKey :: String
indexKey = "__n"

-- [any record type with only primitive fields] -> 𝕊
record_isUsed :: Dict (Val (SelState 𝕊)) -> Boolean
record_isUsed r =
   not <<< isEmpty $ flip filterKeys r \k ->
      k /= indexKey && selected (not <<< isNone𝕊 <$> (get k r # \(Val α _) -> α))

cell_class :: String -> Val (SelState 𝕊) -> String
cell_class col v =
   if col /= indexKey && isPrimary𝕊 (v # \(Val (SelState α) _) -> α.persistent) then "cell-selected"
   else if col /= indexKey && isSecondary𝕊 ((v # \(Val (SelState α) _) -> α.persistent)) then "cell-selected-secondary"
   else "cell-unselected"

-- Bundle into a record so we can export via FFI
type UIHelpers =
   { val :: forall a. Selectable a -> a
   , selState :: forall a. Selectable a -> SelState 𝕊
   , isNone𝕊 :: 𝕊 -> Boolean
   , isPrimary𝕊 :: 𝕊 -> Boolean
   , isSecondary𝕊 :: 𝕊 -> Boolean
   , colorShade :: String -> Int -> String
   , barChartHelpers ::
        { bar_fill :: SelState 𝕊 -> Endo String
        , bar_stroke :: SelState 𝕊 -> Endo String
        }
   , tableViewHelpers ::
        { indexKey :: String
        , record_isUsed :: Dict (Val (SelState 𝕊)) -> 𝔹
        , cell_class :: String -> Val (SelState 𝕊) -> String
        }
   }

uiHelpers :: UIHelpers
uiHelpers =
   { val: fst
   , selState: snd
   , isNone𝕊
   , isPrimary𝕊
   , isSecondary𝕊
   , colorShade
   , barChartHelpers:
        { bar_fill
        , bar_stroke
        }
   , tableViewHelpers:
        { indexKey
        , record_isUsed
        , cell_class
        }
   }

data 𝕊 = None | Primary | Secondary
type Selectable a = a × SelState 𝕊

to𝔹 :: SelState 𝕊 -> SelState 𝔹
to𝔹 = (to𝔹' <$> _)
   where
   to𝔹' :: 𝕊 -> 𝔹
   to𝔹' None = false
   to𝔹' Primary = true
   to𝔹' Secondary = true

to𝕊 :: SelState 𝔹 -> SelState 𝕊
to𝕊 = (to𝕊' <$> _)
   where
   to𝕊' :: 𝔹 -> 𝕊
   to𝕊' false = None
   to𝕊' true = Primary

-- Turn previous selection state + new state obtained via related outputs/inputs into primary/secondary sel
as𝕊 :: SelState 𝔹 -> SelState 𝔹 -> SelState 𝕊
as𝕊 = lift2 as𝕊'
   where
   as𝕊' :: 𝔹 -> 𝔹 -> 𝕊
   as𝕊' false false = None
   as𝕊' false true = Secondary
   as𝕊' true false = Primary -- "costless output", but ignore those for now
   as𝕊' true true = Primary

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
derive instance Generic 𝕊 _
instance Show 𝕊 where
   show = genericShow

derive instance Newtype (SelState a) _
derive instance Functor SelState

derive instance Eq a => Eq (SelState a)
derive newtype instance Show a => Show (SelState a)

instance Apply SelState where
   apply (SelState fs) (SelState s) =
      SelState { persistent: fs.persistent s.persistent, transient: fs.transient s.transient }

instance JoinSemilattice a => JoinSemilattice (SelState a) where
   join = over2 SelState \s1 s2 ->
      { persistent: s1.persistent ∨ s2.persistent, transient: s1.transient ∨ s2.transient }

instance BoundedJoinSemilattice a => BoundedJoinSemilattice (SelState a) where
   bot = SelState { persistent: bot, transient: bot }
