module App.Util
   ( Attrs
   , ReactState(..)
   , SelState(..)
   , Selectable
   , Selector
   , TelState(..)
   , ViewSelector
   , asℝ
   , as𝕊
   , attrs
   , class Reflect
   , colorShade
   , compare'
   , css
   , eventData
   , from
   , fromℝ
   , get_intOrNumber
   , isNone
   , isPersistent
   , isPrimary
   , isSecondary
   , isTransient
   , persist
   , record
   , runAffs_
   , selClasses
   , selClassesFor
   , selState
   , selected
   , selectionEventData
   , selector
   , toℝ
   , to𝔹
   , to𝕊
   , 𝕊(..)
   ) where

import Prelude hiding (absurd, join)

import Bind (Bind, Var)
import Control.Apply (lift2)
import Data.Array ((:)) as A
import Data.Array (concat)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (fromStringAs, hexadecimal, toStringAs)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over, over2)
import Data.Profunctor.Strong ((&&&), first)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.String.CodeUnits (drop, take)
import Data.Traversable (sequence, sequence_)
import Data.Tuple (snd)
import DataType (cCons, cNil)
import Dict (Dict)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Foreign.Object (Object, empty, fromFoldable, union)
import Lattice (class BoundedJoinSemilattice, class JoinSemilattice, 𝔹, bot, neg, (∨))
import Primitive (as, intOrNumber, unpack)
import Primitive as P
import Test.Util.Debug (tracing)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), Endo, definitely', error, spyWhen)
import Util.Map (get)
import Val (class Highlightable, BaseVal(..), DictRep(..), Val(..), highlightIf)
import Web.Event.Event (Event, EventType(..), target, type_)
import Web.Event.EventTarget (EventTarget)

type Selector (f :: Type -> Type) = Endo (f (SelState 𝔹)) -- modifies selection state
type ViewSelector a = a -> Endo (Selector Val) -- convert mouse event data to view selector

-- Selection has two dimensions: persistent/transient and primary/secondary/inert. An element can be persistently
-- *and* transiently selected at the same time; these need to be visually distinct (so that for example
-- clicking during mouseover visibly changes the state). Primary and secondary also need to be visually
-- distinct but not orthogonal; primary should (visually) subsume secondary. 
-- inert is for nodes with no descendants. 
-- We implement ReactState, then TelState to possibly include none in a different location.

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

data ReactState a = Inert | Reactive (SelState a)

newtype TelState a = TelState
   {
     -- like ReactState, but here we shove none as a possibility via {unused, inert} = {true,false}
     -- note that now sel will have a great deal of unused things, but this perhaps might be nicer 
     -- if we can streamline it to "persistent or not" "primary or not"
     -- requires streamlining of this Sel to be useful in future, but may well be so.
     unused :: Boolean
   , inert :: Boolean
   , sel :: SelState a
   }

-- note that I/ T basically just a bool, done solely for 
--data 𝕀 = IInert | INone
--data 𝕋 = TSecondary | TPrimary
--data ℝ = RNone | RSecondary | RPrimary
data 𝕊 = None | Secondary | Primary
type Selectable a = a × SelState 𝕊

isPrimary :: SelState 𝕊 -> 𝔹
isPrimary (SelState { persistent, transient }) =
   persistent == Primary || transient == Primary

isSecondary :: SelState 𝕊 -> 𝔹
isSecondary (SelState { persistent, transient }) =
   persistent == Secondary || transient == Secondary

isNone :: SelState 𝕊 -> 𝔹
isNone sel = not (isPersistent sel || isTransient sel)

--isInert :: SelState 𝕊 -> 𝔹
--isInert (SelState { persistent, transient }) =
--  persistent == Inert || transient == Inert

isPersistent :: SelState 𝕊 -> 𝔹
isPersistent (SelState { persistent }) = persistent /= None

isTransient :: SelState 𝕊 -> 𝔹
isTransient (SelState { transient }) = transient /= None

-- UI sometimes merges 𝕊 values, e.g. x and y coordinates in a scatter plot
compare' :: 𝕊 -> 𝕊 -> Ordering
--compare' Inert Inert = EQ
--compare' Inert _ = LT
--compare' None Inert = GT
compare' None None = EQ
compare' None _ = LT
compare' Secondary Secondary = EQ
compare' Secondary Primary = LT
compare' Secondary _ = GT
compare' Primary Primary = EQ
compare' Primary _ = GT

instance Eq 𝕊 where
   eq s s' = compare' s s' == EQ

instance Ord 𝕊 where
   compare = compare'

instance JoinSemilattice 𝕊 where
   join = max

to𝔹 :: SelState 𝕊 -> SelState 𝔹
to𝔹 = (_ <#> (_ /= None))

to𝕊 :: SelState 𝔹 -> SelState 𝕊
to𝕊 = (_ <#> if _ then Primary else None)

--this assumes we know what inert is.


fromℝ :: ReactState 𝕊 -> SelState 𝕊
fromℝ Inert = (SelState { persistent: None, transient: None })
fromℝ (Reactive sel) = sel

-- Turn previous selection state + new state obtained via related outputs/inputs into primary/secondary sel
-- in place currently selected
as𝕊 :: SelState 𝔹 -> SelState 𝔹 -> SelState 𝕊
as𝕊 = lift2 as𝕊'
   where
   as𝕊' :: 𝔹 -> 𝔹 -> 𝕊
   as𝕊' false false = None
   as𝕊' false true = Secondary
   as𝕊' true false = Primary -- the other atS method makes this case a) not run, as lazy compiler, and b) be replaced by Inert
   as𝕊' true true = Primary

-- purely a helper method for asR
at𝕊 :: SelState 𝔹 -> SelState 𝔹 -> SelState 𝕊
at𝕊 = lift2 at𝕊'
   where
   at𝕊' :: 𝔹 -> 𝔹 -> 𝕊
   at𝕊' false false = Primary
   at𝕊' false true = Primary
   at𝕊' true false = None -- just abusing the lift notn and other helper methods to solve this
   at𝕊' true true = Primary
   
toℝ :: 𝔹 -> SelState 𝔹 -> ReactState 𝕊
toℝ true _ = Inert
toℝ false sel = Reactive (to𝕊 sel)

asℝ :: SelState 𝔹 -> SelState 𝔹 -> ReactState 𝕊
asℝ a b = (if c then Inert else Reactive (sel))
   where
   t :: SelState 𝕊
   t = at𝕊 a b

   c :: Boolean
   c = isNone t

   sel :: SelState 𝕊
   sel = as𝕊 a b

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

-- Unpack d3.js data and event type associated with mouse event target.
selectionEventData :: forall a. Event -> a × Selector Val
selectionEventData = eventData &&& type_ >>> selector

eventData :: forall a. Event -> a
eventData = target >>> unsafeEventData
   where
   unsafeEventData :: Maybe EventTarget -> a
   unsafeEventData tgt = (unsafeCoerce $ definitely' tgt).__data__

-- maybe we make inert unselectable
selector :: EventType -> Selector Val
selector = case _ of
   EventType "mousedown" -> (over SelState (report <<< \s -> s { persistent = neg s.persistent }) <$> _)
   EventType "mouseenter" -> (over SelState (report <<< \s -> s { transient = true }) <$> _)
   EventType "mouseleave" -> (over SelState (report <<< \s -> s { transient = false }) <$> _)
   EventType _ -> error "Unsupported event type"
   where
   report = spyWhen tracing.mouseEvent "Setting SelState to " show

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

-- need to consider inert things for this
css
   :: { sel ::
           { transient ::
                { primary :: String
                , secondary :: String
                }
           , persistent ::
                { primary :: String
                , secondary :: String
                }
           }
      }
css =
   { sel:
        { transient:
             { primary: "selected-primary-transient"
             , secondary: "selected-secondary-transient"
             }
        , persistent:
             { primary: "selected-primary-persistent"
             , secondary: "selected-secondary-persistent"
             }
        }
   }

-- Ideally would derive from css.sel
selClasses :: String
selClasses = joinWith " " $
   [ css.sel.transient.primary
   , css.sel.transient.secondary
   , css.sel.persistent.primary
   , css.sel.persistent.secondary
   --more inert shenanigans required
   ]

selClassesFor :: SelState 𝕊 -> String
selClassesFor (SelState s) =
   joinWith " " $ concat
      [ case s.persistent of
           Secondary -> [ css.sel.persistent.secondary ]
           Primary -> [ css.sel.persistent.primary ]
           None -> []
      , case s.transient of
           Secondary -> [ css.sel.transient.secondary ]
           Primary -> [ css.sel.transient.primary ]
           None -> []
      ]

type Attrs = Array (Bind String)

attrs :: Array Attrs -> Object String
attrs = foldl (\kvs -> (kvs `union` _) <<< fromFoldable) empty

-- ======================
-- boilerplate
-- ======================
derive instance Generic 𝕊 _
instance Show 𝕊 where
   show = genericShow

derive instance Newtype (SelState a) _
derive instance Functor SelState

derive instance Ord a => Ord (SelState a)
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
