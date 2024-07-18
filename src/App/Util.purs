module App.Util
   ( Attrs
   , SelState(..)
   , Selectable
   , Selector
   , ViewSelector
   , as𝕊
   , attrs
   , class Reflect
   , colorShade
   , compare'
   , css
   , eventData
   , from
   , get_intOrNumber
   , isNone
   , isPersistent
   , isPrimary
   , isSecondary
   , isTransient
   , isChildless
   , persist
   , record
   , runAffs_
   , selClasses
   , selClassesFor
   , selState
   , selected
   , selectionEventData
   , selector
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

-- Selection has two dimensions: persistent/transient and primary/secondary/Childless. An element can be persistently
-- *and* transiently selected at the same time; these need to be visually distinct (so that for example
-- clicking during mouseover visibly changes the state). Primary and secondary also need to be visually
-- distinct but not orthogonal; primary should (visually) subsume secondary. 
-- Childless is for input data lacking a dependency from output data, and should be used for a lack of display
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

-- claim, we don't worry about persistency/transiency of Childless for redundancy purposes, as it never should be selected
-- here we add "Childless" as a state of data (only input) - ideally where output query(top) excludes it, but just adding with no functionality for now
data 𝕊 = Childless | None | Secondary | Primary
type Selectable a = a × SelState 𝕊

isPrimary :: SelState 𝕊 -> 𝔹
isPrimary (SelState { persistent, transient }) =
   persistent == Primary || transient == Primary

isSecondary :: SelState 𝕊 -> 𝔹
isSecondary (SelState { persistent, transient }) =
   persistent == Secondary || transient == Secondary

-- would this imply having to rerun Childless for every hover?
isChildless :: SelState 𝕊 -> 𝔹
isChildless (SelState { persistent, transient }) =
   persistent == Childless || transient == Childless

isNone :: SelState 𝕊 -> 𝔹
isNone sel = not (isPersistent sel || isTransient sel)

isPersistent :: SelState 𝕊 -> 𝔹
isPersistent (SelState { persistent }) = persistent /= None

isTransient :: SelState 𝕊 -> 𝔹
isTransient (SelState { transient }) = transient /= None

-- UI sometimes merges 𝕊 values, e.g. x and y coordinates in a scatter plot
compare' :: 𝕊 -> 𝕊 -> Ordering
--compare' None None = EQ
--compare' None _ = LT
--compare' Secondary None = GT
--compare' Secondary Secondary = EQ
--compare' Secondary Primary = LT
--compare' Primary Primary = EQ
--compare' Primary _ = GT
-- shouldn't really need this for Childless, as we should never have comparison? - is adding Childless to Boolean algebra necessary
-- if so, then it's lower than none.
-- this is an insensate comparison
compare' Childless Childless = EQ
compare' Childless _ = LT
compare' None Childless = GT
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

--we need this not to change Childless, right? But then how are we expressing this as a boolean?
to𝕊 :: SelState 𝔹 -> SelState 𝕊
to𝕊 = (_ <#> if _ then Primary else None)

--should we have a new variable here, asinit𝕊, say, that could give us an initial value for S of Childless?
as𝕊 :: SelState 𝔹 -> SelState 𝔹 -> SelState 𝕊
as𝕊 = lift2 as𝕊'
   where
   as𝕊' :: 𝔹 -> 𝔹 -> 𝕊
   as𝕊' false false = None
   as𝕊' false true = Secondary
   as𝕊' true false = Childless -- "costless output/ Childless input", but ignore those for now
   as𝕊' true true = Primary

-- so: despite not having a nice use case for Childless, this is OK since we'll never update that data.
-- doesn't work if we want to make some data not-Childless, but ah well
-- Turn previous selection state + new state obtained via related outputs/inputs into primary/secondary sel
-- this code is where we end up redefining everything to be "none", rather than Prim/Sec/Childless every time it is used
-- are we going for a "if Childless, then don't bother checking?"
-- here we have code if we want to alter fig to check if Childless every time, but this isn't necessary bar for completeness.
--as𝕊 :: SelState 𝕊 -> SelState 𝔹 -> SelState 𝔹 -> SelState 𝕊
--as𝕊 = lift3 as𝕊'
-- where
--as𝕊' :: 𝕊 -> 𝔹 -> 𝔹 -> 𝕊
-- as𝕊' :: 𝔹 -> 𝔹 -> 𝕊 
-- does the ordering actually work like this, or do we need to repeat lines with None, Secondary and Primary?
-- as𝕊' Childless _ _ = Childless
-- as𝕊' None false false = None
-- as𝕊' Primary false false = None
--as𝕊' Secondary false false = None
-- as𝕊' None false true = Secondary
--as𝕊' Primary false true = Secondary
-- as𝕊' Secondary false true = Secondary
--as𝕊' None true false = Primary -- "costless output", but ignore those for now
-- as𝕊' Primary true false = Primary -- "costless output", but ignore those for now
--  as𝕊' Secondary true false = Primary -- "costless output", but ignore those for now
--  as𝕊' None true true = Primary
-- as𝕊' Primary true true = Primary
-- as𝕊' Secondary true true = Primary
--  as𝕊' _ false true = Secondary
-- as𝕊' _ true false = Primary -- "costless output", but ignore those for now
-- as𝕊' _ true true = Primary

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

css
   :: { sel ::
           { transient ::
                { primary :: String
                , secondary :: String
                , childless :: String
                }
           , persistent ::
                { primary :: String
                , secondary :: String
                , childless :: String
                }
           }
      }
css =
   { sel:
        { transient:
             { primary: "selected-primary-transient"
             , secondary: "selected-secondary-transient"
             , childless: "selected-childless-transient"
             }
        , persistent:
             { primary: "selected-primary-persistent"
             , secondary: "selected-secondary-persistent"
             , childless: "selected-childless-persistent"
             }
        }
   }

-- Ideally would derive from css.sel
selClasses :: String
selClasses = joinWith " " $
   [ css.sel.transient.primary
   , css.sel.transient.secondary
   , css.sel.transient.childless
   , css.sel.persistent.primary
   , css.sel.persistent.secondary
   , css.sel.persistent.childless
   ]

selClassesFor :: SelState 𝕊 -> String
selClassesFor (SelState s) =
   joinWith " " $ concat
      [ case s.persistent of
           Secondary -> [ css.sel.persistent.secondary ]
           Primary -> [ css.sel.persistent.primary ]
           None -> []
           Childless -> [ css.sel.persistent.childless ]
      , case s.transient of
           Secondary -> [ css.sel.transient.secondary ]
           Primary -> [ css.sel.transient.primary ]
           None -> []
           Childless -> [ css.sel.transient.childless ]
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
