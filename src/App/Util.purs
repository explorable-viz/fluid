module App.Util where

import Prelude hiding (absurd, join)

import Bind (Bind, Var, (↦))
import Data.Array ((:)) as A
import Data.Array (concat)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (fromStringAs, hexadecimal, toStringAs)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong ((&&&), first)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.String.CodeUnits (drop, take)
import Data.Traversable (sequence, sequence_)
import Data.Tuple (fst, snd)
import DataType (cCons, cNil)
import Dict (Dict)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class.Console (log)
import Foreign.Object (Object, empty, fromFoldable, union)
import Lattice (class BoundedJoinSemilattice, class BoundedMeetSemilattice, class JoinSemilattice, class MeetSemilattice, 𝔹, bot, neg, (∧), (∨))
import Pretty (prettyP)
import Primitive (as, int, intOrNumber, unpack)
import Primitive as P
import Test.Util.Debug (tracing)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), Endo, Setter, definitely', error, shapeMismatch, spyWhen)
import Util.Map (get)
import Val (class Highlightable, BaseVal(..), DictRep(..), Val(..), highlightIf)
import Web.Event.Event (Event, EventType(..), target, type_)
import Web.Event.EventTarget (EventTarget)

type Selector (f :: Type -> Type) = Endo (f (SelState 𝔹)) -- modifies selection state

-- Selection can occur on data that can be interacted with, reactive data rather than inert data. Within reactive data,
-- selection has two dimensions: persistent or transient. An element can be persistently
-- *and* transiently selected at the same time; these need to be visually distinct (so that for example
-- clicking during mouseover visibly changes the state). Types of selection are primary/secondary/none.
-- These are visually distinct but not orthogonal; primary should (visually) subsume secondary.

data SelState a
   = Inert
   | Reactive
        { persistent :: a
        , transient :: a
        }

selState :: forall a. 𝔹 -> a -> a -> SelState a
selState true _ _ = Inert
selState false b1 b2 = Reactive { persistent: b1, transient: b2 }

contents :: forall a. Selectable a -> a
contents = fst

sel :: forall a. Selectable a -> SelState 𝕊
sel = snd

persist :: forall a. Setter (SelState a) a
persist δα = case _ of
   Reactive s -> Reactive { persistent: δα s.persistent, transient: s.transient }
   Inert -> Inert

data 𝕊 = None | Secondary | Primary

type Selectable a = a × SelState 𝕊

isPrimary :: SelState 𝕊 -> 𝔹
isPrimary Inert = false
isPrimary (Reactive { persistent, transient }) =
   persistent == Primary || transient == Primary

isSecondary :: SelState 𝕊 -> 𝔹
isSecondary Inert = false
isSecondary (Reactive { persistent, transient }) =
   persistent == Secondary || transient == Secondary

isInert :: forall a. SelState a -> 𝔹
isInert Inert = true
isInert (Reactive _) = false

getPersistent :: forall a. BoundedJoinSemilattice a => SelState a -> a
getPersistent Inert = bot
getPersistent (Reactive { persistent }) = persistent

getTransient :: forall a. BoundedJoinSemilattice a => SelState a -> a
getTransient Inert = bot
getTransient (Reactive { transient }) = transient

isPersistent :: SelState 𝕊 -> 𝔹
isPersistent = getPersistent >>> (_ /= None)

isTransient :: SelState 𝕊 -> 𝔹
isTransient = getTransient >>> (_ /= None)

-- UI sometimes merges 𝕊 values, e.g. x and y coordinates in a scatter plot
compare' :: 𝕊 -> 𝕊 -> Ordering
compare' None None = EQ
compare' None _ = LT
compare' Secondary Secondary = EQ
compare' Secondary Primary = LT
compare' Secondary None = GT
compare' Primary Primary = EQ
compare' Primary _ = GT

instance Eq 𝕊 where
   eq s s' = compare' s s' == EQ

instance Ord 𝕊 where
   compare = compare'

instance JoinSemilattice 𝕊 where
   join = max

instance MeetSemilattice 𝕊 where
   meet = min

instance BoundedJoinSemilattice 𝕊 where
   bot = None

as𝕊 :: 𝔹 -> 𝔹 -> 𝕊
as𝕊 false false = None
as𝕊 false true = Secondary
as𝕊 true false = None -- this should be error absurd, but see case #
as𝕊 true true = Primary

to𝕊 :: 𝔹 -> 𝕊
to𝕊 true = Primary
to𝕊 false = None

unselected :: SelState 𝔹
unselected = Reactive { persistent: false, transient: false }

get_intOrNumber :: Var -> Dict (SelState 𝕊 × Val (SelState 𝕊)) -> Selectable Number
get_intOrNumber x r = first as (unpack intOrNumber (snd (get x r)))

-- Assumes fields are all of primitive type.
dict :: forall a. (Dict (SelState 𝕊 × Val (SelState 𝕊)) -> a) -> Val (SelState 𝕊) -> a
dict toDict (Val _ v) = toDict (P.dict.unpack v)

class Reflect a b where
   from :: Partial => a -> b

runAffs_ :: forall a. (a -> Effect Unit) -> Array (Aff a) -> Effect Unit
runAffs_ f as = flip runAff_ (sequence as) case _ of
   Left err -> log $ show err
   Right as' -> as' <#> f # sequence_

-- Unpack d3.js data and event type associated with mouse event target.
selectionEventData :: forall a. Event -> a × Selector Val
selectionEventData = (eventData &&& type_ >>> selector)

eventData :: forall a. Event -> a
eventData = target >>> unsafeEventData
   where
   unsafeEventData :: Maybe EventTarget -> a
   unsafeEventData tgt = (unsafeCoerce $ definitely' tgt).__data__

selector :: EventType -> Selector Val
selector (EventType ev) v =
   reportSelState <<< setSel <$> reportTarget v
   where
   setSel :: Endo (SelState 𝔹)
   setSel Inert = Inert
   setSel (Reactive sel')
      | ev == "mousedown" = Reactive (sel' { persistent = neg sel'.persistent })
      | ev == "mouseenter" = Reactive (sel' { transient = true })
      | ev == "mouseleave" = Reactive (sel' { transient = false })
      | otherwise = error "Unsupported event type"

   reportSelState = spyWhen tracing.mouseEvent "to " show
   reportTarget = spyWhen tracing.mouseEvent "Setting selState of " prettyP

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
                }
           , persistent ::
                { primary :: String
                , secondary :: String
                }
           }
      , inert :: String
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
   , inert: "inert"
   }

classes :: Array String -> Bind String
classes = joinWith " " >>> ("class" ↦ _)

selClasses :: String
selClasses = joinWith " " $
   [ css.sel.transient.primary
   , css.sel.transient.secondary
   , css.sel.persistent.primary
   , css.sel.persistent.secondary
   , css.inert
   ]

selClassesFor :: SelState 𝕊 -> String
selClassesFor Inert = css.inert
selClassesFor t =
   joinWith " " $ concat
      [ case getPersistent t of
           Secondary -> [ css.sel.persistent.secondary ]
           Primary -> [ css.sel.persistent.primary ]
           None -> []
      , case getTransient t of
           Secondary -> [ css.sel.transient.secondary ]
           Primary -> [ css.sel.transient.primary ]
           None -> []
      ]

type Attrs = Array (Bind String)

attrs :: Array Attrs -> Object String
attrs = foldl (\kvs -> (kvs `union` _) <<< fromFoldable) empty

newtype Dimensions a = Dimensions
   { width :: a
   , height :: a
   }

-- ======================
-- boilerplate
-- ======================
derive instance Generic 𝕊 _
instance Show 𝕊 where
   show = genericShow

derive instance Functor SelState
derive instance Generic (SelState a) _
instance Show a => Show (SelState a) where
   show = genericShow

instance Apply SelState where
   apply Inert Inert = Inert
   apply (Reactive fs) (Reactive s) =
      Reactive { persistent: fs.persistent s.persistent, transient: fs.transient s.transient }
   apply _ _ = shapeMismatch unit

instance JoinSemilattice a => JoinSemilattice (SelState a)
   where
   join s Inert = s
   join Inert s = s
   join (Reactive s) (Reactive s') =
      Reactive { persistent: s.persistent ∨ s'.persistent, transient: s.transient ∨ s'.transient }

instance MeetSemilattice a => MeetSemilattice (SelState a)
   where
   meet _ Inert = Inert
   meet Inert _ = Inert
   meet (Reactive s) (Reactive s') =
      Reactive { persistent: s.persistent ∧ s'.persistent, transient: s.transient ∧ s'.transient }

instance BoundedJoinSemilattice a => BoundedJoinSemilattice (SelState a)
   where
   bot = Inert

instance (Bounded a, BoundedMeetSemilattice a) => BoundedMeetSemilattice (SelState a)
   where
   top = Reactive { persistent: top, transient: top }

derive instance Eq a => Eq (SelState a)

instance (Highlightable a, JoinSemilattice a) => Highlightable (SelState a) where
   highlightIf Inert = highlightIf false
   highlightIf (Reactive { persistent, transient }) = highlightIf (persistent ∨ transient)

derive instance Newtype (Dimensions a) _
derive instance Functor Dimensions
derive instance Generic (Dimensions a) _
instance Show a => Show (Dimensions a) where
   show = genericShow

instance Reflect (Val (SelState 𝕊)) (Dict (SelState 𝕊 × Val (SelState 𝕊))) where
   from (Val _ (Dictionary (DictRep d))) = d

instance Reflect (Val (SelState 𝕊)) (Array (Val (SelState 𝕊))) where
   from (Val _ (Constr c Nil)) | c == cNil = []
   from (Val _ (Constr c (u1 : u2 : Nil))) | c == cCons = u1 A.: from u2

instance Reflect (Dict (SelState 𝕊 × Val (SelState 𝕊))) (Dimensions (Selectable Int)) where
   from r = Dimensions
      { width: unpack int (snd (get "width" r))
      , height: unpack int (snd (get "height" r))
      }
