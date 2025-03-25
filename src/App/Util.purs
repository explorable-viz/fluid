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
import Data.Newtype (class Newtype, over, unwrap)
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
import Util (type (×), (×), Endo, Setter, definitely', error, shapeMismatch, spyWhen)
import Util.Map (get)
import Val (class Highlightable, BaseVal(..), DictRep(..), Val(..), highlightIf)
import Web.Event.Event (Event, EventType(..), target, type_)
import Web.Event.EventTarget (EventTarget)

type Selector (f :: Type -> Type) = Endo (f (SelStates 𝔹)) -- modifies selection state
type SetSel a = a -> a × SelectionType -- modifies selection state
-- Selection can occur on data that can be interacted with, reactive data rather than inert data. Within reactive data,
-- selection has two dimensions: persistent or transient. An element can be persistently
-- *and* transiently selected at the same time; these need to be visually distinct (so that for example
-- clicking during mouseover visibly changes the state). Types of selection are primary/secondary/none.
-- These are visually distinct but not orthogonal; primary should (visually) subsume secondary.

data SelState a
   = Inert
   | Reactive a

newtype SelStates a = SelStates (SelState (Selection a))
newtype Selection a = Selection { persistent :: a, transient :: a }

data SelectionType = Persistent | Transient | Unselectable

selStates :: forall a. 𝔹 -> a -> a -> SelStates a
selStates true _ _ = SelStates Inert
selStates false b1 b2 = SelStates $ Reactive $ Selection { persistent: b1, transient: b2 }

selection :: forall a. 𝔹 -> a -> a -> Selection (SelState a)
selection true _ _ = Selection { persistent: Inert, transient: Inert }
selection false b1 b2 = Selection { persistent: Reactive b1, transient: Reactive b2 }

selState :: forall a. 𝔹 -> a -> SelState a
selState true _ = Inert
selState false b = Reactive b

contents :: forall a. Selectable a -> a
contents = fst

sel :: forall a. Selectable a -> SelStates 𝕊
sel = snd

persist :: forall a. Setter (SelStates a) a
persist δα = over SelStates mapδ
   where
   mapδ :: SelState (Selection a) -> SelState (Selection a)
   mapδ Inert = Inert
   mapδ (Reactive s) = Reactive (over Selection (\s' -> s' { persistent = δα s'.persistent }) s)

transition :: forall a. Setter (SelStates a) a
transition δα = over SelStates mapδ
   where
   mapδ :: SelState (Selection a) -> SelState (Selection a)
   mapδ Inert = Inert
   mapδ (Reactive s) = Reactive (over Selection (\s' -> s' { transient = δα s'.transient }) s)

data 𝕊 = None | Secondary | Primary

type Selectable a = a × SelStates 𝕊

splitSelStates :: forall f a. Functor f => f (SelStates a) -> Selection (f (SelState a))
splitSelStates fa = Selection
   { persistent: (splitSel Persistent) <$> fa
   , transient: (splitSel Transient) <$> fa
   }
   where
   splitSel :: SelectionType -> SelStates a -> SelState a
   splitSel _ (SelStates Inert) = Inert
   splitSel Unselectable _ = Inert
   splitSel Persistent (SelStates (Reactive (Selection s))) = Reactive s.persistent
   splitSel Transient (SelStates (Reactive (Selection s))) = Reactive s.transient

mergeSelStates :: forall f a. Functor f => Apply f => Selection (f (SelState a)) -> f (SelStates a)
mergeSelStates (Selection { persistent: ps, transient: ts }) = mergeSel <$> ps <*> ts
   where
   mergeSel :: SelState a -> SelState a -> SelStates a
   mergeSel _ Inert = SelStates Inert
   mergeSel Inert _ = SelStates Inert
   mergeSel (Reactive p) (Reactive t) = SelStates (Reactive $ Selection { persistent: p, transient: t })

distributeSel :: forall f a. Functor f => f (Selection a) -> Selection (f a)
distributeSel fa = Selection
   { persistent: fa <#> unwrap >>> _.persistent
   , transient: fa <#> unwrap >>> _.transient
   }

isPrimary :: SelStates 𝕊 -> 𝔹
isPrimary (SelStates Inert) = false
isPrimary (SelStates (Reactive (Selection { persistent, transient }))) =
   persistent == Primary || transient == Primary

isSecondary :: SelStates 𝕊 -> 𝔹
isSecondary (SelStates Inert) = false
isSecondary (SelStates (Reactive (Selection { persistent, transient }))) =
   persistent == Secondary || transient == Secondary

isInert :: forall a. SelStates a -> 𝔹
isInert (SelStates Inert) = true
isInert (SelStates (Reactive _)) = false

getPersistent :: forall a. BoundedJoinSemilattice a => SelStates a -> a
getPersistent (SelStates Inert) = bot
getPersistent (SelStates (Reactive (Selection { persistent }))) = persistent

to𝔹 :: SelState 𝔹 -> 𝔹
to𝔹 Inert = false
to𝔹 (Reactive b) = b

getTransient :: forall a. BoundedJoinSemilattice a => SelStates a -> a
getTransient (SelStates Inert) = bot
getTransient (SelStates (Reactive (Selection { transient }))) = transient

isPersistent :: SelStates 𝕊 -> 𝔹
isPersistent = getPersistent >>> (_ /= None)

isTransient :: SelStates 𝕊 -> 𝔹
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

unselected :: SelStates 𝔹
unselected = SelStates $ Reactive (Selection { persistent: false, transient: false })

get_intOrNumber :: Var -> Dict (SelStates 𝕊 × Val (SelStates 𝕊)) -> Selectable Number
get_intOrNumber x r = first as (unpack intOrNumber (snd (get x r)))

-- Assumes fields are all of primitive type.
dict :: forall a. (Dict (SelStates 𝕊 × Val (SelStates 𝕊)) -> a) -> Val (SelStates 𝕊) -> a
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

selectionEventData' :: forall a. Event -> a × SetSel (Val (SelState 𝔹))
selectionEventData' = (eventData &&& type_ >>> selector')

eventData :: forall a. Event -> a
eventData = target >>> unsafeEventData
   where
   unsafeEventData :: Maybe EventTarget -> a
   unsafeEventData tgt = (unsafeCoerce $ definitely' tgt).__data__

selector :: EventType -> Selector Val
selector (EventType ev) v =
   reportSelStates <<< setSel <$> reportTarget v
   where
   setSel :: Endo (SelStates 𝔹)
   setSel (SelStates Inert) = SelStates Inert
   setSel (SelStates (Reactive (Selection sel')))
      | ev == "mousedown" = SelStates (Reactive (Selection (sel' { persistent = neg sel'.persistent })))
      | ev == "mouseenter" = SelStates (Reactive (Selection (sel' { transient = true })))
      | ev == "mouseleave" = SelStates (Reactive (Selection (sel' { transient = false })))
      | otherwise = error "Unsupported event type"

   reportSelStates = spyWhen tracing.mouseEvent "to " show
   reportTarget = spyWhen tracing.mouseEvent "Setting selStates of " prettyP

selector' :: EventType -> SetSel (Val (SelState 𝔹))
selector' (EventType ev) v = case ev of
   "mousedown" -> (neg <$> v) × Persistent
   "mouseenter" -> (const (Reactive true) <$> v) × Transient
   "mouseleave" -> (const (Reactive false) <$> v) × Transient
   _ -> error "Unsupported event type"

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

selClassesFor :: SelStates 𝕊 -> String
selClassesFor (SelStates Inert) = css.inert
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

derive instance Eq SelectionType

derive instance Functor SelState
derive instance Functor SelStates
derive instance Functor Selection
derive instance Generic (SelStates a) _
derive instance Generic (SelState a) _
derive instance Generic (Selection a) _

derive instance Newtype (Selection a) _
derive instance Newtype (SelStates a) _

instance Show a => Show (Selection a) where
   show = genericShow

instance Show a => Show (SelState a) where
   show = genericShow

instance Show a => Show (SelStates a) where
   show = genericShow

instance Apply SelState where
   apply Inert Inert = Inert
   apply (Reactive fs) (Reactive s) =
      Reactive (fs s)
   apply _ _ = shapeMismatch unit

instance Apply Selection where
   apply (Selection { persistent, transient }) (Selection { persistent: persistent', transient: transient' }) =
      Selection { persistent: persistent persistent', transient: transient transient' }

instance Apply SelStates where
   apply (SelStates fs) (SelStates s) = SelStates (map (<*>) fs <*> s)

instance JoinSemilattice a => JoinSemilattice (SelState a)
   where
   join s Inert = s
   join Inert s = s
   join (Reactive s) (Reactive s') =
      Reactive (s ∨ s')

instance JoinSemilattice a => JoinSemilattice (Selection a) where
   join (Selection { persistent, transient }) (Selection { persistent: persistent', transient: transient' }) =
      Selection { persistent: persistent ∨ persistent', transient: transient ∨ transient' }

instance JoinSemilattice a => JoinSemilattice (SelStates a) where
   join (SelStates s) (SelStates s') = SelStates (s ∨ s')

instance MeetSemilattice a => MeetSemilattice (SelState a)
   where
   meet _ Inert = Inert
   meet Inert _ = Inert
   meet (Reactive s) (Reactive s') =
      Reactive (s ∧ s')

instance MeetSemilattice a => MeetSemilattice (Selection a) where
   meet (Selection { persistent, transient }) (Selection { persistent: persistent', transient: transient' }) =
      Selection { persistent: persistent ∧ persistent', transient: transient ∧ transient' }

instance MeetSemilattice a => MeetSemilattice (SelStates a)
   where
   meet (SelStates s) (SelStates s') = SelStates (s ∧ s')

instance BoundedJoinSemilattice a => BoundedJoinSemilattice (SelState a)
   where
   bot = Inert

instance BoundedJoinSemilattice a => BoundedJoinSemilattice (Selection a) where
   bot = Selection { persistent: bot, transient: bot }

instance BoundedJoinSemilattice a => BoundedJoinSemilattice (SelStates a)
   where
   bot = SelStates bot

instance (Bounded a, BoundedMeetSemilattice a) => BoundedMeetSemilattice (SelState a)
   where
   top = Reactive top

instance (Bounded a, BoundedMeetSemilattice a) => BoundedMeetSemilattice (SelStates a)
   where
   top = SelStates (Reactive (Selection { persistent: top, transient: top }))

derive instance Eq a => Eq (Selection a)

derive instance Eq a => Eq (SelState a)

derive instance Eq a => Eq (SelStates a)

instance (Highlightable a, JoinSemilattice a) => Highlightable (SelState a) where
   highlightIf Inert = highlightIf false
   highlightIf (Reactive s) = highlightIf s

instance (Highlightable a, JoinSemilattice a) => Highlightable (Selection a) where
   highlightIf (Selection { persistent, transient }) = highlightIf (persistent ∨ transient)

instance (Highlightable a, JoinSemilattice a) => Highlightable (SelStates a) where
   highlightIf (SelStates s) = highlightIf s

derive instance Newtype (Dimensions a) _
derive instance Functor Dimensions
derive instance Generic (Dimensions a) _
instance Show a => Show (Dimensions a) where
   show = genericShow

instance Reflect (Val (SelStates 𝕊)) (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) where
   from (Val _ (Dictionary (DictRep d))) = d

instance Reflect (Val (SelStates 𝕊)) (Array (Val (SelStates 𝕊))) where
   from (Val _ (Constr c Nil)) | c == cNil = []
   from (Val _ (Constr c (u1 : u2 : Nil))) | c == cCons = u1 A.: from u2

instance Reflect (Dict (SelStates 𝕊 × Val (SelStates 𝕊))) (Dimensions (Selectable Int)) where
   from r = Dimensions
      { width: unpack int (snd (get "width" r))
      , height: unpack int (snd (get "height" r))
      }
