module App.Util where

import Prelude hiding (absurd, join)

import Bind (Bind, Var)
import Data.Array ((:)) as A
import Data.Array (concat)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int (fromStringAs, hexadecimal, toStringAs)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, over2)
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
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), Endo, Setter, absurd, definitely', error, shapeMismatch)
import Util.Map (get)
import Val (class Highlightable, BaseVal(..), DictRep(..), Val(..), highlightIf)
import Web.Event.Event (Event, EventType(..), target, type_)
import Web.Event.EventTarget (EventTarget)

type Selector (f :: Type -> Type) = Endo (f (SelState 𝔹)) -- modifies selection state
-- Selection has two dimensions: persistent/transient and primary/secondary/inert. An element can be persistently
-- *and* transiently selected at the same time; these need to be visually distinct (so that for example
-- clicking during mouseover visibly changes the state). Primary and secondary also need to be visually
-- distinct but not orthogonal; primary should (visually) subsume secondary.
-- inert is for nodes with no descendants.

newtype SelectionType a = SelectionType
   { persistent :: a
   , transient :: a
   }

instance (Highlightable a, JoinSemilattice a) => Highlightable (SelState a) where
   highlightIf Inert = highlightIf false
   highlightIf (Reactive (SelectionType { persistent, transient })) = highlightIf (persistent ∨ transient)

persist :: forall a. Setter (SelState a) a
persist δα = \sel ->
   case sel of
      Reactive (SelectionType s) -> Reactive (SelectionType { persistent: δα s.persistent, transient: s.transient })
      Inert -> Inert

selState :: 𝔹 -> 𝔹 -> 𝔹 -> SelState 𝔹
selState true _ _ = Inert
selState false b1 b2 = Reactive (SelectionType { persistent: b1, transient: b2 })

data SelState a = Inert | Reactive (SelectionType a)

data 𝕊 = None | Secondary | Primary

type Selectable a = a × SelState 𝕊

isPrimary :: SelState 𝕊 -> 𝔹
isPrimary (Reactive (SelectionType { persistent, transient })) =
   persistent == Primary || transient == Primary
isPrimary Inert = false

isSecondary :: SelState 𝕊 -> 𝔹
isSecondary (Reactive (SelectionType { persistent, transient })) =
   persistent == Secondary || transient == Secondary
isSecondary Inert = false

isNone :: SelState 𝕊 -> 𝔹
isNone (Reactive (SelectionType { persistent, transient })) =
   persistent == None && transient == None
isNone _ = false

isInert :: forall a. SelState a -> 𝔹
isInert Inert = true
isInert _ = false

isPersistent :: SelState 𝕊 -> 𝔹
isPersistent = getPersistent >>> to𝔹

isTransient :: SelState 𝕊 -> 𝔹
isTransient = getTransient >>> to𝔹

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

instance BoundedJoinSemilattice 𝕊 where
   bot = None

toR𝔹 :: SelState 𝕊 -> SelState 𝔹
toR𝔹 Inert = Inert
toR𝔹 a = selState false (isPersistent a) (isTransient a)

-- methods for obtaining the SelState, designed to accept varying type inputs for redundancy
as𝕊 :: SelState 𝔹 -> SelState 𝔹 -> SelState 𝕊
as𝕊 Inert _ = Inert
as𝕊 _ Inert = Inert
as𝕊 (Reactive (SelectionType { persistent: a1, transient: b1 })) (Reactive (SelectionType { persistent: a2, transient: b2 })) = (if ((a1 && not a2) || (b1 && not b2)) then Inert else Reactive (SelectionType { persistent: cross a1 a2, transient: cross b1 b2 }))
   where
   cross :: 𝔹 -> 𝔹 -> 𝕊
   cross false false = None
   cross false true = Secondary
   cross true false = Primary -- the if solves this case, (as you can't be persistent inert and transient not...)
   cross true true = Primary

to𝕊 :: SelState 𝔹 -> SelState 𝕊
to𝕊 Inert = Inert
to𝕊 (Reactive (sel)) = Reactive (sel <#> if _ then Primary else None)

to𝔹 :: 𝕊 -> 𝔹
to𝔹 None = false
to𝔹 _ = true

getPersistent :: forall a. BoundedJoinSemilattice a => SelState a -> a
getPersistent Inert = bot
getPersistent (Reactive (SelectionType { persistent })) = persistent

getTransient :: forall a. BoundedJoinSemilattice a => SelState a -> a
getTransient Inert = bot
getTransient (Reactive (SelectionType { transient })) = transient

get_intOrNumber :: Var -> Dict (Val (SelState 𝕊)) -> Selectable Number
get_intOrNumber x r = first as (unpack intOrNumber (get x r))

-- Assumes fields are all of primitive type.
record :: forall a. (Dict (Val (SelState 𝕊)) -> a) -> Val (SelState 𝕊) -> a
record toRecord (Val _ v) = toRecord (P.record2.unpack v)

class Reflect a b where
   from :: Partial => a -> b

instance Reflect (Val (SelState 𝕊)) (Dict (Val (SelState 𝕊))) where
   from (Val _ (Dictionary (DictRep d))) = d <#> snd

instance Reflect (Val (SelState 𝕊)) (Array (Val (SelState 𝕊))) where
   from (Val _ (Constr c Nil)) | c == cNil = []
   from (Val _ (Constr c (u1 : u2 : Nil))) | c == cCons = u1 A.: from u2

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
selector (EventType ev) = (setSel ev <$> _)
   where
   setSel :: String -> SelState 𝔹 -> SelState 𝔹
   setSel _ Inert = Inert
   setSel "mousedown" (Reactive (SelectionType { persistent: a, transient: b })) = Reactive (SelectionType { persistent: neg a, transient: b })
   setSel "mouseenter" (Reactive (SelectionType { persistent: a, transient: _ })) = Reactive (SelectionType { persistent: a, transient: true })
   setSel "mouseleave" (Reactive (SelectionType { persistent: a, transient: _ })) = Reactive (SelectionType { persistent: a, transient: false })
   setSel _ _ = error "Unsupported event type"

--report = spyWhen tracing.mouseEvent "Setting SelectionType to " show <<< cheatToSel

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

selClasses :: String
selClasses = joinWith " " $
   [ css.sel.transient.primary
   , css.sel.transient.secondary
   , css.sel.persistent.primary
   , css.sel.persistent.secondary
   , css.inert
   ]

selClassesFor :: SelState 𝕊 -> String
selClassesFor Inert =
   joinWith " " $ concat
      [ [ css.inert ] ]
selClassesFor t =
   joinWith " " $ concat
      [ case (getPersistent t) of
           Secondary -> [ css.sel.persistent.secondary ]
           Primary -> [ css.sel.persistent.primary ]
           None -> []
      , case (getTransient t) of
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

derive instance Newtype (SelectionType a) _

derive instance Functor SelectionType
derive instance Functor SelState

instance Apply SelectionType where
   apply (SelectionType fs) (SelectionType s) =
      SelectionType { persistent: fs.persistent s.persistent, transient: fs.transient s.transient }

instance Apply SelState where
   apply Inert Inert = Inert
   apply (Reactive (SelectionType fs)) (Reactive (SelectionType s)) =
      Reactive (SelectionType { persistent: fs.persistent s.persistent, transient: fs.transient s.transient })
   apply _ _ = shapeMismatch unit

derive instance Ord a => Ord (SelectionType a)
derive instance Eq a => Eq (SelectionType a)
derive newtype instance Show a => Show (SelectionType a)

instance JoinSemilattice a => JoinSemilattice (SelectionType a) where
   join = over2 SelectionType \s1 s2 ->
      { persistent: s1.persistent ∨ s2.persistent, transient: s1.transient ∨ s2.transient }

instance BoundedJoinSemilattice a => BoundedJoinSemilattice (SelectionType a) where
   bot = SelectionType { persistent: bot, transient: bot }

instance JoinSemilattice a => JoinSemilattice (SelState a)
   where
   join Inert Inert = Inert
   join (Reactive b) (Reactive c) = Reactive (b ∨ c)
   join _ _ = error absurd

derive instance Eq a => Eq (SelState a)
