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

-- Selection can occur on data that can be interacted with, reactive data rather than inert data. Within reactive data,
-- selection has two dimensions: persistent or transient. An element can be persistently
-- *and* transiently selected at the same time; these need to be visually distinct (so that for example
-- clicking during mouseover visibly changes the state). Types of selection are primary/secondary/none.
-- These are visually distinct but not orthogonal; primary should (visually) subsume secondary.

data SelState a
   = Inert
   | Reactive
        ( { persistent :: a
          , transient :: a
          }
        )

selState :: 𝔹 -> 𝔹 -> 𝔹 -> SelState 𝔹
selState true _ _ = Inert
selState false b1 b2 = Reactive ({ persistent: b1, transient: b2 })

persist :: forall a. Setter (SelState a) a
persist δα = \sel ->
   case sel of
      Reactive s -> Reactive ({ persistent: δα s.persistent, transient: s.transient })
      Inert -> Inert

data 𝕊 = None | Secondary | Primary

type Selectable a = a × SelState 𝕊

isPrimary :: SelState 𝕊 -> 𝔹
isPrimary (Reactive ({ persistent, transient })) =
   persistent == Primary || transient == Primary
isPrimary Inert = false

isSecondary :: SelState 𝕊 -> 𝔹
isSecondary (Reactive ({ persistent, transient })) =
   persistent == Secondary || transient == Secondary
isSecondary Inert = false

isNone :: SelState 𝕊 -> 𝔹
isNone (Reactive ({ persistent, transient })) =
   persistent == None && transient == None
isNone _ = false

isInert :: forall a. SelState a -> 𝔹
isInert Inert = true
isInert _ = false

getPersistent :: forall a. BoundedJoinSemilattice a => SelState a -> a
getPersistent Inert = bot
getPersistent (Reactive ({ persistent })) = persistent

getTransient :: forall a. BoundedJoinSemilattice a => SelState a -> a
getTransient Inert = bot
getTransient (Reactive ({ transient })) = transient

isPersistent :: SelState 𝕊 -> 𝔹
isPersistent a = getPersistent a /= None

isTransient :: SelState 𝕊 -> 𝔹
isTransient a = getTransient a /= None

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

-- methods for obtaining the SelState, designed to accept varying type inputs for redundancy
as𝕊 :: SelState 𝔹 -> SelState 𝔹 -> SelState 𝕊
as𝕊 Inert Inert = Inert
as𝕊 (Reactive ({ persistent: a1, transient: b1 })) (Reactive ({ persistent: a2, transient: b2 })) = (if ((a1 && not a2) || (b1 && not b2)) then Inert else Reactive ({ persistent: cross a1 a2, transient: cross b1 b2 }))
   where
   cross :: 𝔹 -> 𝔹 -> 𝕊
   cross false false = None
   cross false true = Secondary
   cross true false = error absurd
   cross true true = Primary
as𝕊 _ _ = shapeMismatch unit

to𝕊 :: SelState 𝔹 -> SelState 𝕊
to𝕊 Inert = Inert
to𝕊 (Reactive ({ persistent: a, transient: b })) = Reactive ({ persistent: t a, transient: t b })
   where
   t :: 𝔹 -> 𝕊
   t true = Primary
   t false = None

nullSelState :: SelState 𝔹
nullSelState = Reactive ({ persistent: false, transient: false })

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
   setSel "mousedown" (Reactive ({ persistent: a, transient: b })) = Reactive ({ persistent: neg a, transient: b })
   setSel "mouseenter" (Reactive ({ persistent: a, transient: _ })) = Reactive ({ persistent: a, transient: true })
   setSel "mouseleave" (Reactive ({ persistent: a, transient: _ })) = Reactive ({ persistent: a, transient: false })
   setSel _ _ = error "Unsupported event type"

--report = spyWhen tracing.mouseEvent "Setting  to " show <<< cheatToSel

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

derive instance Functor SelState

instance Apply SelState where
   apply Inert Inert = Inert
   apply (Reactive (fs)) (Reactive (s)) =
      Reactive ({ persistent: fs.persistent s.persistent, transient: fs.transient s.transient })
   apply _ _ = shapeMismatch unit

instance JoinSemilattice a => JoinSemilattice (SelState a)
   where
   join Inert Inert = Inert
   join (Reactive { persistent: s1, transient: t1 }) (Reactive { persistent: s2, transient: t2 }) = Reactive ({ persistent: s1 ∨ s2, transient: t1 ∨ t2 })
   join _ _ = error absurd

derive instance Eq a => Eq (SelState a)

instance (Highlightable a, JoinSemilattice a) => Highlightable (SelState a) where
   highlightIf Inert = highlightIf false
   highlightIf (Reactive ({ persistent, transient })) = highlightIf (persistent ∨ transient)
