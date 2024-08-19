module App.Util where

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
import Util (type (×), Endo, Setter, definitely', error, spyWhen)
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
-- We implement ReactState, then TelState to possibly include none in a different location.

newtype SelState a = SelState
   { persistent :: a
   , transient :: a
   }

instance (Highlightable a, JoinSemilattice a) => Highlightable (SelState a) where
   highlightIf (SelState { persistent, transient }) = highlightIf (persistent ∨ transient)

persist :: forall a. Setter (SelState a) a
persist δα = \(SelState s) -> SelState (s { persistent = δα s.persistent })

{-}
--   where s = SelState {persistent: s, transient: t}

perrsist :: forall a. Endo a -> Endo (ReactState a)
perrsist δα = \sel ->
   case sel of
      Reactive (SelState s) -> Reactive (SelState { persistent: δα s.persistent, transient: s.transient })
      Inert -> Inert

kindOfBot :: ReactState 𝔹 -> ReactState 𝔹
kindOfBot (Inert) = Inert
kindOfBot (Reactive (SelState _)) = Reactive (SelState { persistent: false, transient: false })

kindOfTop :: ReactState 𝔹 -> ReactState 𝔹
kindOfTop (Inert) = Inert
kindOfTop (Reactive (SelState _)) = Reactive (SelState { persistent: true, transient: true })
-}
selState :: forall a. a -> a -> SelState a
selState b1 b2 = SelState { persistent: b1, transient: b2 }

reactState :: 𝔹 -> 𝔹 -> 𝔹 -> ReactState 𝔹
reactState true _ _ = Inert
reactState false b1 b2 = Reactive (SelState { persistent: b1, transient: b2 })

data ReactState a = Inert | Reactive (SelState a)

data 𝕊 = None | Secondary | Primary

type Selectable a = a × ReactState 𝕊

isPrimary :: ReactState 𝕊 -> 𝔹
isPrimary (Reactive (SelState { persistent, transient })) =
   persistent == Primary || transient == Primary
isPrimary Inert = false

isSecondary :: ReactState 𝕊 -> 𝔹
isSecondary (Reactive (SelState { persistent, transient })) =
   persistent == Secondary || transient == Secondary
isSecondary Inert = false

isNone :: ReactState 𝕊 -> 𝔹
isNone (Reactive (SelState { persistent, transient })) =
   persistent == None && transient == None
isNone _ = false

isInert :: ReactState 𝕊 -> 𝔹
isInert Inert = true
isInert _ = false

isPersistent :: ReactState 𝕊 -> 𝔹
isPersistent (Reactive (SelState { persistent })) = persistent /= None
isPersistent Inert = false

isTransient :: ReactState 𝕊 -> 𝔹
isTransient (Reactive (SelState { transient })) = transient /= None
isTransient Inert = false

-- UI sometimes merges 𝕊 values, e.g. x and y coordinates in a scatter plot
compare' :: 𝕊 -> 𝕊 -> Ordering
compare' None None = EQ
compare' None _ = LT
compare' Secondary Secondary = EQ
compare' Secondary Primary = LT
compare' Secondary None = GT
compare' Primary Primary = EQ
compare' Primary _ = GT

--rather than deriving instances, and just taking inert as bot whenever we derive, directly
instance Eq 𝕊 where
   eq s s' = compare' s s' == EQ

instance Ord 𝕊 where
   compare = compare'

instance JoinSemilattice 𝕊 where
   join = max

instance JoinSemilattice (ReactState 𝕊) where
   join a Inert = a
   join Inert b = b
   join (Reactive (SelState { persistent: a1, transient: b1 })) (Reactive (SelState { persistent: a2, transient: b2 })) = (Reactive (SelState { persistent: a1 ∨ a2, transient: b1 ∨ b2 }))

to𝔹 :: ReactState 𝕊 -> SelState 𝔹
--only used in tests
to𝔹 = ((_ /= None) <$> _) <<< fromℝ

--methods for initial assignation of states
toℝ :: 𝔹 -> SelState 𝔹 -> ReactState 𝕊
toℝ true _ = Inert
toℝ false sel = Reactive (sel <#> if _ then Primary else None)

{-}
cheatToℝ :: forall a. SelState a -> ReactState a
cheatToℝ b = Reactive b

torℝ :: 𝔹 -> ReactState 𝔹 -> ReactState 𝕊
torℝ true _ = Inert
torℝ false sel = sel <#> if _ then Primary else None

asrℝ :: ReactState 𝔹 -> ReactState 𝔹 -> ReactState 𝕊
asrℝ (Inert) _ = (Inert)
asrℝ _ (Inert) = (Inert)
asrℝ (Reactive (SelState { persistent: a1, transient: b1 })) (Reactive (SelState { persistent: a2, transient: b2 })) = (if ((a1 && not a2) || (b1 && not b2)) then Inert else Reactive (lift2 asr𝕊' a b))
   where
   a = SelState { persistent: a1, transient: b1 }
   b = SelState { persistent: a2, transient: b2 }

   asr𝕊' :: 𝔹 -> 𝔹 -> 𝕊
   asr𝕊' false false = None
   asr𝕊' false true = Secondary
   asr𝕊' true false = Primary -- the if solves this case, (as you can't be persistent inert and transient not...)
   asr𝕊' true true = Primary
-}
asℝ :: SelState 𝔹 -> SelState 𝔹 -> ReactState 𝕊
asℝ (SelState { persistent: a1, transient: b1 }) (SelState { persistent: a2, transient: b2 }) = (if ((a1 && not a2) || (b1 && not b2)) then Inert else Reactive (lift2 as𝕊' a b))
   where
   a = (SelState { persistent: a1, transient: b1 })
   b = (SelState { persistent: a2, transient: b2 })

   as𝕊' :: 𝔹 -> 𝔹 -> 𝕊
   as𝕊' false false = None
   as𝕊' false true = Secondary
   as𝕊' true false = Primary -- the if solves this case, (as you can't be persistent inert and transient not...)
   as𝕊' true true = Primary

arℝ :: ReactState 𝔹 -> ReactState 𝔹 -> ReactState 𝕊
arℝ Inert _ = Inert
arℝ _ Inert = Inert
arℝ (Reactive (SelState { persistent: a1, transient: b1 })) (Reactive (SelState { persistent: a2, transient: b2 })) = (if ((a1 && not a2) || (b1 && not b2)) then Inert else Reactive (SelState { persistent: cross a1 a2, transient: cross b1 b2 }))
   where
   cross :: 𝔹 -> 𝔹 -> 𝕊
   cross false false = None
   cross false true = Secondary
   cross true false = Primary -- the if solves this case, (as you can't be persistent inert and transient not...)
   cross true true = Primary

-- TO FIX/REMOVE/OTHERWISE ALTER

fromℝ :: ReactState 𝕊 -> SelState 𝕊
fromℝ Inert = (SelState { persistent: None, transient: None })
fromℝ (Reactive sel) = sel

{-}
fromℝ𝔹 :: ReactState 𝔹 -> SelState 𝔹
fromℝ𝔹 Inert = (SelState { persistent: false, transient: false })
fromℝ𝔹 (Reactive sel) = sel
-}
get_intOrNumber :: Var -> Dict (Val (ReactState 𝕊)) -> Selectable Number
get_intOrNumber x r = first as (unpack intOrNumber (get x r))

-- Assumes fields are all of primitive type.
record :: forall a. (Dict (Val (ReactState 𝕊)) -> a) -> Val (ReactState 𝕊) -> a
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

instance Reflect (Val (ReactState 𝕊)) (Dict (Val (ReactState 𝕊))) where
   from (Val _ (Dictionary (DictRep d))) = d <#> snd

instance Reflect (Val (ReactState 𝕊)) (Array (Val (ReactState 𝕊))) where
   from (Val _ (Constr c Nil)) | c == cNil = []
   from (Val _ (Constr c (u1 : u2 : Nil))) | c == cCons = u1 A.: from u2

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
selector (EventType ev) = (over SelState (report <<< setSel ev) <$> _)
   where
   setSel :: String -> Endo { persistent :: 𝔹, transient :: 𝔹 }
   setSel s sel
      | s == "mousedown" = sel { persistent = neg sel.persistent }
      | s == "mouseenter" = sel { transient = true }
      | s == "mouseleave" = sel { transient = false }
      | otherwise = error "Unsupported event type"
   report = spyWhen tracing.mouseEvent "Setting SelState to " show

{-}
relector :: EventType -> Relector Val
relector = case _ of
   EventType "mousedown" -> (over Reactive (SelState (report <<< \s -> s { persistent = neg s.persistent })) <$> _) || (over Inert identity)
   EventType "mouseenter" -> (over Reactive (over SelState (report <<< \s -> s { transient = true })) <$> _)
   EventType "mouseleave" -> (over Reactive (over SelState (report <<< \s -> s { transient = false })) <$> _)
   EventType _ -> error "Unsupported event type"
   where
   report = spyWhen tracing.mouseEvent "Setting ReactState to " show

-}

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

selClassesFor :: ReactState 𝕊 -> String
selClassesFor Inert =
   joinWith " " $ concat
      [ [ css.inert ] ]
selClassesFor (Reactive (SelState s)) =
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
derive instance Functor ReactState

instance Apply SelState where
   apply (SelState fs) (SelState s) =
      SelState { persistent: fs.persistent s.persistent, transient: fs.transient s.transient }

{-}
instance Apply ReactState where
   apply (Inert) _ = Inert
   apply _ (Inert) = Inert
   apply (Reactive (SelState fs)) (Reactive (SelState s)) =
      Reactive (SelState { persistent: fs.persistent s.persistent, transient: fs.transient s.transient })
-}
derive instance Ord a => Ord (SelState a)
derive instance Eq a => Eq (SelState a)
derive newtype instance Show a => Show (SelState a)

instance JoinSemilattice a => JoinSemilattice (SelState a) where
   join = over2 SelState \s1 s2 ->
      { persistent: s1.persistent ∨ s2.persistent, transient: s1.transient ∨ s2.transient }

instance BoundedJoinSemilattice a => BoundedJoinSemilattice (SelState a) where
   bot = SelState { persistent: bot, transient: bot }
{-}
instance BoundedMeetSemiLattice ReactState 𝔹 where
   top = \s ->
   case s of
   Inert = Inert
   Reactive (SelState _) = Reactive (SelState { persistent: true, transient: true })

   -- we're still working on the principle that we'll never compare Inert, SelState(), so maybe we let inert win?
   -- therefore, I want to make this new explicit structure, called a pair of lattices

meetReactState :: forall a. BoundedMeetSemilattice a => ReactState a -> ReactState a -> Maybe (ReactState a)
meetReactState Inert Inert = Just Inert
meetReactState (Reactive s1) (Reactive s2) = Just $ Reactive (meetSelState s1 s2)
meetReactState _ _ = Nothing -- Incomparable cases (Inert with Reactive)

instance boundedMeetSemiReactState :: BoundedMeetSemilattice a => BoundedMeetSemilattice (ReactState a) where
  meet x y = fromMaybe (error "Cannot take the meet of different ReactState types") (meetReactState x y)


s1 = SelState { persistent: w, transient: x }
s2 = SelState { persistent: y, transient: z }

a = Reactive s1
b = Reactive s2
i = Inert

result1 = meet a b -- Reactive (SelState { persistent: w∇y, transient: x∇z })
result2 = meet i i -- Inert
result3 = meet a i -- Throws error "Cannot take the meet of different ReactState types"
-}
