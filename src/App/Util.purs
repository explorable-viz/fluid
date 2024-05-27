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
import Lattice (class BoundedJoinSemilattice, class JoinSemilattice, 𝔹, bot, neg, (∨))
import Primitive (as, intOrNumber, unpack)
import Primitive as P
import Test.Util.Debug (tracing)
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), Endo, definitely', error, spyWhen, (×))
import Util.Map (filterKeys, get)
import Util.Set (isEmpty)
import Val (class Highlightable, BaseVal(..), DictRep(..), Val(..), highlightIf)
import Web.Event.Event (Event, EventType(..), target, type_)
import Web.Event.EventTarget (EventListener, EventTarget)

type Selector (f :: Type -> Type) = Endo (f (SelState 𝔹)) -- modifies selection state
type HTMLId = String
type Renderer a = RendererSpec a -> EventListener -> Effect Unit
type ViewSelector a = a -> Endo (Selector Val) -- convert mouse event data to view selector

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

bar_fill :: SelState 𝕊 -> Endo String
bar_fill s col = case s of
   SelState { persistent: None } -> col
   _ -> colorShade col (-20)

bar_stroke :: SelState 𝕊 -> Endo String
bar_stroke (SelState { persistent, transient }) col =
   case persistent × transient of
      None × None -> col
      _ -> colorShade col (-70)

point_smallRadius :: Int
point_smallRadius = 2

point_radius :: SelState 𝕊 -> Int
point_radius (SelState { persistent, transient }) =
   case persistent × transient of
      None × None -> point_smallRadius
      _ -> point_smallRadius * 2

point_stroke :: SelState 𝕊 -> Endo String
point_stroke (SelState { persistent, transient }) col =
   case persistent × transient of
      None × None -> col
      _ -> colorShade col (-30)

indexKey :: String
indexKey = "__n"

-- [any record type with only primitive fields] -> 𝕊
record_isUsed :: Dict (Val (SelState 𝕊)) -> Boolean
record_isUsed r =
   not <<< isEmpty $ flip filterKeys r \k ->
      k /= indexKey && selected (not <<< isNone𝕊 <$> (get k r # \(Val α _) -> α))

css
   :: { sel ::
           { unselected :: String
           , selected :: String
           , selected_transient :: String
           , selected_secondary :: String
           , selected_secondary_transient :: String
           }
      }
css =
   { sel:
        { unselected: "unselected" -- delete this
        , selected: "selected"
        , selected_transient: "selected-transient"
        , selected_secondary: "selected-secondary"
        , selected_secondary_transient: "selected-secondary-transient"
        }
   }

-- Ideally would derive this from css.sel
selClasses :: String
selClasses = joinWith " " $
   [ css.sel.selected
   , css.sel.selected_transient
   , css.sel.selected_secondary
   , css.sel.selected_secondary_transient
   ]

cell_classes :: String -> Val (SelState 𝕊) -> String
cell_classes col v
   | col == indexKey = "cell " <> css.sel.unselected
   | isPrimary𝕊 (v # \(Val (SelState α) _) -> α.persistent) = "cell " <> css.sel.selected
   | isPrimary𝕊 (v # \(Val (SelState α) _) -> α.transient) = "cell " <> css.sel.selected_transient
   | isSecondary𝕊 (v # \(Val (SelState α) _) -> α.persistent) = "cell " <> css.sel.selected_secondary
   | isSecondary𝕊 (v # \(Val (SelState α) _) -> α.transient) = "cell " <> css.sel.selected_secondary_transient
   | otherwise = "cell " <> css.sel.unselected

matrix_cell_selClass :: SelState 𝕊 -> String
matrix_cell_selClass (SelState { persistent }) =
   if isPrimary𝕊 persistent then css.sel.selected
   else if isSecondary𝕊 persistent then css.sel.selected_secondary
   else ""

-- Bundle into a record so we can export via FFI
type UIHelpers =
   { val :: forall a. Selectable a -> a
   , selState :: forall a. Selectable a -> SelState 𝕊
   , isNone𝕊 :: 𝕊 -> Boolean
   , isPrimary𝕊 :: 𝕊 -> Boolean
   , isSecondary𝕊 :: 𝕊 -> Boolean
   , colorShade :: String -> Int -> String
   , selClasses :: String
   , barChartHelpers ::
        { bar_fill :: SelState 𝕊 -> Endo String
        , bar_stroke :: SelState 𝕊 -> Endo String
        }
   , lineChartHelpers ::
        { point_smallRadius :: Int
        , point_radius :: SelState 𝕊 -> Int
        , point_stroke :: SelState 𝕊 -> Endo String
        }
   , matrixViewHelpers ::
        { matrix_cell_selClass :: SelState 𝕊 -> String
        }
   , tableViewHelpers ::
        { indexKey :: String
        , record_isUsed :: Dict (Val (SelState 𝕊)) -> 𝔹
        , cell_classes :: String -> Val (SelState 𝕊) -> String
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
   , selClasses
   , barChartHelpers:
        { bar_fill
        , bar_stroke
        }
   , lineChartHelpers:
        { point_smallRadius
        , point_radius
        , point_stroke
        }
   , matrixViewHelpers:
        { matrix_cell_selClass
        }
   , tableViewHelpers:
        { indexKey
        , record_isUsed
        , cell_classes
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
eventData :: forall a. Event -> a × Selector Val
eventData = target >>> unsafeEventData &&& type_ >>> selector
   where
   unsafeEventData :: Maybe EventTarget -> a
   unsafeEventData tgt = (unsafeCoerce $ definitely' tgt).__data__

selector :: EventType -> Selector Val
selector = case _ of
   EventType "mousedown" -> (over SelState (\s -> report "mousedown" (s { persistent = neg s.persistent })) <$> _)
   EventType "mouseenter" -> (over SelState (\s -> report "mouseenter" (s { transient = true })) <$> _)
   EventType "mouseleave" -> (over SelState (\s -> report "mouseleave" (s { transient = false })) <$> _)
   EventType _ -> error "Unsupported event type"
   where
   report = flip (spyWhen tracing.mouseEvent) show

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
