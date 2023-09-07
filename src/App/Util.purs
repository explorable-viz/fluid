module App.Util where

import Prelude hiding (absurd)

import Bindings (Var)
import Data.Array ((:)) as A
import Data.Foldable (class Foldable)
import Data.List (List(..), (:), (!!), updateAt)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Strong (first)
import Data.Set (Set, difference, union)
import Data.Set as S
import Data.Traversable (class Traversable)
import Data.Tuple (fst)
import DataType (Ctr, cBarChart, cCons, cNil, cPair, cSome, f_data, f_y)
import Dict (Dict, get)
import Effect (Effect)
import Foreign.Object (update)
import Graph (Vertex)
import Graph.GraphWriter (alloc, runWithAlloc)
import Graph.Slice (selectαs, select𝔹s)
import Lattice (class Neg, 𝔹, neg, topOf)
import Partial.Unsafe (unsafePartial)
import Primitive (as, intOrNumber)
import Primitive (record) as P
import Util (Endo, type (×), (×), absurd, error, definitely', successful)
import Val (Val(..), addr, matrixGet, matrixUpdate)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)

type HTMLId = String
type Renderer a = HTMLId -> Int -> a -> EventListener -> Effect Unit
type Selector f = f 𝔹 -> f 𝔹 -- modifies selection state
newtype Selector2 f = Selector2 (f Vertex -> Set Vertex) -- specifies selection
type OnSel = Selector Val -> Effect Unit -- redraw based on modified output selection
type OnSel2 = Selector2 Val -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector Val
type Handler2 = Event -> Selector2 Val

-- Turn vertex selector into corresponding (constant) 𝔹-selector.
as𝔹Selector :: forall f. Traversable f => Selector2 f -> Selector f
as𝔹Selector (Selector2 sel) v =
   let _ × vα = runWithAlloc 0 (alloc v) in select𝔹s vα (sel vα)

-- Can these be TopOf/BotOf instances?
selectAll :: forall f. Apply f => Foldable f => Selector2 f
selectAll = wrap $ \vα -> selectαs (topOf vα) vα

selectNone :: forall f. Apply f => Foldable f => Selector2 f
selectNone = wrap $ const S.empty

derive instance Newtype (Selector2 f) _

instance Semigroup (Selector2 f) where
   append (Selector2 sel1) (Selector2 sel2) = Selector2 $ \x -> sel1 x `union` sel2 x

instance Monoid (Selector2 f) where
   mempty = Selector2 $ const S.empty

instance (Apply f, Foldable f) => Neg (Selector2 f) where
   neg (Selector2 sel) = Selector2 $ \x -> sel x `difference` unwrap selectAll x

doNothing :: OnSel
doNothing = const $ pure unit

get_intOrNumber :: Var -> Dict (Val 𝔹) -> Number × 𝔹
get_intOrNumber x r = first as (intOrNumber.match (get x r))

-- Assumes fields are all of primitive type.
record :: forall a. (Dict (Val 𝔹) -> a) -> Val 𝔹 -> a
record toRecord u = toRecord (fst (P.record.match u))

class Reflect a b where
   from :: Partial => a -> b

-- Discard any constructor-level annotations.
instance reflectArray :: Reflect (Val Boolean) (Array (Val Boolean)) where
   from (Constr _ c Nil) | c == cNil = []
   from (Constr _ c (u1 : u2 : Nil)) | c == cCons = u1 A.: from u2

-- Selection helpers.
selectMatrixElement :: Int -> Int -> Selector2 Val
selectMatrixElement i j = Selector2 $ unsafePartial $ case _ of
   Matrix _ r -> S.singleton (addr v)
      where
      v = successful (matrixGet i j r) :: Val Vertex

selectNth :: Int -> Endo (Selector Val)
selectNth 0 δv (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (δv v : v' : Nil)
selectNth n δv (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (v : selectNth (n - 1) δv v' : Nil)
selectNth _ _ _ = error absurd

selectNth2 :: Int -> Endo (Selector2 Val)
selectNth2 n sel = Selector2 $ unsafePartial $ case _ of
   Constr _ c (v : _ : Nil) | n == 0 && c == cCons -> unwrap sel v
   Constr _ c (_ : v' : Nil) | c == cCons -> unwrap (selectNth2 (n - 1) sel) v'

selectNthCell :: Int -> Selector2 Val
selectNthCell n = Selector2 $ unsafePartial $ case _ of
   Constr α c Nil | n == 0 && c == cNil -> S.singleton α
   Constr α c (_ : _ : Nil) | n == 0 && c == cCons -> S.singleton α
   Constr _ c (_ : v' : Nil) | c == cCons -> unwrap (selectNthCell (n - 1)) v'

selectSome :: Selector2 Val
selectSome = Selector2 $ unsafePartial $ case _ of
   Constr α c _ | c == cSome -> S.singleton α

select_y :: Endo (Selector2 Val)
select_y = selectField f_y

selectBarChart_data :: Endo (Selector2 Val)
selectBarChart_data sel = Selector2 $ unsafePartial $ case _ of
   Constr _ c (v : Nil) | c == cBarChart -> unwrap (selectField f_data sel) v

selectPair :: Selector2 Val -> Selector2 Val -> Selector2 Val
selectPair sel1 sel2 = Selector2 $ unsafePartial $ case _ of
   Constr _ c (v1 : v2 : Nil) | c == cPair -> unwrap sel1 v1 `union` unwrap sel2 v2

-- Togglers. TODO: subsumed by selectors now?
toggleCell :: Int -> Int -> Selector Val
toggleCell i j (Matrix α m) = Matrix α (matrixUpdate i j neg m)
toggleCell _ _ _ = error absurd

toggleField :: Var -> Endo (Selector Val)
toggleField f selector (Record α r) = Record α $ update (selector >>> Just) f r
toggleField _ _ _ = error absurd

selectField :: Var -> Endo (Selector2 Val)
selectField f sel = Selector2 $ unsafePartial $ case _ of
   Record _ r -> unwrap sel (get f r)

toggleConstrArg :: Ctr -> Int -> Endo (Selector Val)
toggleConstrArg c n selector (Constr α c' us) | c == c' =
   let
      us' = definitely' $ do
         u1 <- us !! n
         updateAt n (selector u1) us
   in
      Constr α c us'
toggleConstrArg _ _ _ _ = error absurd

selectConstrArg :: Ctr -> Int -> Endo (Selector2 Val)
selectConstrArg c n sel = Selector2 $ unsafePartial $ case _ of
   Constr _ c' us | c == c' ->
      unwrap sel $ definitely' $ us !! n
