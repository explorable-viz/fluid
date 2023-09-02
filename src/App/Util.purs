module App.Util where

import Prelude hiding (absurd)

import Bindings (Var)
import Data.Array ((:)) as A
import Data.List (List(..), (:), (!!), updateAt)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong (first)
import Data.Set (Set, union)
import Data.Set as S
import Data.Traversable (class Traversable)
import Data.Tuple (fst)
import DataType (Ctr, cBarChart, cCons, cNil, cPair, cSome, f_data, f_y)
import Dict (Dict, get)
import Effect (Effect)
import Foreign.Object (update)
import Graph (Vertex)
import Graph.GraphWriter (alloc, runWithAlloc)
import Graph.Slice (select𝔹s)
import Lattice (𝔹, neg)
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
type Handler = Event -> Selector Val

-- Turn a vertex-based selector into the corresponding constant 𝔹-based selector.
as𝔹Selector :: forall f. Traversable f => Selector2 f -> Selector f
as𝔹Selector (Selector2 sel) v =
   let _ × vα = runWithAlloc 0 (alloc v) in select𝔹s vα (sel vα)

derive instance Newtype (Selector2 f) _

instance Semigroup (Selector2 f) where
   append (Selector2 s1) (Selector2 s2) = Selector2 $ \x -> s1 x `union` s2 x

instance Monoid (Selector2 f) where
   mempty = Selector2 $ const S.empty

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

select_y :: Selector Val -> Selector Val
select_y δv (Record α r) = Record α $ update (δv >>> Just) f_y r
select_y _ _ = error absurd

selectBarChart_data :: Endo (Selector Val)
selectBarChart_data δv (Constr α c (Record β r : Nil)) | c == cBarChart =
   Constr α c (Record β (update (δv >>> Just) f_data r) : Nil)
selectBarChart_data _ _ = error absurd

selectPair :: Endo 𝔹 -> Selector Val -> Selector Val -> Selector Val
selectPair δα δv1 δv2 (Constr α c (v1 : v2 : Nil)) | c == cPair = Constr (δα α) c (δv1 v1 : δv2 v2 : Nil)
selectPair _ _ _ _ = error absurd

-- Togglers. TODO: subsumed by selectors now?
toggleCell :: Int -> Int -> Selector Val
toggleCell i j (Matrix α m) = Matrix α (matrixUpdate i j neg m)
toggleCell _ _ _ = error absurd

toggleField :: Var -> Selector Val -> Selector Val
toggleField f selector (Record α r) = Record α $ update (selector >>> Just) f r
toggleField _ _ _ = error absurd

toggleConstrArg :: Ctr -> Int -> Selector Val -> Selector Val
toggleConstrArg c n selector (Constr α c' us) | c == c' =
   definitely' $ do
      u1 <- us !! n
      us' <- updateAt n (selector u1) us
      pure $ Constr α c us'
toggleConstrArg _ _ _ _ = error absurd
