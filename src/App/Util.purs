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
import Data.Tuple (fst)
import DataType (Ctr, cBarChart, cCons, cNil, cSome, f_data)
import Dict (Dict, get)
import Effect (Effect)
import Foreign.Object (update)
import Graph (Vertex)
import Graph.Slice (selectαs)
import Lattice (class Neg, 𝔹, neg, topOf)
import Partial.Unsafe (unsafePartial)
import Primitive (as, intOrNumber)
import Primitive (record) as P
import Util (Endo, type (×), (×), absurd, error, definitely')
import Val (Val(..), matrixUpdate)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)

type HTMLId = String
type Renderer a = HTMLId -> Int -> a -> EventListener -> Effect Unit
type Selector f = f 𝔹 -> f 𝔹 -- modifies selection state
newtype Selector2 f = Selector2 (f Vertex -> Set Vertex) -- specifies selection
type Sel f = f Vertex × Set Vertex
newtype Sel2 f = Sel2 (f Vertex -> Set Vertex) -- specifies selection
newtype Selector3 f = Selector3 (Sel f -> Sel f)
type OnSel = Selector Val -> Effect Unit -- redraw based on modified output selection
type OnSel2 = Selector2 Val -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector Val
type Handler2 = Event -> Selector2 Val

derive instance Newtype (Selector2 f) _
derive instance Newtype (Selector3 f) _

-- Can these be TopOf/BotOf instances?
selectAll :: forall f. Apply f => Foldable f => Selector2 f
selectAll = wrap $ \vα -> selectαs (topOf vα) vα

selectAll' :: forall f. Apply f => Foldable f => Selector3 f
selectAll' = wrap $ \(vα × _) -> vα × selectαs (topOf vα) vα

selectNone :: forall f. Apply f => Foldable f => Selector2 f
selectNone = wrap $ const S.empty

selectNone' :: forall f. Apply f => Foldable f => Selector3 f
selectNone' = wrap $ \(vα × _) -> vα × S.empty

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
instance Reflect (Val Boolean) (Array (Val Boolean)) where
   from (Constr _ c Nil) | c == cNil = []
   from (Constr _ c (u1 : u2 : Nil)) | c == cCons = u1 A.: from u2

-- Selection helpers. TODO: turn these into lenses.
selectMatrixElement :: Int -> Int -> Endo (Selector Val)
selectMatrixElement i j δv (Matrix α r) = Matrix α $ matrixUpdate i j δv r
selectMatrixElement _ _ _ _ = error absurd

selectNth :: Int -> Endo (Selector Val)
selectNth 0 δv (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (δv v : v' : Nil)
selectNth n δv (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (v : selectNth (n - 1) δv v' : Nil)
selectNth _ _ _ = error absurd

selectNthCell :: Int -> Endo 𝔹 -> Selector Val
selectNthCell 0 δα (Constr α c Nil) | c == cNil = Constr (δα α) c Nil
selectNthCell 0 δα (Constr α c (v : v' : Nil)) | c == cCons = Constr (δα α) c (v : v' : Nil)
selectNthCell n δα (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (v : selectNthCell (n - 1) δα v' : Nil)
selectNthCell _ _ _ = error absurd

selectConstr :: Ctr -> Selector Val
selectConstr c' = unsafePartial $ case _ of
   Constr _ c vs | c == c' -> Constr true c vs

selectSome :: Selector Val
selectSome = selectConstr cSome

selectBarChart_data :: Endo (Selector Val)
selectBarChart_data =
   selectConstrArg2 cBarChart 0 <<< selectField2 f_data

toggleCell :: Int -> Int -> Selector Val
toggleCell i j (Matrix α m) = Matrix α (matrixUpdate i j neg m)
toggleCell _ _ _ = error absurd

selectField2 :: Var -> Endo (Selector Val)
selectField2 f selector = unsafePartial $ case _ of
   Record α r -> Record α $ update (selector >>> Just) f r

selectConstrArg2 :: Ctr -> Int -> Endo (Selector Val)
selectConstrArg2 c n sel = unsafePartial $ case _ of
   Constr α c' us | c == c' ->
      let
         us' = definitely' $ do
            u1 <- us !! n
            updateAt n (sel u1) us
      in
         Constr α c us'
