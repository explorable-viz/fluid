module App.Util where

import Prelude hiding (absurd)

import Bindings (Var)
import Data.Array ((:)) as A
import Data.List (List(..), (:), (!!), updateAt)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (first)
import Data.Set (Set, union)
import Data.Set as S
import Data.Tuple (fst)
import DataType (Ctr, cBarChart, cCons, cNil, cPair, cSome, f_data, f_y)
import Dict (Dict, get)
import Effect (Effect)
import Foreign.Object (update)
import Graph (Vertex)
import Lattice (𝔹, botOf, neg)
import Primitive (as, intOrNumber)
import Primitive (record) as P
import Util (Endo, type (×), absurd, error, definitely', successful)
import Val (Val(..), addr, matrixGet, matrixUpdate)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)

type HTMLId = String
type Renderer a = HTMLId -> Int -> a -> EventListener -> Effect Unit
type Selector f = f 𝔹 -> f 𝔹
newtype Selector2 f = Selector2 (f Vertex -> Set Vertex)
type OnSel = Selector Val -> Effect Unit -- redraw based on modified output selection
type Handler = Event -> Selector Val

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
selectMatrixElement :: Int -> Int -> Endo (Selector Val)
selectMatrixElement i j δv (Matrix α r) = Matrix α $ matrixUpdate i j δv r
selectMatrixElement _ _ _ _ = error absurd

selectMatrixElement2 :: Int -> Int -> Selector2 Val
selectMatrixElement2 i j = Selector2 $ case _ of
   Matrix _ r -> S.singleton (addr v)
      where
      v = successful (matrixGet i j r) :: Val Vertex
   _ -> error absurd

selectNth :: Int -> Endo (Selector Val)
selectNth 0 δv (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (δv v : v' : Nil)
selectNth n δv (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (v : selectNth (n - 1) δv v' : Nil)
selectNth _ _ _ = error absurd

selectNthCell :: Int -> Endo 𝔹 -> Selector Val
selectNthCell 0 δα (Constr α c Nil) | c == cNil = Constr (δα α) c Nil
selectNthCell 0 δα (Constr α c (v : v' : Nil)) | c == cCons = Constr (δα α) c (v : v' : Nil)
selectNthCell n δα (Constr α c (v : v' : Nil)) | c == cCons = Constr α c (v : selectNthCell (n - 1) δα v' : Nil)
selectNthCell _ _ _ = error absurd

selectSome :: Selector Val
selectSome (Constr _ c vs) | c == cSome = Constr true c (botOf <$> vs)
selectSome _ = error absurd

selectSome2 :: Selector2 Val
selectSome2 = Selector2 $ case _ of
   (Constr α c _) | c == cSome -> S.singleton α
   _ -> error absurd

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
