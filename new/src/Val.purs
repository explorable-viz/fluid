module Val where

import Prelude hiding (absurd, top)
import Control.Apply (lift2)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Bindings (Bindings, Var)
import DataType (Ctr)
import Expr (Elim, Elim2, RecDefs, RecDefs2)
import Lattice (class Selectable, class Selectable2, Selected, mapα, maybeZipWith, maybeZipWithα)
import Util ((≟), error)

data Primitive =
   IntOp (Int -> Val) -- one constructor for each primitive type we care about

data RawVal =
   Int Int |
   Str String |
   Constr Ctr (List Val) |
   Closure Env RecDefs Elim |
   Primitive Primitive

data RawVal2 a =
   Int2 Int |
   Str2 String |
   Constr2 Ctr (List (Val2 a)) |
   Closure2 (Env2 a) (RecDefs2 a) (Elim2 a) |
   Primitive2 Primitive

data Val = Val Selected RawVal

data Val2 a = Val2 a (RawVal2 a)

derive instance functorRawVal :: Functor RawVal2
derive instance functorVal :: Functor Val2

type Val' = Val2 Selected

val :: RawVal -> Val
val = Val false

type Env = Bindings Val

instance selectablePrimitive :: Selectable Primitive where
   mapα _ = identity
   maybeZipWithα f (IntOp op) (IntOp op') = pure $ IntOp op'

instance selectableVal :: Selectable Val where
   mapα f (Val α u)                       = Val (f α) (mapα f u)
   maybeZipWithα f (Val α r) (Val α' r')  = Val <$> pure (α `f` α') <*> maybeZipWithα f r r'

instance selectableRawVal :: Selectable RawVal where
   mapα _ (Int x)          = Int x
   mapα _ (Str s)          = Str s
   mapα f (Constr c es)    = Constr c $ map (mapα f) es
   mapα f (Closure ρ δ σ)  = Closure (mapα f ρ) (map (mapα f) δ) $ mapα f σ
   mapα f (Primitive φ)    = Primitive $ mapα f φ

   maybeZipWithα f (Int x) (Int x')                   = Int <$> x ≟ x'
   maybeZipWithα f (Str s) (Str s')                   = Str <$> s ≟ s'
   maybeZipWithα f (Constr c es) (Constr c' es') =
      Constr <$> c ≟ c' <*> maybeZipWithα f es es'
   maybeZipWithα f (Closure ρ δ σ) (Closure ρ' δ' σ') =
      Closure <$> maybeZipWithα f ρ ρ' <*> maybeZipWithα f δ δ' <*> maybeZipWithα f σ σ'
   maybeZipWithα f (Primitive φ) (Primitive φ')       = Primitive <$> maybeZipWithα f φ φ'
   maybeZipWithα _ _ _                                = Nothing

instance selectable2Val :: Selectable2 Val2 where
   maybeZipWith f (Val2 α r) (Val2 α' r')  = Val2 <$> pure (α `f` α') <*> maybeZipWith f r r'

instance selectable2RawVal :: Selectable2 RawVal2 where
   maybeZipWith f (Int2 x) (Int2 x')                   = Int2 <$> x ≟ x'
   maybeZipWith f (Str2 s) (Str2 s')                   = Str2 <$> s ≟ s'
   maybeZipWith f (Constr2 c es) (Constr2 c' es') =
      Constr2 <$> c ≟ c' <*> maybeZipWith (error "todo") es es'
   maybeZipWith f (Closure2 ρ δ σ) (Closure2 ρ' δ' σ') =
      Closure2 <$> maybeZipWith f ρ ρ' <*> maybeZipWith (error "todo") δ δ' <*> maybeZipWith f σ σ'
   maybeZipWith f (Primitive2 φ) (Primitive2 φ')       = pure $ Primitive2 φ -- should require φ == φ'
   maybeZipWith _ _ _                                = Nothing

data Bind2 a = Bind2 Var (Maybe (Val2 a))
data Env2 a = Empty2 | Extend2 (Env2 a) (Bind2 a)

infix 6 Bind2 as ↦
infixl 5 Extend2 as :+:

derive instance functorBind :: Functor Bind2
derive instance functorEnv :: Functor Env2

instance semigroupEnv :: Semigroup (Env2 a) where
   append m Empty2          = m
   append m (Extend2 m' kv) = Extend2 (append m m') kv

instance monoidEnv :: Monoid (Env2 a) where
   mempty = Empty2

instance selectableEnv :: Selectable2 Env2 where
   maybeZipWith _ Empty2 Empty2                              = pure Empty2
   maybeZipWith f (Extend2 m (x ↦ v)) (Extend2 m' (y ↦ v'))
      = Extend2 <$> maybeZipWith f m m' <*> ((↦) <$> x ≟ y <*> lift2 (maybeZipWith f) v v')
   maybeZipWith _ _ _                                      = Nothing
