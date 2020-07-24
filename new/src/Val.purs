module Val where

import Prelude hiding (absurd, top)
import Data.List (List)
import Data.Maybe (Maybe(..))
import DataType (Ctr)
import Expr (Elim, RecDefs, Var)
import Lattice (class BoundedJoinSemilattice, class JoinSemilattice, 𝔹, (∨), bot, maybeJoin)
import Util (Endo, MayFail, type (×), (×), (≟), report)

data Primitive =
   IntOp (Int -> Val 𝔹) -- one constructor for each primitive type we care about

data RawVal a =
   Int Int |
   Str String |
   Constr Ctr (List (Val a)) |
   Closure (Env a) (RecDefs a) (Elim a) |
   Primitive Primitive

data Val a = Hole | Val a (RawVal a)

val :: RawVal 𝔹 -> Val 𝔹
val = Val false

data Bind a = Bind Var (Val a)
data Env a = Empty | Extend (Env a) (Bind a)

infix 6 Bind as ↦
infixl 5 Extend as :+:
infixl 5 update as ◃

find :: Var -> Env 𝔹 -> MayFail (Val 𝔹)
find x Empty  = report $ "variable " <> x <> " not found"
find x (xs :+: x' ↦ v)
   | x == x'   = pure v
   | otherwise = find x xs

foldEnv :: forall a . (Bind 𝔹 -> Endo a) -> a -> Env 𝔹 -> a
foldEnv f z (ρ :+: x ↦ v)   = f (x ↦ v) $ foldEnv f z ρ
foldEnv _ z Empty           = z

update :: Env 𝔹 -> Bind 𝔹 -> Env 𝔹
update Empty _ = Empty
update (xs :+: x ↦ v) (x' ↦ v')
   | x == x'    = xs :+: x' ↦ v'
   | otherwise  = update xs (x' ↦ v') :+: x ↦ v

splitAt :: Int -> Env 𝔹 -> Env 𝔹 × Env 𝔹
splitAt n ρ
  | n <= 0     = ρ × Empty
  | otherwise  = splitAt' n ρ
   where
   splitAt' :: Int -> Env 𝔹 -> Env 𝔹 × Env 𝔹
   splitAt' _  Empty        = Empty × Empty
   splitAt' 1  (ρ0 :+: xv)  = ρ0 × Extend Empty xv
   splitAt' m  (ρ0 :+: xv)  = ρ' × (ρ'' :+: xv)
      where
      ρ' × ρ'' = splitAt' (m - 1) ρ0

-- ======================
-- boilerplate
-- ======================

derive instance functorRawVal :: Functor RawVal
derive instance functorVal :: Functor Val

instance joinSemilatticeVal :: JoinSemilattice (Val Boolean) where
   maybeJoin Hole v                 = pure v
   maybeJoin v Hole                 = pure v
   maybeJoin (Val α r) (Val α' r')  = Val <$> pure (α ∨ α') <*> maybeJoin r r'

instance boundedJoinSemilattice :: BoundedJoinSemilattice (Val Boolean) where
   bot = const Hole

instance joinSemilatticeRawVal :: JoinSemilattice (RawVal Boolean) where
   maybeJoin (Int n) (Int m)                   = Int <$> n ≟ m
   maybeJoin (Str s) (Str s')                   = Str <$> s ≟ s'
   maybeJoin (Constr c vs) (Constr c' vs')      = Constr <$> c ≟ c' <*> maybeJoin vs vs'
   maybeJoin (Closure ρ δ σ) (Closure ρ' δ' σ') = Closure <$> maybeJoin ρ ρ' <*> maybeJoin δ δ' <*> maybeJoin σ σ'
   maybeJoin (Primitive φ) (Primitive φ')       = pure $ Primitive φ -- should require φ == φ'
   maybeJoin _ _                                = Nothing

derive instance functorBind :: Functor Bind
derive instance functorEnv :: Functor Env

instance semigroupEnv :: Semigroup (Env a) where
   append ρ Empty          = ρ
   append ρ (Extend ρ' kv) = Extend (append ρ ρ') kv

instance monoidEnv :: Monoid (Env a) where
   mempty = Empty

instance joinSemilatticeEnv :: JoinSemilattice (Env Boolean) where
   maybeJoin Empty Empty                             = pure Empty
   maybeJoin (Extend ρ (x ↦ v)) (Extend ρ' (y ↦ v')) = Extend <$> maybeJoin ρ ρ' <*> ((↦) <$> x ≟ y <*> maybeJoin v v')
   maybeJoin _ _                                     = Nothing

instance boundedJoinSemilatticeEnv :: BoundedJoinSemilattice (Env Boolean) where
   bot Empty = Empty
   bot (Extend ρ (x ↦ v)) = Extend (bot ρ) (x ↦ bot v)
