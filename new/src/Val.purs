module Val where

import Prelude hiding (absurd, top)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Bindings (Bindings)
import DataType (Ctr)
import Expr (Elim, RecDefs)
import Lattice (class BoundedJoinSemilattice, class JoinSemilattice, 𝔹, (∨), maybeJoin)
import Util (Endo, type (+), (≟), absurd, error)

-- one constructor for each PureScript type that appears in an exported operator signature
data Primitive =
   ValOp (Val 𝔹 -> Val 𝔹) |
   IntOp (Int -> Val 𝔹) |
   NumberOp (Number -> Val 𝔹) |
   IntOrNumberOp (Int + Number -> Val 𝔹) |
   StringOp (String -> Val 𝔹) |
   IntOrNumberOrStringOp ((Int + Number) + String -> Val 𝔹)

data RawVal a =
   Int Int |
   Float Number |
   Str String |
   Constr Ctr (List (Val a)) |
   Closure (Env a) (RecDefs a) (Elim a) |
   Primitive Primitive

data Val a = Hole | Val a (RawVal a)

val :: RawVal 𝔹 -> Val 𝔹
val = Val false

setα :: 𝔹 -> Endo (Val 𝔹)
setα true Hole    = error absurd
setα false Hole   = Hole
setα α (Val _ u)  = Val α u

type Env = Bindings Val

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
   maybeJoin (Int n) (Int m)                    = Int <$> n ≟ m
   maybeJoin (Float n) (Float m)                = Float <$> n ≟ m
   maybeJoin (Str s) (Str s')                   = Str <$> s ≟ s'
   maybeJoin (Constr c vs) (Constr c' vs')      = Constr <$> c ≟ c' <*> maybeJoin vs vs'
   maybeJoin (Closure ρ δ σ) (Closure ρ' δ' σ') = Closure <$> maybeJoin ρ ρ' <*> maybeJoin δ δ' <*> maybeJoin σ σ'
   maybeJoin (Primitive φ) (Primitive φ')       = pure $ Primitive φ -- should require φ == φ'
   maybeJoin _ _                                = Nothing
