module Val where

import Prelude hiding (absurd, top)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Bindings (Bindings)
import DataType (Ctr)
import Expr (Elim(..), RecDefs)
import Lattice (
   class BoundedSlices, class Expandable, class JoinSemilattice, class Slices,
   𝔹, (∨), botOf, definedJoin, expand, maybeJoin
)
import Util (Endo, type (×), type (+), (≟), (≜), (⪄), absurd, error)

-- one constructor for each PureScript type that appears in an exported operator signature
data Primitive =
   ValOp (Val 𝔹 -> Val 𝔹) |
   IntOp (Int -> Val 𝔹) |
   NumberOp (Number -> Val 𝔹) |
   IntOrNumberOp (Int + Number -> Val 𝔹) |
   StringOp (String -> Val 𝔹) |
   IntOrNumberOrStringOp (Int + Number + String -> Val 𝔹)

-- Only annotate first-order data for now.
data Val a =
   Hole |
   Int a Int |
   Float a Number |
   Str a String |
   Constr a Ctr (List (Val a)) |
   Matrix a (Array (Array (Val a))) (Int × Int) |
   Closure (Env a) (RecDefs a) (Elim a) |
   Primitive a Primitive

-- The annotation on a value.
getα :: Val 𝔹 -> 𝔹
getα Hole             = false
getα (Int α _)        = α
getα (Float α _)      = α
getα (Str α _)        = α
getα (Constr α _ _)   = α
getα (Matrix α _ _)   = α
getα (Primitive α _)  = α
getα (Closure _ _ _)  = error absurd

-- Set the annotation on a value, which may not be a hole.
setα :: 𝔹 -> Endo (Val 𝔹)
setα α Hole               = error absurd
setα α (Int _ n)          = Int α n
setα α (Float _ n)        = Float α n
setα α (Str _ str)        = Str α str
setα α (Primitive _ φ)    = Primitive α φ
setα α (Constr _ c vs)    = Constr α c vs
setα α (Matrix _ vss ij)  = Matrix α vss ij
setα α (Closure _ _ _)    = error absurd

type Env = Bindings Val

-- ======================
-- boilerplate
-- ======================
derive instance functorVal :: Functor Val

instance joinSemilatticeVal :: JoinSemilattice a => JoinSemilattice (Val a) where
   join = definedJoin

instance slicesVal :: JoinSemilattice a => Slices (Val a) where
   maybeJoin Hole v                                   = pure v
   maybeJoin v Hole                                   = pure v
   maybeJoin (Int α n) (Int α' n')                    = Int (α ∨ α') <$> (n ≟ n')
   maybeJoin (Float α n) (Float α' n')                = Float (α ∨ α') <$> (n ≟ n')
   maybeJoin (Str α str) (Str α' str')                = Str (α ∨ α') <$> (str ≟ str')
   maybeJoin (Constr α c vs) (Constr α' c' us)        = Constr (α ∨ α') <$> (c ≟ c') <*> maybeJoin vs us
   maybeJoin (Matrix α vss xy) (Matrix α' vss' xy')   = Matrix (α ∨ α') <$> (maybeJoin vss vss') <*> (xy ≟ xy')
   maybeJoin (Closure ρ δ σ) (Closure ρ' δ' σ')       = Closure <$> maybeJoin ρ ρ' <*> maybeJoin δ δ' <*> maybeJoin σ σ'
   maybeJoin (Primitive α φ) (Primitive α' φ')        = Primitive (α ∨ α') <$> pure φ -- TODO: require φ == φ'
   maybeJoin _ _                                      = Nothing

instance boundedSlices :: JoinSemilattice a => BoundedSlices (Val a) where
   botOf = const Hole

instance valExpandable :: Expandable (Val Boolean) where
   expand v Hole                                = v
   expand Hole v@(Int false n)                  = v
   expand Hole v@(Float false n)                = v
   expand Hole v@(Str false str)                = v
   expand Hole v@(Primitive false φ)            = v
   expand Hole v@(Constr false c vs)            = Constr false c (expand Hole <$> vs)
   expand Hole v@(Matrix false vs xy)           = Matrix false (((<$>) (expand Hole)) <$> vs) xy
   expand Hole v@(Closure ρ δ σ)                = Closure (botOf ρ) (botOf δ) ElimHole
   expand (Int α n) (Int β n')                  = Int (α ⪄ β) (n ≜ n')
   expand (Float α n) (Float β n')              = Float (α ⪄ β) (n ≜ n')
   expand (Str α str) (Str β str')              = Str (α ⪄ β) (str ≜ str')
   expand (Constr α c vs) (Constr β c' vs')     = Constr (α ⪄ β) (c ≜ c') (expand vs vs')
   expand (Matrix α vss xy) (Matrix β vss' xy') = Matrix (α ⪄ β) (expand vss vss') (xy ≜ xy)
   expand (Closure ρ δ σ) (Closure ρ' δ' σ')    = Closure (expand ρ ρ') (expand δ δ') (expand σ σ')
   expand (Primitive α φ) (Primitive β φ')      = Primitive (α ⪄ β) φ -- TODO: require φ = φ'
   expand _ _                                   = error absurd
