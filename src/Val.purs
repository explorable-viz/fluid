module Val where

import Prelude hiding (absurd)
import Control.Apply (lift2)
import Data.Array (replicate)
import Data.List (List)
import Bindings (Bindings)
import DataType (Ctr)
import Expr (Elim(..), RecDefs)
import Lattice (
   class BoundedJoinSemilattice, class BoundedSlices, class Expandable, class JoinSemilattice, class Slices,
   𝔹, (∨), bot, botOf, definedJoin, expand, maybeJoin
)
import Util (Endo, type (×), (×), (⪄), (≞), (≜), (!), absurd, error, report, unsafeUpdateAt)

type Op a = a × 𝔹 -> Val 𝔹

data Val a =
   Hole a |
   Int a Int |
   Float a Number |
   Str a String |
   Constr a Ctr (List (Val a)) |       -- potentially unsaturated
   Matrix a (MatrixRep a) |
   Primitive PrimOp (List (Val a)) |   -- always unsaturated
   Closure (Env a) (RecDefs a) (Elim a)

-- op_fwd will be provided with original (non-hole) arguments, op_bwd with original output and arguments
newtype PrimOp = PrimOp {
   arity :: Int,
   op :: List (Val 𝔹) -> Val 𝔹,
   op_fwd :: List (Val 𝔹 × Val 𝔹) -> Val 𝔹,
   op_bwd :: Val 𝔹 × Val 𝔹 -> Endo (List (Val 𝔹))
}

type Env = Bindings Val

-- Matrices.
type MatrixRep a = Array (Array (Val a)) × (Int × a) × (Int × a)

insertMatrix :: Int -> Int -> Val 𝔹 -> Endo (MatrixRep 𝔹)
insertMatrix i j v (vss × h × w) =
   let vs_i = vss!(i - 1)
       vss' = unsafeUpdateAt (i - 1) (unsafeUpdateAt (j - 1) v vs_i) vss
   in  vss' × h × w

holeMatrix :: Int -> Int -> MatrixRep 𝔹
holeMatrix i j = replicate i (replicate j (Hole false)) × (i × false) × (j × false)

-- ======================
-- boilerplate
-- ======================
-- derive instance functorVal :: Functor Val

instance joinSemilatticeVal :: JoinSemilattice a => JoinSemilattice (Val a) where
   join = definedJoin

instance slicesVal :: JoinSemilattice a => Slices (Val a) where
   maybeJoin (Hole _) v                               = pure v -- TODO: fix
   maybeJoin v (Hole _)                               = pure v -- TODO: fix
   maybeJoin (Int α n) (Int α' n')                    = Int (α ∨ α') <$> (n ≞ n')
   maybeJoin (Float α n) (Float α' n')                = Float (α ∨ α') <$> (n ≞ n')
   maybeJoin (Str α str) (Str α' str')                = Str (α ∨ α') <$> (str ≞ str')
   maybeJoin (Constr α c vs) (Constr α' c' us)        = Constr (α ∨ α') <$> (c ≞ c') <*> maybeJoin vs us
   maybeJoin (Matrix α (vss × (i × β) × (j × γ))) (Matrix α' (vss' × (i' × β') × (j' × γ'))) =
      Matrix (α ∨ α') <$> (
         maybeJoin vss vss' `lift2 (×)`
         ((flip (×) (β ∨ β')) <$> (i ≞ i')) `lift2 (×)`
         ((flip (×) (γ ∨ γ')) <$> (j ≞ j'))
      )
   maybeJoin (Closure ρ δ σ) (Closure ρ' δ' σ')       = Closure <$> maybeJoin ρ ρ' <*> maybeJoin δ δ' <*> maybeJoin σ σ'
   maybeJoin (Primitive φ vs) (Primitive φ' vs')      = Primitive φ <$> maybeJoin vs vs' -- TODO: require φ == φ'
   maybeJoin _ _                                      = report "Incompatible values"

instance boundedSlices :: BoundedJoinSemilattice a => BoundedSlices (Val a) where
   botOf = const (Hole bot)

instance valExpandable :: Expandable (Val Boolean) where
   expand _ (Hole true)                         = error absurd
   expand v (Hole false)                        = v
   expand (Hole α) (Int β n)                    = Int (α ⪄ β) n
   expand (Hole α) (Float β n)                  = Float (α ⪄ β) n
   expand (Hole α) (Str β str)                  = Str (α ⪄ β) str
   expand (Hole α) (Primitive φ vs)             = Primitive φ (expand (Hole α) <$> vs)
   expand (Hole α) (Constr β c vs)              = Constr (α ⪄ β) c (expand (Hole α) <$> vs)
   expand (Hole α) (Matrix β (vss × (i × β1) × (j × β2))) =
      Matrix (α ⪄ β) ((((<$>) (expand (Hole α))) <$> vss) × (i × (α ⪄ β1)) × (j × (α ⪄ β2)))
   expand (Hole α) (Closure ρ δ σ)              = Closure (expand (botOf ρ) ρ) (expand (botOf δ) δ) (expand ElimHole σ)
   expand (Int α n) (Int β n')                  = Int (α ⪄ β) (n ≜ n')
   expand (Float α n) (Float β n')              = Float (α ⪄ β) (n ≜ n')
   expand (Str α str) (Str β str')              = Str (α ⪄ β) (str ≜ str')
   expand (Constr α c vs) (Constr β c' vs')     = Constr (α ⪄ β) (c ≜ c') (expand vs vs')
   expand (Matrix α (vss × (i × β) × (j × γ))) (Matrix α' (vss' × (i' × β') × (j' × γ'))) =
      Matrix (α ⪄ α') (expand vss vss' × ((i ≜ i') × (β ⪄ β')) × ((j ≜ j') × (γ ⪄ γ')))
   expand (Closure ρ δ σ) (Closure ρ' δ' σ')    = Closure (expand ρ ρ') (expand δ δ') (expand σ σ')
   expand (Primitive φ vs) (Primitive φ' vs')   = Primitive φ (expand vs vs') -- TODO: require φ = φ'
   expand _ _                                   = error absurd
