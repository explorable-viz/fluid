module Val2 where

import Prelude hiding (absurd)
import Control.Apply (lift2)
import Data.List (List)
import Bindings2 (Bindings)
import DataType2 (Ctr)
import Expr2 (Elim, RecDefs)
import Lattice2 (
   class BoundedSlices, class JoinSemilattice, class Slices, 𝔹, (∨), bot, botOf, definedJoin, maybeJoin, neg
)
import Util2 (Endo, type (×), (×), (≞), (!), report, unsafeUpdateAt)

type Op a = a × 𝔹 -> Val 𝔹

data Val a =
   Int a Int |
   Float a Number |
   Str a String |
   Record a (Bindings (Val a)) |             -- always saturated
   Constr a Ctr (List (Val a)) |             -- potentially unsaturated
   Matrix a (MatrixRep a) |
   Primitive PrimOp (List (Val a)) |         -- never saturated
   Closure (Env a) (RecDefs a) a (Elim a)

-- op_fwd will be provided with original arguments, op_bwd with original output and arguments
newtype PrimOp = PrimOp {
   arity :: Int,
   op :: List (Val 𝔹) -> Val 𝔹,
   op_fwd :: List (Val 𝔹) -> Val 𝔹,
   op_bwd :: Val 𝔹 -> Endo (List (Val 𝔹))
}

type Env a = Bindings (Val a)

-- Matrices.
type Array2 a = Array (Array a)
type MatrixRep a = Array2 (Val a) × (Int × a) × (Int × a)

insertMatrix :: Int -> Int -> Val 𝔹 -> Endo (MatrixRep 𝔹)
insertMatrix i j v (vss × h × w) =
   let vs_i = vss!(i - 1)
       vss' = unsafeUpdateAt (i - 1) (unsafeUpdateAt (j - 1) v vs_i) vss
   in  vss' × h × w

-- ======================
-- boilerplate
-- ======================
instance functorVal :: Functor Val where
   map f (Int α n)                  = Int (f α) n
   map f (Float α n)                = Float (f α) n
   map f (Str α str)                = Str (f α) str
   map f (Record α xvs)             = Record (f α) (map (map f) <$> xvs)
   map f (Constr α c vs)            = Constr (f α) c (map f <$> vs)
   -- PureScript can't derive this case
   map f (Matrix α (r × iα × jβ))   = Matrix (f α) ((map (map f) <$> r) × (f <$> iα) × (f <$> jβ))
   map f (Primitive φ vs)           = Primitive φ ((map f) <$> vs)
   map f (Closure ρ h α σ)          = Closure (map (map f) <$> ρ) (map (map f) <$> h) (f α) (f <$> σ)

instance joinSemilatticeVal :: JoinSemilattice (Val Boolean) where
   join = definedJoin
   neg = (<$>) neg

instance slicesVal :: Slices (Val Boolean) where
   maybeJoin (Int α n) (Int α' n')                    = Int (α ∨ α') <$> (n ≞ n')
   maybeJoin (Float α n) (Float α' n')                = Float (α ∨ α') <$> (n ≞ n')
   maybeJoin (Str α str) (Str α' str')                = Str (α ∨ α') <$> (str ≞ str')
   maybeJoin (Record α xvs) (Record α' xvs')          = Record (α ∨ α') <$> maybeJoin xvs xvs'
   maybeJoin (Constr α c vs) (Constr α' c' us)        = Constr (α ∨ α') <$> (c ≞ c') <*> maybeJoin vs us
   maybeJoin (Matrix α (vss × (i × β) × (j × γ))) (Matrix α' (vss' × (i' × β') × (j' × γ'))) =
      Matrix (α ∨ α') <$> (
         maybeJoin vss vss' `lift2 (×)`
         ((flip (×) (β ∨ β')) <$> (i ≞ i')) `lift2 (×)`
         ((flip (×) (γ ∨ γ')) <$> (j ≞ j'))
      )
   maybeJoin (Closure ρ δ α σ) (Closure ρ' δ' α' σ')  =
      Closure <$> maybeJoin ρ ρ' <*> maybeJoin δ δ' <@> α ∨ α' <*> maybeJoin σ σ'
   maybeJoin (Primitive φ vs) (Primitive _ vs')       = Primitive φ <$> maybeJoin vs vs' -- TODO: require φ == φ'
   maybeJoin _ _                                      = report "Incompatible values"

instance boundedSlices :: BoundedSlices (Val Boolean) where
   botOf (Int _ n)                  = Int bot n
   botOf (Float _ n)                = Float bot n
   botOf (Str _ str)                = Str bot str
   botOf (Record _ xvs)             = Record bot (botOf <$> xvs)
   botOf (Constr _ c vs)            = Constr bot c (botOf <$> vs)
   -- PureScript can't derive this case
   botOf (Matrix _ (r × (i × _) × (j × _))) = Matrix bot ((((<$>) botOf) <$> r) × (i × bot) × (j × bot))
   botOf (Primitive φ vs)           = Primitive φ (botOf <$> vs)
   botOf (Closure γ ρ _ σ)          = Closure (botOf <$> γ) (botOf <$> ρ) bot (botOf σ)
