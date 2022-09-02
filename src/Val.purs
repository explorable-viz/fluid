module Val where

import Prelude hiding (absurd, append)
import Control.Apply (lift2)
import Data.List (List(..), (:))
import Data.Map (Map, filterKeys, keys, isEmpty, lookup, pop, unionWith)
import Data.Maybe (Maybe(..))
import Data.Set (Set, difference, empty, intersection, member, singleton, toUnfoldable, union)
import Bindings (Bind, Var, (↦))
import DataType (Ctr)
import Expr (Elim, fv)
import Lattice (
   class BoundedSlices, class JoinSemilattice, class Slices, 𝔹, (∨), bot, botOf, definedJoin, maybeJoin, neg
)
import Util (
   Endo, MayFail, type (×), (×), (≞), (!),
   absurd, disjUnion, error, mustLookup, orElse, report, unsafeUpdateAt
)

type Op a = a × 𝔹 -> Val 𝔹

data Val a =
   Int a Int |
   Float a Number |
   Str a String |
   Record a (List (Bind (Val a))) |          -- always saturated
   Constr a Ctr (List (Val a)) |             -- potentially unsaturated
   Matrix a (MatrixRep a) |
   Primitive PrimOp (List (Val a)) |         -- never saturated
   Closure a (Env a) (FunEnv a) (Elim a)

-- op_fwd will be provided with original arguments, op_bwd with original output and arguments
newtype PrimOp = PrimOp {
   arity :: Int,
   op :: List (Val 𝔹) -> Val 𝔹,
   op_fwd :: List (Val 𝔹) -> Val 𝔹,
   op_bwd :: Val 𝔹 -> Endo (List (Val 𝔹))
}

-- Environments.
type Env a = Map Var (Val a)
type FunEnv a = Map Var (Elim a)

dom :: forall a . Map Var a -> Set Var
dom = keys

lookup' :: forall a . Var -> Env a -> MayFail (Val a)
lookup' x γ = lookup x γ # (orElse $ "variable " <> x <> " not found")

update :: forall a . List (Bind a) -> Map Var a -> List (Bind a)
update Nil γ  | isEmpty γ = Nil
               | otherwise = error absurd
update (x ↦ v: xvs) γ =
   case pop x γ of
      Just (u × γ')  -> x ↦ u : update xvs γ'
      Nothing        -> x ↦ v : update xvs γ

-- Want a monoid instance but needs a newtype
append :: forall a . Env a -> Endo (Env a)
append = unionWith (const identity)

infixl 5 append as <+>

append_inv :: forall a . Set Var -> Env a -> Env a × Env a
append_inv xs γ = filterKeys (_ `not <<< member` xs) γ × restrict γ xs

restrict :: forall a . Map Var a -> Set Var -> Map Var a
restrict γ xs = filterKeys (_ `member` xs) γ

reaches :: forall a . FunEnv a -> Endo (Set Var)
reaches ρ xs = go (toUnfoldable xs) empty
   where
   dom_ρ = dom ρ
   go :: List Var -> Endo (Set Var)
   go Nil acc                          = acc
   go (x : xs') acc | x `member` acc   = go xs' acc
   go (x : xs') acc | otherwise        =
      let σ = mustLookup x ρ in
      go (toUnfoldable (fv σ `intersection` dom_ρ) <> xs')
         (singleton x `union` acc)

for :: forall a . FunEnv a -> Elim a -> FunEnv a
for ρ σ = ρ `restrict` reaches ρ (fv σ `intersection` dom ρ)

weakJoin :: forall a . Slices a => Map Var a -> Endo (Map Var a)
weakJoin m m' =
   let dom_m × dom_m' = dom m × dom m' in
   (m `restrict` (dom_m `difference` dom_m'))
   `disjUnion`
   (m `restrict` (dom_m `intersection` dom_m') ∨ m' `restrict` (dom_m `intersection` dom_m'))
   `disjUnion`
   (m' `restrict` (dom_m' `difference` dom_m))

infixl 6 weakJoin as ∨∨

-- Matrices.
type Array2 a = Array (Array a)
type MatrixRep a = Array2 (Val a) × (Int × a) × (Int × a)

updateMatrix :: Int -> Int -> Endo (Val 𝔹) -> Endo (MatrixRep 𝔹)
updateMatrix i j δv (vss × h × w) =
   let vs_i = vss!(i - 1)
       v_j = vs_i!(j - 1)
       vss' = unsafeUpdateAt (i - 1) (unsafeUpdateAt (j - 1) (δv v_j) vs_i) vss
   in vss' × h × w

-- ======================
-- boilerplate
-- ======================
instance Functor Val where
   map f (Int α n)                  = Int (f α) n
   map f (Float α n)                = Float (f α) n
   map f (Str α str)                = Str (f α) str
   map f (Record α xvs)             = Record (f α) (map (map f) <$> xvs)
   map f (Constr α c vs)            = Constr (f α) c (map f <$> vs)
   -- PureScript can't derive this case
   map f (Matrix α (r × iα × jβ))   = Matrix (f α) ((map (map f) <$> r) × (f <$> iα) × (f <$> jβ))
   map f (Primitive φ vs)           = Primitive φ ((map f) <$> vs)
   map f (Closure α γ ρ σ)          = Closure (f α) (map f <$> γ) (map f <$> ρ) (f <$> σ)

instance JoinSemilattice (Val Boolean) where
   join = definedJoin
   neg = (<$>) neg

instance Slices (Val Boolean) where
   maybeJoin (Int α n) (Int α' n')                    = Int (α ∨ α') <$> (n ≞ n')
   maybeJoin (Float α n) (Float α' n')                = Float (α ∨ α') <$> (n ≞ n')
   maybeJoin (Str α str) (Str α' str')                = Str (α ∨ α') <$> (str ≞ str')
   maybeJoin (Record α xvs) (Record α' xvs')          = Record (α ∨ α') <$> maybeJoin xvs xvs'
   maybeJoin (Constr α c vs) (Constr α' c' us)        = Constr (α ∨ α') <$> (c ≞ c') <*> maybeJoin vs us
   maybeJoin (Matrix α (vss × (i × βi) × (j × βj))) (Matrix α' (vss' × (i' × βi') × (j' × βj'))) =
      Matrix (α ∨ α') <$> (
         maybeJoin vss vss' `lift2 (×)`
         ((flip (×) (βi ∨ βi')) <$> (i ≞ i')) `lift2 (×)`
         ((flip (×) (βj ∨ βj')) <$> (j ≞ j'))
      )
   maybeJoin (Closure α γ ρ σ) (Closure α' γ' ρ' σ')  =
      Closure (α ∨ α') <$> maybeJoin γ γ' <*> maybeJoin ρ ρ' <*> maybeJoin σ σ'
   maybeJoin (Primitive φ vs) (Primitive _ vs')       = Primitive φ <$> maybeJoin vs vs' -- TODO: require φ == φ'
   maybeJoin _ _                                      = report "Incompatible values"

instance BoundedSlices (Val Boolean) where
   botOf (Int _ n)                  = Int bot n
   botOf (Float _ n)                = Float bot n
   botOf (Str _ str)                = Str bot str
   botOf (Record _ xvs)             = Record bot (botOf <$> xvs)
   botOf (Constr _ c vs)            = Constr bot c (botOf <$> vs)
   -- PureScript can't derive this case
   botOf (Matrix _ (r × (i × _) × (j × _))) = Matrix bot ((((<$>) botOf) <$> r) × (i × bot) × (j × bot))
   botOf (Primitive φ vs)           = Primitive φ (botOf <$> vs)
   botOf (Closure _ γ ρ σ)         = Closure bot (botOf <$> γ) (botOf <$> ρ) (botOf σ)
