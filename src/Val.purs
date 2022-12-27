module Val where

import Prelude hiding (absurd, append)

import Bindings (Var)
import Control.Apply (lift2)
import Data.List (List(..), (:))
import Data.Set (Set, empty, fromFoldable, intersection, member, singleton, toUnfoldable, union)
import DataType (Ctr)
import Dict (Dict, get)
import Expr (Elim, RecDefs, fv)
import Foreign.Object (filterKeys, lookup, unionWith)
import Foreign.Object (keys) as O
import Lattice (class BoundedJoinSemilattice, class BoundedLattice, class BoundedMeetSemilattice, class Expandable, class JoinSemilattice, class PartialJoinSemilattice, 𝔹, Raw, (∨), definedJoin, expand, maybeJoin, neg)
import Text.Pretty (Doc, beside, text)
import Unsafe.Coerce (unsafeCoerce)
import Util (Endo, MayFail, type (×), (×), (≞), (≜), (!), error, orElse, report, unsafeUpdateAt)

type Op a = a × 𝔹 -> Val 𝔹

data Val a
   = Int a Int
   | Float a Number
   | Str a String
   | Record a (Dict (Val a)) -- always saturated
   | Dictionary a (Dict (Val a)) -- always saturated
   | Constr a Ctr (List (Val a)) -- potentially unsaturated
   | Matrix a (MatrixRep a)
   | Primitive (PrimOp a) (List (Val a)) -- never saturated
   | Closure a (Env a) (RecDefs a) (Elim a)

-- op_bwd will be provided with original output and arguments
newtype PrimOp a = PrimOp
   { arity :: Int
   , op :: Highlightable a => BoundedMeetSemilattice a => List (Val a) -> Val a
   , op_bwd :: Highlightable a => BoundedLattice a => Val a -> List (Raw Val) -> List (Val a)
   }

class Highlightable a where
   highlightIf :: a -> Endo Doc

instance Highlightable Unit where
   highlightIf _ = identity

instance Highlightable Boolean where
   highlightIf false = identity
   highlightIf true = \doc -> text "_" `beside` doc `beside` text "_"

-- Environments.
type Env a = Dict (Val a)

lookup' :: forall a. Var -> Dict a -> MayFail a
lookup' x γ = lookup x γ # (orElse $ "variable " <> x <> " not found")

-- Want a monoid instance but needs a newtype
append :: forall a. Env a -> Endo (Env a)
append = unionWith (const identity)

infixl 5 append as <+>

append_inv :: forall a. Set Var -> Env a -> Env a × Env a
append_inv xs γ = filterKeys (_ `not <<< member` xs) γ × restrict γ xs

restrict :: forall a. Dict a -> Set Var -> Dict a
restrict γ xs = filterKeys (_ `member` xs) γ

reaches :: forall a. RecDefs a -> Endo (Set Var)
reaches ρ xs = go (toUnfoldable xs) empty
   where
   dom_ρ = fromFoldable $ O.keys ρ

   go :: List Var -> Endo (Set Var)
   go Nil acc = acc
   go (x : xs') acc | x `member` acc = go xs' acc
   go (x : xs') acc | otherwise =
      go (toUnfoldable (fv σ `intersection` dom_ρ) <> xs')
         (singleton x `union` acc)
      where
      σ = get x ρ

for :: forall a. RecDefs a -> Elim a -> RecDefs a
for ρ σ = ρ `restrict` reaches ρ (fv σ `intersection` (fromFoldable $ O.keys ρ))

-- Matrices.
type Array2 a = Array (Array a)
type MatrixRep a = Array2 (Val a) × (Int × a) × (Int × a)

updateMatrix :: forall a. Int -> Int -> Endo (Val a) -> Endo (MatrixRep a)
updateMatrix i j δv (vss × h × w) =
   vss' × h × w
   where
   vs_i = vss ! (i - 1)
   v_j = vs_i ! (j - 1)
   vss' = unsafeUpdateAt (i - 1) (unsafeUpdateAt (j - 1) (δv v_j) vs_i) vss

-- ======================
-- boilerplate
-- ======================
instance Functor PrimOp where
   map _ φ = unsafeCoerce φ -- ew

instance Functor Val where
   map f (Int α n) = Int (f α) n
   map f (Float α n) = Float (f α) n
   map f (Str α s) = Str (f α) s
   map f (Record α xvs) = Record (f α) (map f <$> xvs)
   map f (Dictionary α svs) = Dictionary (f α) (map f <$> svs)
   map f (Constr α c vs) = Constr (f α) c (map f <$> vs)
   -- PureScript can't derive this case
   map f (Matrix α (r × iα × jβ)) = Matrix (f α) ((map (map f) <$> r) × (f <$> iα) × (f <$> jβ))
   map f (Primitive φ vs) = Primitive (f <$> φ) ((map f) <$> vs)
   map f (Closure α γ ρ σ) = Closure (f α) (map f <$> γ) (map f <$> ρ) (f <$> σ)

instance JoinSemilattice a => JoinSemilattice (Val a) where
   join = definedJoin
   neg = (<$>) neg

instance JoinSemilattice a => PartialJoinSemilattice (Val a) where
   maybeJoin (Int α n) (Int α' n') = Int (α ∨ α') <$> (n ≞ n')
   maybeJoin (Float α n) (Float α' n') = Float (α ∨ α') <$> (n ≞ n')
   maybeJoin (Str α s) (Str α' s') = Str (α ∨ α') <$> (s ≞ s')
   maybeJoin (Record α xvs) (Record α' xvs') = Record (α ∨ α') <$> maybeJoin xvs xvs'
   maybeJoin (Dictionary α svs) (Dictionary α' svs') = Dictionary (α ∨ α') <$> maybeJoin svs svs'
   maybeJoin (Constr α c vs) (Constr α' c' us) = Constr (α ∨ α') <$> (c ≞ c') <*> maybeJoin vs us
   maybeJoin (Matrix α (vss × (i × βi) × (j × βj))) (Matrix α' (vss' × (i' × βi') × (j' × βj'))) =
      Matrix (α ∨ α') <$>
         ( maybeJoin vss vss'
              `lift2 (×)` ((flip (×) (βi ∨ βi')) <$> (i ≞ i'))
              `lift2 (×)`
                 ((flip (×) (βj ∨ βj')) <$> (j ≞ j'))
         )
   maybeJoin (Closure α γ ρ σ) (Closure α' γ' ρ' σ') =
      Closure (α ∨ α') <$> maybeJoin γ γ' <*> maybeJoin ρ ρ' <*> maybeJoin σ σ'
   maybeJoin (Primitive φ vs) (Primitive _ vs') = Primitive φ <$> maybeJoin vs vs' -- TODO: require φ == φ'
   maybeJoin _ _ = report "Incompatible values"

instance BoundedJoinSemilattice a => Expandable (Val a) where
   expand (Int α n) (Int _ n') = Int α (n ≜ n')
   expand (Float α n) (Float _ n') = Float α (n ≜ n')
   expand (Str α s) (Str _ s') = Str α (s ≜ s')
   expand (Record α xvs) (Record _ xvs') = Record α (expand xvs xvs')
   expand (Dictionary α svs) (Dictionary _ svs') = Dictionary α (expand svs svs')
   expand (Constr α c vs) (Constr _ c' us) = Constr α (c ≜ c') (expand vs us)
   expand (Matrix α (vss × (i × βi) × (j × βj))) (Matrix _ (vss' × (i' × _) × (j' × _))) =
      Matrix α (expand vss vss' × ((i ≜ i') × βi) × ((j ≜ j') × βj))
   expand (Closure α γ ρ σ) (Closure _ γ' ρ' σ') =
      Closure α (expand γ γ') (expand ρ ρ') (expand σ σ')
   expand (Primitive φ vs) (Primitive _ vs') = Primitive φ (expand vs vs') -- TODO: require φ == φ'
   expand _ _ = error "Incompatible values"
