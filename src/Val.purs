module Val where

import Prelude hiding (absurd, append)

import Bind (Var)
import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadError)
import Data.Array ((!!))
import Data.Array (zipWith) as A
import Data.Bitraversable (bitraverse)
import Data.Exists (Exists)
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldrDefault)
import Data.List (List(..), (:), zipWith)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import DataType (Ctr)
import Dict (Dict)
import Dict as D
import Effect.Exception (Error)
import Expr (Elim, fv)
import GaloisConnection (GaloisConnection(..))
import Graph (Vertex(..))
import Graph.WithGraph (class MonadWithGraphAlloc)
import Lattice (class BoundedJoinSemilattice, class BoundedLattice, class BoundedMeetSemilattice, class Expandable, class JoinSemilattice, Raw, definedJoin, expand, maybeJoin, topOf, (∨))
import Util (type (×), Endo, assert, assertWith, definitely, shapeMismatch, singleton, unsafeUpdateAt, (!), (×), (∩), (≜), (≞), (⊆))
import Util.Map (class Map', delete, filterKeys, get, insert, intersectionWith, keys, lookup, maplet, restrict, size, toUnfoldable, unionWith, values)
import Util.Pretty (Doc, beside, text)
import Util.Set (class Set', difference, empty, isEmpty, union, (\\), (∈), (∪))

data Val a = Val a (BaseVal a)

data BaseVal a
   = Int Int
   | Float Number
   | Str String
   | Constr Ctr (List (Val a)) -- always saturated
   | Record (Dict (Val a)) -- always saturated
   | Dictionary (DictRep a)
   | Matrix (MatrixRep a)
   | Fun (Fun a)

data Fun a
   = Closure (Env a) (Dict (Elim a)) (Elim a)
   | Foreign ForeignOp (List (Val a)) -- never saturated
   | PartialConstr Ctr (List (Val a)) -- never saturated

class (Highlightable a, BoundedLattice a) <= Ann a

instance Ann Boolean
instance Ann Unit

instance Highlightable a => Highlightable (a × b) where
   highlightIf (a × _) doc = highlightIf a doc

instance (Ann a, BoundedLattice b) => Ann (a × b)

-- similar to an isomorphism lens with complement t
type OpFwd t = forall a m. Ann a => MonadError Error m => List (Val a) -> m (t × Val a)
type OpBwd t = forall a. Ann a => t × Val a -> List (Val a)
type OpGraph = forall m. MonadWithGraphAlloc m => MonadError Error m => List (Val Vertex) -> m (Val Vertex)

data ForeignOp' t = ForeignOp'
   { arity :: Int
   , op :: OpFwd t
   , op' :: OpGraph
   , op_bwd :: OpBwd t
   }

newtype ForeignOp = ForeignOp (String × Exists ForeignOp') -- string is unique identifier (for Eq)

instance Eq ForeignOp where
   eq (ForeignOp (s × _)) (ForeignOp (s' × _)) = s == s'

instance Ord ForeignOp where
   compare (ForeignOp (s × _)) (ForeignOp (s' × _)) = compare s s'

-- Environments.
newtype Env a = Env (Dict (Val a))

instance Set' (Env a) String where
   empty = Env empty
   isEmpty (Env γ) = isEmpty γ
   member x (Env γ) = x ∈ γ
   difference (Env γ) (Env γ') = Env (difference γ γ')
   union (Env γ) (Env γ') = Env (union γ γ')

instance Map' (Env a) String (Val a) where
   maplet k v = Env (maplet k v)
   keys (Env γ) = keys γ
   values (Env γ) = values γ
   size (Env γ) = size γ
   filterKeys p (Env γ) = Env (filterKeys p γ)
   unionWith f (Env γ) (Env γ') = Env (unionWith f γ γ')
   lookup k (Env γ) = lookup k γ
   delete k (Env γ) = Env (delete k γ)
   insert k v (Env γ) = Env (insert k v γ)
   toUnfoldable (Env γ) = toUnfoldable γ

-- Goes from smaller environment to larger (so "dual" to a projection).
unrestrictGC :: forall a. BoundedMeetSemilattice a => Raw Env -> Set Var -> GaloisConnection (Env a) (Env a)
unrestrictGC γ xs =
   assertWith (show xs <> " are in environment ") (xs ⊆ keys γ) $ GC
      { fwd: \γ' -> assert (keys γ' ⊆ keys γ) $ γ' ∪ ((topOf γ \\ γ'))
      , bwd: \γ' -> assert (keys γ' == keys γ) $ restrict xs γ'
      }

reaches :: forall a. Dict (Elim a) -> Endo (Set Var)
reaches ρ xs = go (Set.toUnfoldable xs) empty
   where
   dom_ρ = Set.fromFoldable $ keys ρ

   go :: List Var -> Endo (Set Var)
   go Nil acc = acc
   go (x : xs') acc | x ∈ acc = go xs' acc
   go (x : xs') acc | otherwise =
      go (Set.toUnfoldable (fv σ ∩ dom_ρ) <> xs') (singleton x ∪ acc)
      where
      σ = get x ρ

forDefs :: forall a. Dict (Elim a) -> Elim a -> Dict (Elim a)
forDefs ρ σ = restrict (reaches ρ (fv σ ∩ Set.fromFoldable (keys ρ))) ρ

-- Wrap internal representations to provide foldable/traversable instances.
newtype DictRep a = DictRep (Dict (a × Val a))
newtype MatrixRep a = MatrixRep (Array2 (Val a) × (Int × a) × (Int × a))

type Array2 a = Array (Array a)

matrixGet :: forall a. Int -> Int -> MatrixRep a -> Val a
matrixGet i j (MatrixRep (vss × _ × _)) = definitely "index out of bounds!" $ do
   us <- vss !! (i - 1)
   us !! (j - 1)

matrixPut :: forall a. Int -> Int -> Endo (Val a) -> Endo (MatrixRep a)
matrixPut i j δv (MatrixRep (vss × h × w)) =
   MatrixRep (vss' × h × w)
   where
   vs_i = vss ! (i - 1)
   v_j = vs_i ! (j - 1)
   vss' = unsafeUpdateAt (i - 1) (unsafeUpdateAt (j - 1) (δv v_j) vs_i) vss

class Highlightable a where
   highlightIf :: a -> Endo Doc

instance Highlightable Unit where
   highlightIf _ = identity

instance Highlightable Boolean where
   highlightIf false = identity
   highlightIf true = \doc -> text "⸨" `beside` doc `beside` text "⸩"

instance Highlightable Vertex where
   highlightIf (Vertex α) = \doc -> doc `beside` text "_" `beside` text ("⟨" <> α <> "⟩")

-- ======================
-- boilerplate
-- ======================
derive instance Functor DictRep
derive instance Functor MatrixRep
derive instance Functor Val
derive instance Functor Env
derive instance Functor Fun
derive instance Functor BaseVal
derive instance Traversable Val
derive instance Traversable BaseVal
derive instance Traversable Fun
derive instance Traversable Env
derive instance Foldable Val
derive instance Foldable BaseVal
derive instance Foldable Fun
derive instance Foldable Env

instance Apply Val where
   apply (Val fα fv) (Val α v) = Val (fα α) (fv <*> v)

instance Apply BaseVal where
   apply (Int n) (Int n') = Int (n ≜ n')
   apply (Float n) (Float n') = Float (n ≜ n')
   apply (Str s) (Str s') = Str (s ≜ s')
   apply (Constr c fes) (Constr c' es) = Constr (c ≜ c') (zipWith (<*>) fes es)
   apply (Record fxvs) (Record xvs) = Record (((<*>) <$> fxvs) <*> xvs)
   apply (Dictionary fxvs) (Dictionary xvs) = Dictionary (fxvs <*> xvs)
   apply (Matrix fm) (Matrix m) = Matrix (fm <*> m)
   apply (Fun ff) (Fun f) = Fun (ff <*> f)
   apply _ _ = shapeMismatch unit

instance Apply Fun where
   apply (Closure fγ fρ fσ) (Closure γ ρ σ) = Closure (fγ <*> γ) (((<*>) <$> fρ) <*> ρ) (fσ <*> σ)
   apply (Foreign op fvs) (Foreign _ vs) = Foreign op (zipWith (<*>) fvs vs)
   apply (PartialConstr c fvs) (PartialConstr c' vs) = PartialConstr (c ≜ c') (zipWith (<*>) fvs vs)
   apply _ _ = shapeMismatch unit

-- Should require equal domains?
instance Apply DictRep where
   apply (DictRep fxvs) (DictRep xvs) =
      DictRep $ intersectionWith (\(fα × fv) (α × v) -> fα α × (fv <*> v)) fxvs xvs

instance Apply MatrixRep where
   apply (MatrixRep (fvss × (n × fnα) × (m × fmα))) (MatrixRep (vss × (n' × nα) × (m' × mα))) =
      MatrixRep $ (A.zipWith (A.zipWith (<*>)) fvss vss) × ((n ≜ n') × fnα nα) × ((m ≜ m') × fmα mα)

instance Apply Env where
   apply (Env fγ) (Env γ) = Env (((<*>) <$> fγ) <*> γ)

instance Foldable DictRep where
   foldl f acc (DictRep d) = foldl (\acc' (a × v) -> foldl f (acc' `f` a) v) acc d
   foldr f = foldrDefault f
   foldMap f = foldMapDefaultL f

instance Traversable DictRep where
   traverse f (DictRep d) = DictRep <$> traverse (bitraverse f (traverse f)) d
   sequence = sequenceDefault

instance Foldable MatrixRep where
   foldl f acc (MatrixRep (vss × (_ × βi) × (_ × βj))) = foldl (foldl (foldl f)) (acc `f` βi `f` βj) vss
   foldr f = foldrDefault f
   foldMap f = foldMapDefaultL f

instance Traversable MatrixRep where
   traverse f (MatrixRep m) =
      MatrixRep <$> bitraverse (traverse (traverse (traverse f)))
         (bitraverse (traverse f) (traverse f))
         m
   sequence = sequenceDefault

instance JoinSemilattice a => JoinSemilattice (DictRep a) where
   maybeJoin (DictRep svs) (DictRep svs') = DictRep <$> maybeJoin svs svs'
   join v = definedJoin v

instance JoinSemilattice a => JoinSemilattice (MatrixRep a) where
   maybeJoin (MatrixRep (vss × (i × βi) × (j × βj))) (MatrixRep (vss' × (i' × βi') × (j' × βj'))) =
      MatrixRep <$>
         ( maybeJoin vss vss'
              `lift2 (×)` (((_ × (βi ∨ βi')) <$> (i ≞ i')) `lift2 (×)` ((_ × (βj ∨ βj')) <$> (j ≞ j')))
         )
   join v = definedJoin v

instance JoinSemilattice a => JoinSemilattice (Val a) where
   maybeJoin (Val α u) (Val α' v) = Val (α ∨ α') <$> maybeJoin u v

   join v = definedJoin v

-- Why not maybeJoin x y = sequence (maybeJoin <$> x <*> y)?
instance JoinSemilattice a => JoinSemilattice (BaseVal a) where
   maybeJoin (Int n) (Int n') = Int <$> (n ≞ n')
   maybeJoin (Float n) (Float n') = Float <$> (n ≞ n')
   maybeJoin (Str s) (Str s') = Str <$> (s ≞ s')
   maybeJoin (Record xvs) (Record xvs') = Record <$> maybeJoin xvs xvs'
   maybeJoin (Dictionary d) (Dictionary d') = Dictionary <$> maybeJoin d d'
   maybeJoin (Constr c vs) (Constr c' us) = Constr <$> (c ≞ c') <*> maybeJoin vs us
   maybeJoin (Matrix m) (Matrix m') = Matrix <$> maybeJoin m m'
   maybeJoin (Fun φ) (Fun φ') = Fun <$> maybeJoin φ φ'
   maybeJoin _ _ = shapeMismatch unit

   join v = definedJoin v

instance JoinSemilattice a => JoinSemilattice (Fun a) where
   maybeJoin (Closure γ ρ σ) (Closure γ' ρ' σ') =
      Closure <$> maybeJoin γ γ' <*> maybeJoin ρ ρ' <*> maybeJoin σ σ'
   maybeJoin (Foreign φ vs) (Foreign _ vs') =
      Foreign φ <$> maybeJoin vs vs' -- TODO: require φ == φ'
   maybeJoin (PartialConstr c vs) (PartialConstr c' us) =
      PartialConstr <$> (c ≞ c') <*> maybeJoin vs us
   maybeJoin _ _ = shapeMismatch unit

   join v = definedJoin v

instance JoinSemilattice a => JoinSemilattice (Env a) where
   maybeJoin (Env γ) (Env γ') = Env <$> maybeJoin γ γ'
   join v = definedJoin v

instance BoundedJoinSemilattice a => Expandable (DictRep a) (Raw DictRep) where
   expand (DictRep svs) (DictRep svs') = DictRep (expand svs svs')

instance BoundedJoinSemilattice a => Expandable (MatrixRep a) (Raw MatrixRep) where
   expand (MatrixRep (vss × (i × βi) × (j × βj))) (MatrixRep (vss' × (i' × _) × (j' × _))) =
      MatrixRep (expand vss vss' × ((i ≜ i') × βi) × ((j ≜ j') × βj))

instance BoundedJoinSemilattice a => Expandable (Val a) (Raw Val) where
   expand (Val α u) (Val _ v) = Val α (expand u v)

instance BoundedJoinSemilattice a => Expandable (BaseVal a) (Raw BaseVal) where
   expand (Int n) (Int n') = Int (n ≜ n')
   expand (Float n) (Float n') = Float (n ≜ n')
   expand (Str s) (Str s') = Str (s ≜ s')
   expand (Record xvs) (Record xvs') = Record (expand xvs xvs')
   expand (Dictionary d) (Dictionary d') = Dictionary (expand d d')
   expand (Constr c vs) (Constr c' us) = Constr (c ≜ c') (expand vs us)
   expand (Matrix m) (Matrix m') = Matrix (expand m m')
   expand (Fun φ) (Fun φ') = Fun (expand φ φ')
   expand _ _ = shapeMismatch unit

instance BoundedJoinSemilattice a => Expandable (Fun a) (Raw Fun) where
   expand (Closure γ ρ σ) (Closure γ' ρ' σ') =
      Closure (expand γ γ') (expand ρ ρ') (expand σ σ')
   expand (Foreign φ vs) (Foreign _ vs') = Foreign φ (expand vs vs') -- TODO: require φ == φ'
   expand (PartialConstr c vs) (PartialConstr c' us) = PartialConstr (c ≜ c') (expand vs us)
   expand _ _ = shapeMismatch unit

instance BoundedJoinSemilattice a => Expandable (Env a) (Raw Env) where
   expand (Env γ) (Env γ') = Env (expand γ γ')

derive instance Eq a => Eq (Val a)
derive instance Eq a => Eq (BaseVal a)
derive instance Eq a => Eq (DictRep a)
derive instance Eq a => Eq (MatrixRep a)
derive instance Eq a => Eq (Fun a)
derive instance Eq a => Eq (Env a)

derive instance Ord a => Ord (Val a)
derive instance Ord a => Ord (BaseVal a)
derive instance Ord a => Ord (DictRep a)
derive instance Ord a => Ord (MatrixRep a)
derive instance Ord a => Ord (Fun a)
derive instance Ord a => Ord (Env a)

derive instance Newtype (Env a) _
