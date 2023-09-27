module Val2 where

import Prelude hiding (absurd, append)

import Bindings (Var)
import BoolAlg (BoolAlg)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Data.Array ((!!))
import Data.Array (zipWith) as A
import Data.Bitraversable (bitraverse)
import Data.Exists (Exists)
import Data.Foldable (class Foldable, foldl, foldrDefault, foldMapDefaultL)
import Data.List (List(..), (:), zipWith)
import Data.Set (Set, empty, fromFoldable, intersection, member, singleton, toUnfoldable, union)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import DataType (Ctr)
import Dict (Dict, get)
import Dict (apply2, intersectionWith) as D
import Effect.Exception (Error)
import Expr (Elim, RecDefs, fv)
import Foreign.Object (filterKeys, lookup, unionWith)
import Foreign.Object (keys) as O
import Graph (Vertex(..))
import Graph.GraphWriter (class MonadGraphAlloc)
import Util (type (×), Endo, error, orElse, unsafeUpdateAt, (!), (×))
import Util.Pretty (Doc, beside, text)

data Val a
   = Int a Int
   | Float a Number
   | Str a String
   | Constr a Ctr (List (Val a)) -- always saturated
   | Record a (Dict (Val a)) -- always saturated
   | Dictionary a (DictRep a)
   | Matrix a (MatrixRep a)
   | Fun a (Fun a)

data Fun a
   = Closure (Env a) (RecDefs a) (Elim a)
   | Foreign ForeignOp (List (Val a)) -- never saturated
   | PartialConstr Ctr (List (Val a)) -- never saturated

instance Highlightable a => Highlightable (a × b) where
   highlightIf (a × _) doc = highlightIf a doc

-- similar to an isomorphism lens with complement t
type OpFwd t = forall a m. Highlightable a => MonadError Error m => BoolAlg a -> List (Val a) -> m (t × Val a)
type OpBwd t = forall a. Highlightable a => BoolAlg a -> t × Val a -> List (Val a)
type OpGraph = forall m. MonadGraphAlloc m => MonadError Error m => List (Val Vertex) -> m (Val Vertex)

data ForeignOp' t = ForeignOp'
   { arity :: Int
   , op :: OpFwd t
   , op' :: OpGraph
   , op_bwd :: OpBwd t
   }

type ForeignOp = Exists ForeignOp'

-- Environments.
type Env a = Dict (Val a)

lookup' :: forall a m. MonadThrow Error m => Var -> Dict a -> m a
lookup' x γ = lookup x γ # orElse ("variable " <> x <> " not found")

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

-- Wrap internal representations to provide foldable/traversable instances.
newtype DictRep a = DictRep (Dict (a × Val a))
newtype MatrixRep a = MatrixRep (Array2 (Val a) × (Int × a) × (Int × a))

type Array2 a = Array (Array a)

matrixGet :: forall a m. MonadThrow Error m => Int -> Int -> MatrixRep a -> m (Val a)
matrixGet i j (MatrixRep (vss × _ × _)) =
   orElse "Index out of bounds" $ do
      us <- vss !! (i - 1)
      us !! (j - 1)

matrixUpdate :: forall a. Int -> Int -> Endo (Val a) -> Endo (MatrixRep a)
matrixUpdate i j δv (MatrixRep (vss × h × w)) =
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
derive instance Foldable Val
derive instance Traversable Val
derive instance Functor Fun
derive instance Foldable Fun
derive instance Traversable Fun

instance Apply Val where
   apply (Int fα n) (Int α _) = Int (fα α) n
   apply (Float fα n) (Float α _) = Float (fα α) n
   apply (Str fα s) (Str α _) = Str (fα α) s
   apply (Constr fα c fes) (Constr α _ es) = Constr (fα α) c (zipWith (<*>) fes es)
   apply (Record fα fxvs) (Record α xvs) = Record (fα α) (D.apply2 fxvs xvs)
   apply (Dictionary fα fxvs) (Dictionary α xvs) = Dictionary (fα α) (fxvs <*> xvs)
   apply (Matrix fα fm) (Matrix α m) = Matrix (fα α) (fm <*> m)
   apply (Fun fα ff) (Fun α f) = Fun (fα α) (ff <*> f)
   apply _ _ = error "Apply Expr: shape mismatch"

instance Apply Fun where
   apply (Closure fγ fρ fσ) (Closure γ ρ σ) = Closure (D.apply2 fγ γ) (D.apply2 fρ ρ) (fσ <*> σ)
   apply (Foreign op fvs) (Foreign _ vs) = Foreign op (zipWith (<*>) fvs vs)
   apply (PartialConstr c fvs) (PartialConstr _ vs) = PartialConstr c (zipWith (<*>) fvs vs)
   apply _ _ = error "Apply Fun: shape mismatch"

instance Apply DictRep where
   apply (DictRep fxvs) (DictRep xvs) = DictRep $ D.intersectionWith (\(fα' × fv') (α' × v') -> (fα' α') × (fv' <*> v')) fxvs xvs

instance Apply MatrixRep where
   apply (MatrixRep (fvss × (n × fnα) × (m × fmα))) (MatrixRep (vss × (_ × nα) × (_ × mα))) = MatrixRep $ (A.zipWith (A.zipWith (<*>)) fvss vss) × (n × fnα nα) × (m × fmα mα)

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
