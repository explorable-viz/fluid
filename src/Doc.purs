module Doc where

import Prelude

import Data.List (List, zipWith)
import Data.Set as Set
import Data.Traversable (class Foldable, class Traversable)
import Graph (class Vertices, Vertex, vertices)
import Lattice (class BoundedJoinSemilattice, class Expandable, class JoinSemilattice, Raw, expand, (∨))
import Util (error, shapeMismatch, (≜))

data DocOpt :: (Type -> Type) -> Type -> Type
data DocOpt e a = None | Doc (List (e a)) (List (DocCommentElem e a))

data DocCommentElem :: (Type -> Type) -> Type -> Type
data DocCommentElem e a = Token String | Unquote (e a)

-- Purescript Typeclass instances
instance Eq (e a) => Eq (DocOpt e a) where
   eq None None = true
   eq (Doc _ doc) (Doc _ doc') = doc == doc'
   eq _ _ = false

instance Eq (e a) => Eq (DocCommentElem e a) where
   eq (Token s) (Token s') = s == s'
   eq (Unquote e) (Unquote e') = e == e'
   eq _ _ = false

derive instance Ord (e a) => Ord (DocOpt e a)
instance Ord (e a) => Ord (DocCommentElem e a) where
   compare (Token s) (Token s') = compare s s'
   compare (Unquote e) (Unquote e') = compare e e'
   compare (Token _) (Unquote _) = LT
   compare (Unquote _) (Token _) = GT

derive instance Functor e => Functor (DocCommentElem e)
derive instance Foldable e => Foldable (DocCommentElem e)
derive instance Traversable e => Traversable (DocCommentElem e)
derive instance Functor e => Functor (DocOpt e)
derive instance Foldable e => Foldable (DocOpt e)
derive instance Traversable e => Traversable (DocOpt e)

instance Show (e a) => Show (DocOpt e a) where
   show None = "None"
   show (Doc is doc) = "Doc " <> show is <> show doc

instance Show (e a) => Show (DocCommentElem e a) where
   show (Token s) = "Token " <> show s
   show (Unquote e) = "Unquote " <> show e

instance Apply e => Apply (DocOpt e) where
   apply None _ = None
   apply (Doc fins doc) (Doc ins doc') = Doc (zipWith (<*>) fins ins) (zipWith (<*>) doc doc')
   apply _ _ = error $ shapeMismatch unit

instance Apply e => Apply (DocCommentElem e) where
   apply (Token s) (Token s') = Token (s ≜ s')
   apply (Unquote e) (Unquote e') = Unquote (e <*> e')
   apply _ _ = error $ shapeMismatch unit

-- Fluid specific instances
instance JoinSemilattice (e a) => JoinSemilattice (DocOpt e a) where
   join None None = None
   join (Doc ins doc) (Doc ins' doc') = Doc (ins ∨ ins') (doc ∨ doc')
   join _ _ = error $ shapeMismatch unit

instance JoinSemilattice (e a) => JoinSemilattice (DocCommentElem e a) where
   join (Token s) (Token s') = Token (s ≜ s')
   join (Unquote e) (Unquote e') = Unquote (e ∨ e')
   join _ _ = error $ shapeMismatch unit

instance (Functor e, BoundedJoinSemilattice a, Expandable (e a) (Raw e)) => Expandable (DocOpt e a) (DocOpt e Unit) where
   expand None _ = None
   expand (Doc ins doc) (Doc ins' doc') = Doc (expand ins ins') (expand doc doc')
   expand _ _ = error $ shapeMismatch unit

instance (BoundedJoinSemilattice a, (Expandable (e a) (Raw e))) => Expandable (DocCommentElem e a) (DocCommentElem e Unit) where
   expand (Token s) (Token s') = Token (s ≜ s')
   expand (Unquote e) (Unquote e') = Unquote (expand e e')
   expand _ _ = error $ shapeMismatch unit

instance Vertices (e Vertex) => Vertices (DocCommentElem e Vertex) where
   vertices (Token _) = Set.empty
   vertices (Unquote e) = vertices e

instance Vertices (e Vertex) => Vertices (DocOpt e Vertex) where
   vertices None = Set.empty
   vertices (Doc _ doc) = Set.unions (vertices <$> doc)

instance Semigroup (DocOpt e a) where
   append doc None = doc
   append None doc = doc
   append (Doc ins doc) (Doc ins' doc') = Doc (ins <> ins') (doc <> doc')
