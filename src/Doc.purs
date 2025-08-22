module Doc where

import Prelude

import Data.List (List, zipWith)
import Data.Set as Set
import Data.Traversable (class Foldable, class Traversable)
import Graph (class Vertices, Vertex, vertices)
import Lattice (class BoundedJoinSemilattice, class Expandable, class JoinSemilattice, Raw, expand, (∨))
import Util (error, shapeMismatch, (≜))

data DocOpt :: (Type -> Type) -> Type -> Type
data DocOpt e a = None | Doc (e a)

type Paragraph e a = List (ParagraphElem e a)

data ParagraphElem :: (Type -> Type) -> Type -> Type
data ParagraphElem e a = Token String | Unquote (e a)

-- Purescript Typeclass instances
instance eqDocOpt :: Eq (e a) => Eq (DocOpt e a) where
  eq None None = true
  eq (Doc a) (Doc b) = a == b
  eq _ _ = false

instance ordDocOpt :: Ord (e a) => Ord (DocOpt e a) where
  compare None None = EQ
  compare None (Doc _) = LT
  compare (Doc _) None = GT
  compare (Doc a) (Doc b) = compare a b

derive instance Functor e => Functor (ParagraphElem e)
derive instance Foldable e => Foldable (ParagraphElem e)
derive instance Traversable e => Traversable (ParagraphElem e)
derive instance Functor e => Functor (DocOpt e)
derive instance Foldable e => Foldable (DocOpt e)
derive instance Traversable e => Traversable (DocOpt e)

instance Show (e a) => Show (DocOpt e a) where
   show None = "None"
   show (Doc doc) = "Doc " <> show doc

instance Show (e a) => Show (ParagraphElem e a) where
   show (Token s) = "Token " <> show s
   show (Unquote e) = "Unquote " <> show e

instance Apply f => Apply (DocOpt f) where
  apply None _ = None
  apply _ None = None
  apply (Doc fs) (Doc xs) = Doc (zipWith (<*>) fs xs)

instance Apply e => Apply (ParagraphElem e) where
   apply (Token s) (Token s') = Token (s ≜ s')
   apply (Unquote e) (Unquote e') = Unquote (e <*> e')
   apply _ _ = error $ shapeMismatch unit

-- Fluid specific instances
instance JoinSemilattice (e a) => JoinSemilattice (DocOpt e a) where
   join None None = None
   join (Doc doc) (Doc doc') = Doc (doc ∨ doc')
   join _ _ = error $ shapeMismatch unit

instance JoinSemilattice (e a) => JoinSemilattice (ParagraphElem e a) where
   join (Token s) (Token s') = Token (s ≜ s')
   join (Unquote e) (Unquote e') = Unquote (e ∨ e')
   join _ _ = error $ shapeMismatch unit

instance (BoundedJoinSemilattice a, Expandable (e a) (Raw e)) => Expandable (DocOpt e a) (DocOpt e Unit) where
   expand None _ = None
   expand (Doc doc) (Doc doc') = Doc (expand doc doc')
   expand _ _ = error $ shapeMismatch unit

instance (BoundedJoinSemilattice a, (Expandable (e a) (Raw e))) => Expandable (ParagraphElem e a) (ParagraphElem e Unit) where
   expand (Token s) (Token s') = Token (s ≜ s')
   expand (Unquote e) (Unquote e') = Unquote (expand e e')
   expand _ _ = error $ shapeMismatch unit

instance Vertices (e Vertex) => Vertices (ParagraphElem e Vertex) where
   vertices (Token _) = Set.empty
   vertices (Unquote e) = vertices e

instance (Foldable e, Functor e, Vertices Vertex) => Vertices (DocOpt e Vertex) where
  vertices None = Set.empty
  vertices (Doc doc) = Set.unions (map vertices doc)


instance Semigroup (f a) => Semigroup (DocOpt f a) where
  append None doc = doc
  append doc None = doc
  append (Doc doc) (Doc doc') = Doc (doc <> doc')
