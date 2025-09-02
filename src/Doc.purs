module Doc where

import Prelude

import Data.List (List)
import Data.Set as Set
import Data.Traversable (class Foldable, class Traversable)
import Graph (class Vertices, Vertex, vertices)
import Lattice (class BoundedJoinSemilattice, class Expandable, class JoinSemilattice, Raw, expand, (∨))
import Util (error, shapeMismatch, (≜))

type Paragraph e a = List (ParagraphElem e a)

data ParagraphElem :: (Type -> Type) -> Type -> Type
data ParagraphElem e a = Token String | Unquote (e a)

instance JoinSemilattice (e a) => JoinSemilattice (ParagraphElem e a) where
   join (Token s) (Token s') = Token (s ≜ s')
   join (Unquote e) (Unquote e') = Unquote (e ∨ e')
   join _ _ = error $ shapeMismatch unit

instance (BoundedJoinSemilattice a, (Expandable (e a) (Raw e))) => Expandable (ParagraphElem e a) (ParagraphElem e Unit) where
   expand (Token s) (Token s') = Token (s ≜ s')
   expand (Unquote e) (Unquote e') = Unquote (expand e e')
   expand _ _ = error $ shapeMismatch unit

instance Vertices (e Vertex) => Vertices (ParagraphElem e Vertex) where
   vertices (Token _) = Set.empty
   vertices (Unquote e) = vertices e

-- ======================
-- boilerplate
-- ======================
instance Eq (e a) => Eq (ParagraphElem e a) where
   eq (Token s) (Token s') = s == s'
   eq (Unquote e) (Unquote e') = e == e'
   eq _ _ = false

instance Ord (e a) => Ord (ParagraphElem e a) where
   compare (Token s) (Token s') = compare s s'
   compare (Unquote e) (Unquote e') = compare e e'
   compare (Token _) (Unquote _) = LT
   compare (Unquote _) (Token _) = GT

derive instance Functor e => Functor (ParagraphElem e)
derive instance Foldable e => Foldable (ParagraphElem e)
derive instance Traversable e => Traversable (ParagraphElem e)

instance Show (e a) => Show (ParagraphElem e a) where
   show (Token s) = "Token " <> show s
   show (Unquote e) = "Unquote " <> show e

instance Apply e => Apply (ParagraphElem e) where
   apply (Token s) (Token s') = Token (s ≜ s')
   apply (Unquote e) (Unquote e') = Unquote (e <*> e')
   apply _ _ = error $ shapeMismatch unit
