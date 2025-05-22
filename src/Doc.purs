module Doc where

import Prelude

import Data.List (List, zipWith)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (class Foldable, class Traversable)
import Graph (class Vertices, Vertex, DVertex, vertices)
import Lattice (class BoundedJoinSemilattice, class Expandable, class JoinSemilattice, Raw, expand, (∨))
import Util (error, shapeMismatch, (≜))

data DocOpt :: (Type -> Type) -> Type -> Type
data DocOpt e a = None | Doc (DocComment e a)

type DocComment :: (Type -> Type) -> Type -> Type
type DocComment e a = List (DocCommentElem e a)

data DocCommentElem :: (Type -> Type) -> Type -> Type
data DocCommentElem e a = Token String | CExpr (e a)

instance Eq (e a) => Eq (DocCommentElem e a) where
   eq (Token s) (Token s') = s == s'
   eq (CExpr e) (CExpr e') = e == e'
   eq _ _ = false

instance Eq (e a) => Eq (DocOpt e a) where
   eq None None = true
   eq (Doc doc) (Doc doc') = doc == doc'
   eq _ _ = false

derive instance Ord (e a) => Ord (DocOpt e a)

instance Ord (e a) => Ord (DocCommentElem e a) where
   compare (Token s) (Token s') = compare s s'
   compare (CExpr e) (CExpr e') = compare e e'
   compare (Token _) (CExpr _) = LT
   compare (CExpr _) (Token _) = GT

instance JoinSemilattice (e a) => JoinSemilattice (DocOpt e a) where
   join None None = None
   join (Doc doc) (Doc doc') = Doc (doc ∨ doc')
   join _ _ = error $ shapeMismatch unit

instance JoinSemilattice (e a) => JoinSemilattice (DocCommentElem e a) where
   join (Token s) (Token s') = Token (s ≜ s')
   join (CExpr e) (CExpr e') = CExpr (e ∨ e')
   join _ _ = error $ shapeMismatch unit

derive instance Functor e => Functor (DocCommentElem e)
derive instance Foldable e => Foldable (DocCommentElem e)
derive instance Traversable e => Traversable (DocCommentElem e)
derive instance Functor e => Functor (DocOpt e)
derive instance Foldable e => Foldable (DocOpt e)
derive instance Traversable e => Traversable (DocOpt e)

instance Apply e => Apply (DocOpt e) where
   apply None _ = None
   apply (Doc doc) (Doc doc') = Doc (zipWith (<*>) doc doc')
   apply _ _ = error $ shapeMismatch unit

instance Apply e => Apply (DocCommentElem e) where
   apply (Token s) (Token s') = Token (s ≜ s')
   apply (CExpr e) (CExpr e') = CExpr (e <*> e')
   apply _ _ = error $ shapeMismatch unit

instance Vertices (e Vertex) => Vertices (DocCommentElem e Vertex) where
   vertices (Token _) = Set.empty
   vertices (CExpr e) = vertices e

docVertices :: forall e. Vertices (e Vertex) => DocOpt e Vertex -> Set DVertex
docVertices None = Set.empty
docVertices (Doc doc) = Set.unions (vertices <$> doc)

instance (BoundedJoinSemilattice a, Expandable (e a) (Raw e)) => Expandable (DocOpt e a) (DocOpt e Unit) where
   expand None _ = None
   expand (Doc doc) (Doc doc') = Doc (expand doc doc')
   expand _ _ = error $ shapeMismatch unit

instance (BoundedJoinSemilattice a, (Expandable (e a) (Raw e))) => Expandable (DocCommentElem e a) (DocCommentElem e Unit) where
   expand (Token s) (Token s') = Token (s ≜ s')
   expand (CExpr e) (CExpr e') = CExpr (expand e e')
   expand _ _ = error $ shapeMismatch unit

instance (Show (e a)) => Show (DocOpt e a) where
   show None = "None"
   show (Doc doc) = "Doc " <> show doc

instance (Show (e a)) => Show (DocCommentElem e a) where
   show (Token s) = "Token " <> show s
   show (CExpr e) = "CExpr " <> show e
