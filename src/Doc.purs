module Doc where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Set as Set
import Graph (class Vertices, Vertex, vertices)
import Lattice (class BoundedJoinSemilattice, class Expandable, class JoinSemilattice, Raw, expand, (∨))
import Util (error, shapeMismatch, (≜))

type DocOpt :: (Type -> Type) -> Type -> Type
type DocOpt e a = Maybe (DocComment e a)

type DocComment :: (Type -> Type) -> Type -> Type
type DocComment e a = List (DocCommentElem e a)

data DocCommentElem :: (Type -> Type) -> Type -> Type
data DocCommentElem e a = Token String | CExpr (e a)

instance (Eq (e a)) => Eq (DocCommentElem e a) where
   eq (Token s) (Token s') = s == s'
   eq (CExpr e) (CExpr e') = e == e'
   eq _ _ = false

instance JoinSemilattice (e a) => JoinSemilattice (DocCommentElem e a) where
   join (Token s) (Token s') = Token (s ≜ s')
   join (CExpr e) (CExpr e') = CExpr (e ∨ e')
   join _ _ = error (shapeMismatch unit)

derive instance (Functor e) => Functor (DocCommentElem e)

instance Apply e => Apply (DocCommentElem e) where
   apply (Token s) (Token s') = Token (s ≜ s')
   apply (CExpr fe) (CExpr e) = CExpr (fe <*> e)
   apply _ _ = error (shapeMismatch unit)

instance Vertices (e Vertex) => Vertices (DocCommentElem e Vertex) where
   vertices (Token _) = Set.empty
   vertices (CExpr e) = vertices e

instance (BoundedJoinSemilattice a, Expandable (e a) (Raw e)) => Expandable (DocCommentElem e a) (DocCommentElem e Unit) where
   expand (Token s) (Token s') = Token (s ≜ s')
   expand (CExpr e) (CExpr e') = CExpr (expand e e')
   expand _ _ = error (shapeMismatch unit)
