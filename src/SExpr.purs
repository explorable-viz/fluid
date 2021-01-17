module SExpr where

import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Lattice (class BoundedJoinSemilattice, bot)
import DataType (Ctr)
import Expr (Var)
import Util (type (×), type (+))

-- Surface language expressions.
data RawExpr a =
   Var Var |
   Op Var |
   Int Int |
   Float Number |
   Str String |
   Constr Ctr (List (Expr a)) |
   Matrix (Expr a) (Var × Var) (Expr a) |
   Lambda (NonEmptyList (Branch a)) |
   App (Expr a) (Expr a) |
   BinaryApp (Expr a) Var (Expr a) |
   MatchAs (Expr a) (NonEmptyList (Pattern × Expr a)) |
   IfElse (Expr a) (Expr a) (Expr a) |
   ListEmpty |  -- in the formalism we unify with Nil, but cleaner here to keep separate
   ListNonEmpty (Expr a) (ListRest a) |
   ListEnum (Expr a) (Expr a) |
   ListComp (Expr a) (NonEmptyList (Qualifier a)) |
   Let (VarDefs a) (Expr a) |
   LetRec (RecDefs a) (Expr a)

data ListRest a =
   End a | Next a (Expr a) (ListRest a)

data Pattern =
   PVar Var |
   PConstr Ctr (List Pattern) |
   PListEmpty |
   PListNonEmpty Pattern ListPatternRest

data ListPatternRest =
   PEnd | PNext Pattern ListPatternRest

-- in the spec, "clause" doesn't include the function name
type Branch a = NonEmptyList Pattern × Expr a
type Clause a = Var × Branch a
type RecDefs a = NonEmptyList (Clause a)

-- The pattern/expr relationship is different to the one in branch (the expr is the "argument", not the "body").
-- Using a data type makes for easier overloading.
data VarDef a = VarDef Pattern (Expr a)
type VarDefs a = NonEmptyList (VarDef a)

data Qualifier a =
   Guard a (Expr a) |
   Generator a Pattern (Expr a) |
   Declaration a (VarDef a) -- could allow VarDefs instead

data Expr a =
   Expr a (RawExpr a)

data Module a = Module (List (VarDefs a + RecDefs a))

expr :: forall a . BoundedJoinSemilattice a => RawExpr a -> Expr a
expr = Expr bot
