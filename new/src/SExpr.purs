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
   Lambda (NonEmptyList (Branch a)) |
   App (Expr a) (Expr a) |
   BinaryApp (Expr a) Var (Expr a) |
   MatchAs (Expr a) (NonEmptyList (Branch a)) |
   IfElse (Expr a) (Expr a) (Expr a) |
   ListRange (Expr a) (Expr a) |
   ListComp (Expr a) (List (Qualifier a)) |
   Let (VarDefs a) (Expr a) |
   LetRec (RecDefs a) (Expr a)

data Pattern =
   PVar Var |
   PConstr Ctr (List Pattern)

type Branch a = NonEmptyList Pattern × Expr a
type Clause a = Var × Branch a
type RecDefs a = NonEmptyList (Clause a)
type VarDef a = Pattern × Expr a
type VarDefs a = List (VarDef a)

data Qualifier a =
   Guard (Expr a) |
   Generator Pattern (Expr a) |
   Declaration Pattern (Expr a)

data Expr a =
   Expr a (RawExpr a)

data Module a = Module (List (VarDefs a + RecDefs a))

expr :: forall a . BoundedJoinSemilattice a => RawExpr a -> Expr a
expr = Expr bot
