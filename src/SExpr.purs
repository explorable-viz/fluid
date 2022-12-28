module SExpr where

import Prelude

import Bindings (Bind, Var)
import Data.Either (Either(..))
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import DataType (Ctr)
import Lattice (class JoinSemilattice, definedJoin, neg)
import Util (type (×), (×), type (+), error, unimplemented)
import Util.Pair (Pair)

-- Surface language expressions.
data Expr a
   = Var Var
   | Op Var
   | Int a Int
   | Float a Number
   | Str a String
   | Constr a Ctr (List (Expr a))
   | Record a (List (Bind (Expr a)))
   | Dictionary a (List (Pair (Expr a)))
   | Matrix a (Expr a) (Var × Var) (Expr a)
   | Lambda (NonEmptyList (Branch a))
   | Project (Expr a) Var
   | App (Expr a) (Expr a)
   | BinaryApp (Expr a) Var (Expr a)
   | MatchAs (Expr a) (NonEmptyList (Pattern × Expr a))
   | IfElse (Expr a) (Expr a) (Expr a)
   | ListEmpty a -- called [] in the paper
   | ListNonEmpty a (Expr a) (ListRest a)
   | ListEnum (Expr a) (Expr a)
   | ListComp a (Expr a) (NonEmptyList (Qualifier a))
   | Let (VarDefs a) (Expr a)
   | LetRec (RecDefs a) (Expr a)

data ListRest a
   = End a
   | Next a (Expr a) (ListRest a)

data Pattern
   = PVar Var
   | PConstr Ctr (List Pattern)
   | PRecord (List (Bind Pattern))
   | PListEmpty
   | PListNonEmpty Pattern ListRestPattern

data ListRestPattern
   = PEnd
   | PNext Pattern ListRestPattern

-- in the spec, "clause" doesn't include the function name
type Branch a = NonEmptyList Pattern × Expr a
type Clause a = Var × Branch a
type RecDefs a = NonEmptyList (Clause a)

-- The pattern/expr relationship is different to the one in branch (the expr is the "argument", not the "body").
-- Using a data type makes for easier overloading.
data VarDef a = VarDef Pattern (Expr a)
type VarDefs a = NonEmptyList (VarDef a)

data Qualifier a
   = Guard (Expr a)
   | Generator Pattern (Expr a)
   | Declaration (VarDef a) -- could allow VarDefs instead

data Module a = Module (List (VarDefs a + RecDefs a))

-- ======================
-- boilerplate
-- ======================
derive instance Functor Expr
derive instance Functor ListRest
derive instance Functor VarDef
derive instance Functor Qualifier

instance Functor Module where
   map f (Module defs) = Module (mapDefs f <$> defs)
      where
      mapDefs :: forall a b. (a -> b) -> VarDefs a + RecDefs a -> VarDefs b + RecDefs b
      mapDefs g (Left ds) = Left $ map g <$> ds
      mapDefs g (Right ds) = Right $ (\(x × (ps × s)) -> x × (ps × (g <$> s))) <$> ds

instance JoinSemilattice a => JoinSemilattice (Expr a) where
   join s = definedJoin s
   maybeJoin _ = error unimplemented
   neg = (<$>) neg
