module SExpr2 where

import Prelude

import Bindings (Bind, Var)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import DataType (Ctr, cFalse, cNil, cTrue, cCons)
import Dict as D
import Expr2 (Expr(..), VarDef(..)) as E
import Expr2 (class Desugarable, Cont(..), Elim(..), Expr, desug, mkSugar)
import Lattice2 (class JoinSemilattice, definedJoin, join, maybeJoin, neg)
import Util (type (×), (×), type (+), error, unimplemented)

scons :: forall a. a -> E.Expr a -> E.Expr a -> E.Expr a
scons ann head rest = E.Constr ann cCons (head : rest : Nil)

instance JoinSemilattice a => Desugarable SExpr a where
    desug (BinaryApp l op r)           = E.App (E.App (E.Op op) l) r 
    desug (MatchAs guard patterns)     = E.App (E.Lambda (clauses patterns)) guard 
    desug (IfElse guard trueP falseP)  = E.App (E.Lambda (elimBool (ContExpr trueP) (ContExpr falseP))) guard
                                         where 
                                             elimBool :: forall a'. Cont a' -> Cont a' -> Elim a'
                                             elimBool κ κ' = ElimConstr (D.fromFoldable [ cTrue × κ, cFalse × κ' ])
    desug (ListEmpty ann)              = E.Constr ann cNil Nil
    desug (ListNonEmpty ann head rest) = scons ann head (mkSugar rest)
    desug (ListEnum head last)         = E.App (E.App (E.Var "enumFromTo") head) last
    desug (ListComp ann head quals)    = error "todo"
    desug (Let defs exp)               = error "todo"
    desug (LetRec recdefs exp)         = error "todo"

instance JoinSemilattice a => Desugarable ListRest a where
    desug (End ann) = E.Constr ann cNil Nil
    desug (Next ann head rest) = scons ann (desug head) (mkSugar rest)

-- instance JoinSemilattice a => Desugarable VarDefs a where
processVarDefs :: forall a. JoinSemilattice a => VarDefs a × SExpr a -> E.Expr a
processVarDefs (NonEmptyList (d :| Nil) × exp) = E.Let (processVarDef d) (mkSugar exp)
processVarDefs (NonEmptyList (d :| d' : ds) × exp) = 
    E.Let (processVarDef d) (processVarDefs (NonEmptyList (d' :| ds) × exp))

processVarDef :: forall a. JoinSemilattice a => VarDef a -> E.VarDef a
processVarDef (VarDef pat exp) = E.VarDef (desugPWithC pat (ContNone :: Cont a)) (mkSugar exp)

clause :: forall a. JoinSemilattice a => Pattern × Expr a -> Elim a
clause (pat × exp) = let cont = ContExpr exp in desugPWithC pat cont

clauses :: forall a. JoinSemilattice a => NonEmptyList (Pattern × Expr a) -> Elim a
clauses cls = let 
                NonEmptyList (head :| rest) = map clause cls 
              in
                foldl join head rest

desugPWithC :: forall a. Pattern -> Cont a -> Elim a
desugPWithC (PVar x)              k = ElimVar x k
desugPWithC (PConstr c ps)        k = error "todo"
desugPWithC (PRecord bps)         k = error "todo"
desugPWithC  PListEmpty           k = error "todo"
desugPWithC (PListNonEmpty p lrp) k = error "todo" 


-- Surface language expressions.
data SExpr a
   = BinaryApp (Expr a) Var (Expr a)
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
   | Next a (SExpr a) (ListRest a)

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
type Branch a = NonEmptyList Pattern × SExpr a
type Clause a = Var × Branch a
type RecDefs a = NonEmptyList (Clause a)

-- The pattern/expr relationship is different to the one in branch (the expr is the "argument", not the "body").
-- Using a data type makes for easier overloading.
data VarDef a = VarDef Pattern (SExpr a)
type VarDefs a = NonEmptyList (VarDef a)

data Qualifier a
   = Guard (SExpr a)
   | Generator Pattern (SExpr a)
   | Declaration (VarDef a) -- could allow VarDefs instead

data Module a = Module (List (VarDefs a + RecDefs a))

-- ======================
-- boilerplate
-- ======================
derive instance Functor SExpr
derive instance Functor ListRest
derive instance Functor VarDef
derive instance Functor Qualifier

instance Functor Module where
   map f (Module defs) = Module (mapDefs f <$> defs)
      where
      mapDefs :: forall a b. (a -> b) -> VarDefs a + RecDefs a -> VarDefs b + RecDefs b
      mapDefs g (Left ds) = Left $ map g <$> ds
      mapDefs g (Right ds) = Right $ (\(x × (ps × s)) -> x × (ps × (g <$> s))) <$> ds

instance JoinSemilattice a => JoinSemilattice (SExpr a) where
   join s = definedJoin s
   maybeJoin _ = error unimplemented
   neg = (<$>) neg
