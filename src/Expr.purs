module Expr where

import Prelude hiding (top)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Bindings (Bindings)
import DataType (Ctr)
import Lattice (
   class BoundedJoinSemilattice, class BoundedSlices, class JoinSemilattice, class Slices,
   𝔹, (∨), bot, botOf, definedJoin, maybeJoin
)
import Util (type(×), type (+), (≟), error)

type Var = String

varAnon = "_" :: Var

data RawExpr a =
   Var Var |
   Op Var |
   Int Int |
   Float Number |
   Str String |
--   Array (Expr a) (Var × Var) (Expr a) |
   Constr Ctr (List (Expr a)) |
   Lambda (Elim a) |
   App (Expr a) (Expr a) |
   BinaryApp (Expr a) Var (Expr a) |
   Let (VarDef a) (Expr a) |
   LetRec (RecDefs a) (Expr a)

data Expr a = Hole | Expr a (RawExpr a)

expr :: forall a . BoundedJoinSemilattice a => RawExpr a -> Expr a
expr = Expr bot

data VarDef a = VarDef (Elim a) (Expr a) -- elim has codomain unit
type RecDefs = Bindings Elim

data Elim a =
   ElimVar Var (Cont a) |
   ElimConstr (Map Ctr (Cont a))

-- Continuation of an eliminator. None form only used in structured let.
data Cont a = None | Body (Expr a) | Arg (Elim a)

body :: Cont 𝔹 -> Expr 𝔹
body (Body e)  = e
body _         = error "Expression expected"

data Module a = Module (List (VarDef a + RecDefs a))

-- ======================
-- boilerplate
-- ======================
derive instance functorVarDef :: Functor VarDef
derive instance functorRawExpr :: Functor RawExpr
derive instance functorExpr :: Functor Expr
derive instance functorCont :: Functor Cont
derive instance functorElim :: Functor Elim

instance joinSemilatticeElim :: JoinSemilattice a => JoinSemilattice (Elim a) where
   join = definedJoin

instance slicesElim :: JoinSemilattice a => Slices (Elim a) where
   maybeJoin (ElimVar x κ) (ElimVar x' κ')      = ElimVar <$> x ≟ x' <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeJoin κs κs'
   maybeJoin _ _                                = Nothing

instance boundedSlicesElim :: JoinSemilattice a => BoundedSlices (Elim a) where
   botOf (ElimVar x κ)   = ElimVar x (botOf κ)
   botOf (ElimConstr κs) = ElimConstr $ map botOf κs

instance joinSemilatticeCont :: JoinSemilattice a => JoinSemilattice (Cont a) where
   join = definedJoin

instance slicesCont :: JoinSemilattice a => Slices (Cont a) where
   maybeJoin None None            = pure None
   maybeJoin (Body e) (Body e')   = Body <$> maybeJoin e e'
   maybeJoin (Arg σ) (Arg σ')     = Arg <$> maybeJoin σ σ'
   maybeJoin _ _                  = Nothing

instance boundedSlicesCont :: JoinSemilattice a => BoundedSlices (Cont a) where
   botOf None      = None
   botOf (Body e)  = Body $ botOf e
   botOf (Arg σ)   = Arg $ botOf σ

instance joinSemilatticeVarDef :: JoinSemilattice a => JoinSemilattice (VarDef a) where
   join = definedJoin

instance slicesVarDef :: JoinSemilattice a => Slices (VarDef a) where
   maybeJoin (VarDef σ e) (VarDef σ' e') = VarDef <$> maybeJoin σ σ' <*> maybeJoin e e'

instance joinSemilatticeExpr :: JoinSemilattice a => JoinSemilattice (Expr a) where
   join = definedJoin

instance slicesExpr :: JoinSemilattice a => Slices (Expr a) where
   maybeJoin Hole e                    = pure e
   maybeJoin e Hole                    = pure e
   maybeJoin (Expr α r) (Expr α' r')   = Expr <$> pure (α ∨ α') <*> maybeJoin r r'

instance boundedSlicesExpr :: JoinSemilattice a => BoundedSlices (Expr a) where
   botOf = const Hole

instance joinSemilatticeRawExpr :: JoinSemilattice a => JoinSemilattice (RawExpr a) where
   join = definedJoin

instance slicesRawExpr :: JoinSemilattice a => Slices (RawExpr a) where
   maybeJoin (Var x) (Var x')              = Var <$> x ≟ x'
   maybeJoin (Op op) (Op op')              = Op <$> op ≟ op'
   maybeJoin (Int n) (Int n')              = Int <$> n ≟ n'
   maybeJoin (Str str) (Str str')          = Str <$> str ≟ str'
   maybeJoin (Float n) (Float n')          = Float <$> n ≟ n'
   maybeJoin (Str s) (Var s')              = Str <$> s ≟ s'
   maybeJoin (Constr c es) (Constr c' es') = Constr <$> c ≟ c' <*> maybeJoin es es'
   maybeJoin (App e1 e2) (App e1' e2')     = App <$> maybeJoin e1 e1' <*> maybeJoin e2 e2'
   maybeJoin (BinaryApp e1 op e2) (BinaryApp e1' op' e2')
      = BinaryApp <$> maybeJoin e1 e1' <*> op ≟ op' <*> maybeJoin e2 e2'
   maybeJoin (Lambda σ) (Lambda σ')        = Lambda <$> maybeJoin σ σ'
   maybeJoin (Let def e) (Let def' e')     = Let <$> maybeJoin def def' <*> maybeJoin e e'
   maybeJoin (LetRec δ e) (LetRec δ' e')   = LetRec <$> maybeJoin δ δ' <*> maybeJoin e e'
   maybeJoin _ _                           = Nothing
