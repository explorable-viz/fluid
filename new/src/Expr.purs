module Expr where

import Prelude hiding (top)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import DataType (Ctr)
import Lattice (class BoundedJoinSemilattice, class JoinSemilattice, class MaybeZippable, 𝔹, (∨), maybeJoin, maybeZipWith, maybeZipWithList, maybeZipWithMap)
import Util (type (+), (≟), error)

type Var = String

varAnon = "_" :: Var

data VarDef a = VarDef (Elim a) (Expr a) -- elim has codomain unit
type VarDefs a = List (VarDef a)

data RecDef a = RecDef Var (Elim a)
type RecDefs a = List (RecDef a)

data RawExpr a =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List (Expr a)) |
   Lambda (Elim a) |
   App (Expr a) (Expr a) |
   BinaryApp (Expr a) Var (Expr a) |
   MatchAs (Expr a) (Elim a) |
   Let (VarDef a) (Expr a) |
   LetRec (RecDefs a) (Expr a)

data Expr a = Hole | Expr a (RawExpr a)

expr :: RawExpr 𝔹 -> Expr 𝔹
expr = Expr false

-- Continuation of an eliminator. None form only used in structured let.
data Cont a = None | Body (Expr a) | Arg (Elim a)

body :: Cont 𝔹 -> Expr 𝔹
body (Body e)  = e
body _         = error "Expression expected"

data Elim a =
   ElimVar Var (Cont a) |
   ElimConstr (Map Ctr (Cont a))

data Module a = Module (List (VarDef a + RecDefs a))

-- ======================
-- boilerplate
-- ======================
derive instance functorVarDef :: Functor VarDef
derive instance functorRecDef :: Functor RecDef
derive instance functorRawExpr :: Functor RawExpr
derive instance functorExpr :: Functor Expr
derive instance functorCont :: Functor Cont
derive instance functorElim :: Functor Elim

instance maybeZippableElim :: MaybeZippable Elim where
   maybeZipWith f (ElimVar x κ) (ElimVar x' κ')      = ElimVar <$> x ≟ x' <*> maybeZipWith f κ κ'
   maybeZipWith f (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeZipWithMap f κs κs'
   maybeZipWith _ _ _                                = Nothing

instance joinSemilatticeElim :: JoinSemilattice (Elim Boolean) where
   maybeJoin (ElimVar x κ) (ElimVar x' κ')      = ElimVar <$> x ≟ x' <*> maybeJoin κ κ'
   maybeJoin (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeJoin κs κs'
   maybeJoin _ _                                = Nothing

instance maybeZippableCont :: MaybeZippable Cont where
   maybeZipWith f None None            = pure None
   maybeZipWith f (Body e) (Body e')   = Body <$> maybeZipWith f e e'
   maybeZipWith f (Arg σ) (Arg σ')     = Arg <$> maybeZipWith f σ σ'
   maybeZipWith _ _ _                  = Nothing

instance joinSemilatticeCont :: JoinSemilattice (Cont Boolean) where
   maybeJoin None None            = pure None
   maybeJoin (Body e) (Body e')   = Body <$> maybeJoin e e'
   maybeJoin (Arg σ) (Arg σ')     = Arg <$> maybeJoin σ σ'
   maybeJoin _ _                  = Nothing

instance maybeZippableVarDef :: MaybeZippable VarDef where
   maybeZipWith f (VarDef σ e) (VarDef σ' e') = VarDef <$> maybeZipWith f σ σ' <*> maybeZipWith f e e'

instance joinSemilatticeVarDef :: JoinSemilattice (VarDef Boolean) where
   maybeJoin (VarDef σ e) (VarDef σ' e') = VarDef <$> maybeJoin σ σ' <*> maybeJoin e e'

instance maybeZippableRecDef :: MaybeZippable RecDef where
   maybeZipWith f (RecDef x σ) (RecDef x' σ') = RecDef <$> x ≟ x' <*> maybeZipWith f σ σ'

instance joinSemilatticeRecDef :: JoinSemilattice (RecDef Boolean) where
   maybeJoin (RecDef x σ) (RecDef x' σ') = RecDef <$> x ≟ x' <*> maybeJoin σ σ'

instance maybeZippableExpr :: MaybeZippable Expr where
   maybeZipWith _ Hole Hole               = pure Hole
   maybeZipWith f (Expr α r) (Expr α' r') = Expr <$> pure (f α α') <*> maybeZipWith f r r'
   maybeZipWith _ _ _                     = Nothing

instance maybeZippableRawExpr :: MaybeZippable RawExpr where
   maybeZipWith _ (Var x) (Var x')                = Var <$> x ≟ x'
   maybeZipWith _ (Op op) (Op op')                = Op <$> op ≟ op'
   maybeZipWith _ (Int n) (Int n')                = Int <$> n ≟ n'
   maybeZipWith _ (Str s) (Var s')                = Str <$> s ≟ s'
   maybeZipWith f (Constr c es) (Constr c' es')
      = Constr <$> c ≟ c' <*> maybeZipWithList f es es'
   maybeZipWith f (App e1 e2) (App e1' e2')
      = App <$> maybeZipWith f e1 e1' <*> maybeZipWith f e2 e2'
   maybeZipWith f (BinaryApp e1 op e2) (BinaryApp e1' op' e2')
      = BinaryApp <$> maybeZipWith f e1 e1' <*> op ≟ op' <*> maybeZipWith f e2 e2'
   maybeZipWith f (Lambda σ) (Lambda σ')
      = Lambda <$> maybeZipWith f σ σ'
   maybeZipWith f (MatchAs e σ) (MatchAs e' σ')
      = MatchAs <$> maybeZipWith f e e' <*> maybeZipWith f σ σ'
   maybeZipWith f (Let def e) (Let def' e')
      = Let <$> maybeZipWith f def def' <*> maybeZipWith f e e'
   maybeZipWith f (LetRec δ e) (LetRec δ' e')
      = LetRec <$> maybeZipWithList f δ δ' <*> maybeZipWith f e e'
   maybeZipWith _ _ _                             = Nothing

instance joinSemilatticeExpr :: JoinSemilattice (Expr Boolean) where
   maybeJoin Hole e                    = pure e
   maybeJoin e Hole                    = pure e
   maybeJoin (Expr α r) (Expr α' r')   = Expr <$> pure (α ∨ α') <*> maybeJoin r r'

instance boundedJoinSemilatticeExpr :: BoundedJoinSemilattice (Expr Boolean) where
   bot2 = const Hole

instance joinSemilatticeRawExpr :: JoinSemilattice (RawExpr Boolean) where
   maybeJoin (Var x) (Var x')              = Var <$> x ≟ x'
   maybeJoin (Op op) (Op op')              = Op <$> op ≟ op'
   maybeJoin (Int n) (Int n')              = Int <$> n ≟ n'
   maybeJoin (Str s) (Var s')              = Str <$> s ≟ s'
   maybeJoin (Constr c es) (Constr c' es') = Constr <$> c ≟ c' <*> maybeJoin es es'
   maybeJoin (App e1 e2) (App e1' e2')     = App <$> maybeJoin e1 e1' <*> maybeJoin e2 e2'
   maybeJoin (BinaryApp e1 op e2) (BinaryApp e1' op' e2')
      = BinaryApp <$> maybeJoin e1 e1' <*> op ≟ op' <*> maybeJoin e2 e2'
   maybeJoin (Lambda σ) (Lambda σ')        = Lambda <$> maybeJoin σ σ'
   maybeJoin (MatchAs e σ) (MatchAs e' σ') = MatchAs <$> maybeJoin e e' <*> maybeJoin σ σ'
   maybeJoin (Let def e) (Let def' e')     = Let <$> maybeJoin def def' <*> maybeJoin e e'
   maybeJoin (LetRec δ e) (LetRec δ' e')   = LetRec <$> maybeJoin δ δ' <*> maybeJoin e e'
   maybeJoin _ _                           = Nothing
