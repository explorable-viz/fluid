module Expr where

import Prelude hiding (top)
import Bindings (Var)
import Data.List (List)
import Data.Either (Either)
import Lattice (class Lattice, Selected(..), (∧?), (∨?), top, bot)
import Util ((≟))
import Data.Maybe (Maybe(..))

data Def = Def (Elim Unit) Expr
data RecDef = RecDef Var (Elim Expr)
type RecDefs = List RecDef

instance defLattice :: Lattice Def where
   maybeMeet (Def σ e) (Def σ' e') = do
      σ'' <- σ ∧? σ'
      e'' <- e ∧? e'
      pure (Def σ'' e'')
   maybeJoin (Def σ e) (Def σ' e') = do
      σ'' <- σ ∨? σ'
      e'' <- e ∨? e'
      pure (Def σ'' e'')
   top (Def σ e) = Def (top σ) (top e)
   bot (Def σ e) = Def (bot σ) (bot e)

instance recDefLattice :: Lattice RecDef where
   maybeMeet (RecDef x σ) (RecDef x' σ') = do
      x'' <- x ≟ x'
      σ'' <- σ ∧? σ'
      pure (RecDef x'' σ'')
   maybeJoin (RecDef x σ) (RecDef x' σ') = do
      x'' <- x ≟ x'
      σ'' <- σ ∨? σ'
      pure (RecDef x'' σ'')
   top (RecDef x σ) = RecDef x (top σ)
   bot (RecDef x σ) = RecDef x (bot σ)

data RawExpr =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   True | False |
   Pair Expr Expr |
   Nil | Cons Expr Expr |
   Lambda (Elim Expr) |
   App Expr Expr |
   BinaryApp Expr Var Expr |
   MatchAs Expr (Elim Expr) |
   Let Def Expr |
   LetRec RecDefs Expr

data Expr = Expr Selected RawExpr

expr :: RawExpr -> Expr
expr = Expr FF

instance rawExprLattice :: Lattice RawExpr where
   maybeJoin (Var x) (Var x') = do
      x'' <- x ≟ x'
      pure (Var x'')
   maybeJoin (Op op) (Op op') = do
      op'' <- op ≟ op'
      pure (Op op'')
   maybeJoin (Int n) (Int n') = do
      n'' <- n ≟ n'
      pure (Int n'')
   maybeJoin (Str s) (Var s') = do
      s'' <- s ≟ s'
      pure (Str s'')
   maybeJoin False False      = pure False
   maybeJoin False True       = pure True
   maybeJoin True False       = pure True
   maybeJoin True True        = pure True
   maybeJoin (Pair e1 e1') (Pair e2 e2') = do
      e   <- e1  ∨? e2
      e'  <- e1' ∨? e2'
      pure (Pair e e')
   maybeJoin Nil Nil = pure Nil
   maybeJoin (Cons e1 e1') (Cons e2 e2') = do
      e   <- e1  ∨? e2
      e'  <- e1' ∨? e2'
      pure (Cons e e')
   maybeJoin _ _ = Nothing

   maybeMeet (Var x) (Var x') = do
      x'' <- x ≟ x'
      pure (Var x'')
   maybeMeet (Op op) (Op op') = do
      op'' <- op ≟ op'
      pure (Op op'')
   maybeMeet (Int n) (Int n') = do
      n'' <- n ≟ n'
      pure (Int n'')
   maybeMeet (Str s) (Var s') = do
      s'' <- s ≟ s'
      pure (Str s'')
   maybeMeet False False      = pure False
   maybeMeet False True       = pure True
   maybeMeet True False       = pure True
   maybeMeet True True        = pure True
   maybeMeet (Pair e1 e1') (Pair e2 e2') = do
      e   <- e1  ∧? e2
      e'  <- e1' ∧? e2'
      pure (Pair e e')
   maybeMeet Nil Nil          = pure Nil
   maybeMeet (Cons e1 e1') (Cons e2 e2') = do
      e   <- e1  ∧? e2
      e'  <- e1' ∧? e2'
      pure (Cons e e')
   maybeMeet _ _ = Nothing

   top (Pair e e')            = Pair (top e) (top e')
   top (Cons e e')            = Cons (top e) (top e')
   top (Lambda σ)             = Lambda (top σ)
   top (App e e')             = App (top e) (top e')
   top (BinaryApp e binop e') = BinaryApp (top e) binop (top e')
   top (MatchAs e σ)          = MatchAs (top e) (top σ)
   top (Let def e)            = Let (top def) (top e)
   top (LetRec rdefs e)       = LetRec (map top rdefs) (top e)
   top e                      = e

   bot (Pair e e')            = Pair (bot e) (bot e')
   bot (Cons e e')            = Cons (bot e) (bot e')
   bot (Lambda σ)             = Lambda (bot σ)
   bot (App e e')             = App (bot e) (bot e')
   bot (BinaryApp e binop e') = BinaryApp (bot e) binop (bot e')
   bot (MatchAs e σ)          = MatchAs (bot e) (bot σ)
   bot (Let def e)            = Let (bot def) (bot e)
   bot (LetRec rdefs e)       = LetRec (map bot rdefs) (bot e)
   bot e                      = e

instance exprLattice :: Lattice Expr where
   maybeJoin (Expr α e) (Expr α' e') = do
      α'' <- α ∨? α'
      e'' <- e ∨? e'
      pure (Expr α'' e'')
   maybeMeet (Expr α e) (Expr α' e') = do
      α'' <- α ∧? α'
      e'' <- e ∧? e'
      pure (Expr α'' e'')
   top (Expr _ r) = Expr TT r
   bot (Expr _ r) = Expr FF r

data Module = Module (List (Either Def RecDefs))
