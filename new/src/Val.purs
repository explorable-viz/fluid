module Val where

import Prelude hiding (absurd, top)
import Bindings (Bindings)
import Elim (Elim)
import Expr (RecDefs, Expr)
import Lattice (class Selectable, Selected, (∨?), mapα, maybeZipWithα)
import Util (error, (≟))
import Data.Maybe (Maybe(..))

data Unary =
   IntStr (Int -> String)

data Binary =
   IntIntInt (Int -> Int -> Int) |
   IntIntBool (Int -> Int -> Boolean)

-- String arguments are "internal" names for printing, unrelated to any user-level identifiers.
data UnaryOp =
   UnaryOp String Unary |
   PartialApp BinaryOp Val

data BinaryOp = BinaryOp String Binary

data RawVal =
   True | False |
   Int Int |
   Str String |
   Closure Env RecDefs (Elim Expr) |
   Binary BinaryOp |
   Unary UnaryOp |
   Pair Val Val |
   Nil | Cons Val Val

data Val = Val Selected RawVal

val :: RawVal -> Val
val = Val false

type Env = Bindings Val

instance selectableVal :: Selectable Val where
   mapα f (Val α u) = Val (f α) u
   maybeZipWithα f (Val α r) (Val α' r') = Val <$> pure (α `f` α') <*> maybeZipWithα f r r'

instance selectableRawVal :: Selectable RawVal where
   mapα _ (Int x) = Int x
   mapα _ (Str s) = Str s
   mapα _ False = False
   mapα _ True = True
   mapα _ Nil = Nil
   mapα f (Cons e1 e2) = Cons (mapα f e1) (mapα f e2)
   mapα f (Pair e1 e2) = Pair (mapα f e1) (mapα f e2)
   mapα f (Closure ρ δ σ) = error "todo"
   mapα f (Binary φ) = error "todo"
   mapα f (Unary φ) = error "todo"

   maybeZipWithα f (Int x) (Int x') = x ≟ x' <#> Int
   maybeZipWithα f (Str s) (Str s') = s ≟ s' <#> Str
   maybeZipWithα f False False = pure False
   maybeZipWithα f True True = pure True
   maybeZipWithα f Nil Nil = pure Nil
   maybeZipWithα f (Cons e1 e2) (Cons e1' e2') = Cons <$> e1 ∨? e1' <*> e2' ∨? e2'
   maybeZipWithα f (Pair e1 e2) (Pair e1' e2') = Pair <$> e1 ∨? e1' <*> e2 ∨? e2'
   maybeZipWithα f (Closure ρ δ σ) (Closure ρ' δ' σ') = error "todo"
   maybeZipWithα f (Binary φ) (Binary φ') = error "todo"
   maybeZipWithα f (Unary φ) (Unary φ') = error "todo"
   maybeZipWithα f _ _ = Nothing
