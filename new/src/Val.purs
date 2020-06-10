module Val where

import Prelude hiding (absurd, top)
import Data.List (zipWith)
import Data.Traversable (sequence)
import Bindings (Bindings)
import Elim (Elim)
import Expr (RecDefs, Expr)
import Lattice (class Selectable, Selected, mapα, maybeZipWithα)
import Util ((≟))
import Data.Maybe (Maybe(..))

data Unary =
   IntStr (Int -> String)

data Binary =
   IntIntInt (Int -> Int -> Int) |
   IntIntBool (Int -> Int -> Boolean)

-- String arguments are "internal" names for printing and equality testing, unrelated to user-level identifiers.
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

instance selectableUnaryOp :: Selectable UnaryOp where
   mapα _ = identity

   maybeZipWithα f (UnaryOp op f') (UnaryOp op' _)     = UnaryOp <$> op ≟ op' <*> pure f'
   maybeZipWithα f (PartialApp φ v) (PartialApp φ' v') = PartialApp <$> maybeZipWithα f φ φ' <*> maybeZipWithα f v v'
   maybeZipWithα _ _ _                                 = Nothing

instance selectableBinaryOp :: Selectable BinaryOp where
   mapα _ = identity
   maybeZipWithα f (BinaryOp op f') (BinaryOp op' _) = BinaryOp <$> op ≟ op' <*> pure f'

instance selectableVal :: Selectable Val where
   mapα f (Val α u)                       = Val (f α) u
   maybeZipWithα f (Val α r) (Val α' r')  = Val <$> pure (α `f` α') <*> maybeZipWithα f r r'

instance selectableRawVal :: Selectable RawVal where
   mapα _ (Int x)          = Int x
   mapα _ (Str s)          = Str s
   mapα _ False            = False
   mapα _ True             = True
   mapα _ Nil              = Nil
   mapα f (Cons e1 e2)     = Cons (mapα f e1) (mapα f e2)
   mapα f (Pair e1 e2)     = Pair (mapα f e1) (mapα f e2)
   mapα f (Closure ρ δ σ)  = Closure (mapα f ρ) (map (mapα f) δ) (mapα f σ)
   mapα f (Binary φ)       = Binary (mapα f φ)
   mapα f (Unary φ)        = Unary (mapα f φ)

   maybeZipWithα f (Int x) (Int x')                   = Int <$> x ≟ x'
   maybeZipWithα f (Str s) (Str s')                   = Str <$> s ≟ s'
   maybeZipWithα f False False                        = pure False
   maybeZipWithα f True True                          = pure True
   maybeZipWithα f Nil Nil                            = pure Nil
   maybeZipWithα f (Cons e1 e2) (Cons e1' e2')        = Cons <$> maybeZipWithα f e1 e1' <*> maybeZipWithα f e2 e2'
   maybeZipWithα f (Pair e1 e2) (Pair e1' e2')        = Pair <$> maybeZipWithα f e1 e1' <*> maybeZipWithα f e2 e2'
   maybeZipWithα f (Closure ρ δ σ) (Closure ρ' δ' σ') =
      Closure <$> maybeZipWithα f ρ ρ'
              <*> (sequence $ zipWith (maybeZipWithα f) δ δ') <*> maybeZipWithα f σ σ'
   maybeZipWithα f (Binary φ) (Binary φ')             = Binary <$> maybeZipWithα f φ φ'
   maybeZipWithα f (Unary φ) (Unary φ')               = Unary <$> maybeZipWithα f φ φ'
   maybeZipWithα f _ _                                = Nothing
