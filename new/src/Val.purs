module Val where

import Prelude hiding (absurd, top)
import Data.List (List, zipWith)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Bindings (Bindings)
import DataType (Ctr)
import DataType (arity) as D
import Expr (Elim, RecDefs)
import Lattice (class Selectable, Selected, mapα, maybeZipWithα)
import Util ((≟), successful)

data Unary =
   IntStr (Int -> String)

data Binary =
   IntIntInt (Int -> Int -> Int) |
   IntIntBool (Int -> Int -> Boolean)

-- String argument is "internal" name for printing/equality testing, unrelated to user-level identifiers.
data UnaryOp =
   UnaryOp String Unary |
   PartialApp BinaryOp Val

data Primitive =
   Constr2 Ctr |
   Unary2 UnaryOp |
   Binary2 BinaryOp |
   PartialApp2 Primitive Val

arity :: Primitive -> Int
arity (Constr2 c)          = successful $ D.arity c
arity (Unary2 φ)           = 1
arity (Binary2 φ)          = 2
arity (PartialApp2 op _)   = arity op - 1

data BinaryOp = BinaryOp String Binary

data RawVal =
   Int Int |
   Str String |
   Constr Ctr (List Val) |
   Closure Env RecDefs Elim |
   Binary BinaryOp |
   Unary UnaryOp |
   Primitive Primitive

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

instance selectablePrimitive :: Selectable Primitive where
   mapα f (Constr2 c)         = Constr2 c
   mapα f (Unary2 φ)          = Unary2 (mapα f φ)
   mapα f (Binary2 φ)         = Binary2 (mapα f φ)
   mapα f (PartialApp2 op v)  = PartialApp2 (mapα f op) (mapα f v)

   maybeZipWithα f (Constr2 c) (Constr2 c')                 = Constr2 <$> c ≟ c'
   maybeZipWithα f (Unary2 φ) (Unary2 φ')                   = Unary2 <$> maybeZipWithα f φ φ'
   maybeZipWithα f (Binary2 φ) (Binary2 φ')                 = Binary2 <$> maybeZipWithα f φ φ'
   maybeZipWithα f (PartialApp2 op v) (PartialApp2 op' v')  =
      PartialApp2 <$> maybeZipWithα f op op' <*> maybeZipWithα f v v'
   maybeZipWithα _ _ _                                      = Nothing

instance selectableVal :: Selectable Val where
   mapα f (Val α u)                       = Val (f α) u
   maybeZipWithα f (Val α r) (Val α' r')  = Val <$> pure (α `f` α') <*> maybeZipWithα f r r'

instance selectableRawVal :: Selectable RawVal where
   mapα _ (Int x)          = Int x
   mapα _ (Str s)          = Str s
   mapα f (Constr c es)    = Constr c (map (mapα f) es)
   mapα f (Closure ρ δ σ)  = Closure (mapα f ρ) (map (mapα f) δ) (mapα f σ)
   mapα f (Binary φ)       = Binary (mapα f φ)
   mapα f (Unary φ)        = Unary (mapα f φ)
   mapα f (Primitive op)   = Primitive (mapα f op)

   maybeZipWithα f (Int x) (Int x')                   = Int <$> x ≟ x'
   maybeZipWithα f (Str s) (Str s')                   = Str <$> s ≟ s'
   maybeZipWithα f (Constr c es) (Constr c' es') =
      Constr <$> c ≟ c' <*> sequence (zipWith (maybeZipWithα f) es es')
   maybeZipWithα f (Closure ρ δ σ) (Closure ρ' δ' σ') =
      Closure <$> maybeZipWithα f ρ ρ'
              <*> sequence (zipWith (maybeZipWithα f) δ δ') <*> maybeZipWithα f σ σ'
   maybeZipWithα f (Binary φ) (Binary φ')             = Binary <$> maybeZipWithα f φ φ'
   maybeZipWithα f (Unary φ) (Unary φ')               = Unary <$> maybeZipWithα f φ φ'
   maybeZipWithα _ _ _                                = Nothing
