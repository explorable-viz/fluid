module Primitive where

import Partial.Unsafe (unsafePartial)
import Prelude hiding (absurd, apply, div)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Profunctor.Choice ((|||))
import Data.Tuple (fst)
import Text.Parsing.Parser.Expr (Assoc)
import Bindings (Var)
import DataType (cFalse, cPair, cTrue)
import Lattice (𝔹, (∧))
import Util (type (×), (×), type (+), error)
import Val (PrimOp(..), Val(..))

-- name in user land, precedence 0 from 9 (similar to Haskell 98), associativity
type OpDef = {
   op    :: Var,
   prec  :: Int,
   assoc :: Assoc
}

opDef :: Var -> Int -> Assoc -> Var × OpDef
opDef op prec assoc = op × { op, prec, assoc }

-- Mediates between Val and underlying data, analously to pattern-matching and construction for data types.
class ToFrom a where
   constr :: a × 𝔹 -> Val 𝔹
   constr_bwd :: Val 𝔹 -> a × 𝔹  -- equivalent to "match" except in the Val case
   match :: Val 𝔹 -> a × 𝔹       -- only defined for non-holes
   expand :: a -> Val 𝔹          -- use just enough information from supplied value to construct an argument to "match"

match_fwd :: forall a . ToFrom a => Val 𝔹 × a -> a × 𝔹
match_fwd (Hole × v') = match (expand v')
match_fwd (v × _)     = match v

match_bwd :: forall a . ToFrom a => a × 𝔹 -> Val 𝔹
match_bwd = constr

-- Analogous to "variable" case in pattern-matching (or "use existing subvalue" case in construction).
instance toFromVal :: ToFrom (Val Boolean) where
   constr = fst               -- construction rights not required
   constr_bwd = (_ × false)   -- return unit of disjunction rather than conjunction
   match = (_ × true)         -- construction rights are always provided
   expand = identity

instance toFromInt :: ToFrom Int where
   match (Int α n)   = n × α
   match _           = error "Int expected"

   constr (n × α) = Int α n
   constr_bwd v = match v
   expand n = constr (n × false)

instance toFromNumber :: ToFrom Number where
   match (Float α n) = n × α
   match _           = error "Float expected"

   constr (n × α) = Float α n
   constr_bwd v = match v
   expand n = constr (n × false)

instance toFromString :: ToFrom String where
   match (Str α str) = str × α
   match _           = error "Str expected"

   constr (str × α) = Str α str
   constr_bwd v = match v
   expand str = constr (str × false)

instance toFromIntOrNumber :: ToFrom (Int + Number) where
   constr (Left n × α)   = Int α n
   constr (Right n × α)  = Float α n

   constr_bwd v = match v

   match (Int α n)    = Left n × α
   match (Float α n)  = Right n × α
   match _            = error "Int or Float expected"

   expand x = constr (x × false)

instance toFromIntOrNumberOrString :: ToFrom (Either (Either Int Number) String) where
   constr (Left (Left n) × α)  = Int α n
   constr (Left (Right n) × α) = Float α n
   constr (Right str × α)      = Str α str

   constr_bwd v = match v

   match (Int α n)   = Left (Left n) × α
   match (Float α n) = Left (Right n) × α
   match (Str α str) = Right str × α
   match _           = error "Int, Float or Str expected"

   expand x = constr (x × false)

instance toFromIntAndInt :: ToFrom ((Int × Boolean) × (Int × Boolean)) where
   constr (nβ × mβ' × α) = Constr α cPair (constr nβ : constr mβ' : Nil)
   constr_bwd v = match v

   match (Constr α c (v : v' : Nil)) | c == cPair  = match v × match v' × α
   match _                                         = error "Pair expected"

   expand _ = Constr false cPair (Hole : Hole : Nil)

instance toFromMatrixRep :: ToFrom (Array (Array (Val Boolean)) × (Int × Boolean) × (Int × Boolean)) where
   match (Matrix α r) = r × α
   match _            = error "Matrix expected"

   constr (r × α) = Matrix α r
   constr_bwd v = match v
   expand (vss × (i × _) × (j × _)) = Matrix false (((<$>) (const Hole) <$> vss) × (i × false) × (j × false))

instance toFromValAndVal :: ToFrom (Val Boolean × Val Boolean) where
   constr (v × v' × α) = Constr α cPair (v : v' : Nil)
   constr_bwd v = match v

   match (Constr α c (v : v' : Nil)) | c == cPair   = v × v' × α
   match _                                          = error "Pair expected"

   expand _ = Constr false cPair (Hole : Hole : Nil)

instance toFromBoolean :: ToFrom Boolean where
   match (Constr α c Nil)
      | c == cTrue   = true × α
      | c == cFalse  = false × α
   match _ = error "Boolean expected"

   constr (true × α)   = Constr α cTrue Nil
   constr (false × α)  = Constr α cFalse Nil

   constr_bwd v = match v
   expand b = constr (b × false)

class IsZero a where
   isZero :: a -> Boolean

instance isZeroInt :: IsZero Int where
   isZero = ((==) 0)

instance isZeroNumber :: IsZero Number where
   isZero = ((==) 0.0)

instance isZeroEither :: (IsZero a, IsZero b) => IsZero (a + b) where
   isZero = isZero ||| isZero

type Unary a b = {
   f :: a -> b,
   g :: b -> a -> a
}

type UnarySpec a b = {
   fwd :: a × 𝔹 -> b × 𝔹,
   bwd :: b × 𝔹 -> a -> a × 𝔹
}

type Binary a b c = {
   f :: a -> b -> c,
   g :: c -> a × b -> a × b
}

type BinarySpec a b c = {
   fwd :: a × 𝔹 -> b × 𝔹 -> c × 𝔹,
   bwd :: c × 𝔹 -> a × b -> (a × 𝔹) × (b × 𝔹)
}

unary :: forall a b . ToFrom a => ToFrom b => UnarySpec a b -> Val 𝔹
unary { fwd, bwd } = flip Primitive Nil $ PrimOp {
   arity: 1,
   op: unsafePartial apply,
   op_fwd: unsafePartial apply_fwd,
   op_bwd: unsafePartial apply_bwd
}
   where
   apply :: Partial => List (Val 𝔹) {-[a]-} -> Val 𝔹 {-b-}
   apply (v : Nil) = constr (fwd (match v))

   apply_fwd :: Partial => List (Val 𝔹 × Val 𝔹) {-[(a, a)]-} -> Val 𝔹 {-b-}
   apply_fwd (v × u : Nil) = constr (fwd (match_fwd (v × fst (match u))))

   apply_bwd :: Partial => Val 𝔹 {-b-} -> List (Val 𝔹) {-[a]-} -> List (Val 𝔹) {-[a]-}
   apply_bwd v (v1 : Nil) = match_bwd v1' : Nil
      where v1' = bwd (constr_bwd v) (fst (match v1))

binary :: forall a b c . ToFrom a => ToFrom b => ToFrom c => BinarySpec a b c -> Val 𝔹
binary { fwd, bwd } = flip Primitive Nil $ PrimOp {
   arity: 2,
   op: unsafePartial apply,
   op_fwd: unsafePartial apply_fwd,
   op_bwd: unsafePartial apply_bwd
}
   where
   apply :: Partial => List (Val 𝔹) {-[a, b]-} -> Val 𝔹 {-c-}
   apply (v : v' : Nil) = constr (fwd (match v) (match v'))

   apply_fwd :: Partial => List (Val 𝔹 × Val 𝔹) {-[(a, a), (b, b)]-} -> Val 𝔹 {-c-}
   apply_fwd (v1 × u1 : v2 × u2 : Nil) = constr (fwd (match_fwd (v1 × fst (match u1))) (match_fwd (v2 × fst (match u2))))

   apply_bwd :: Partial => Val 𝔹 {-c-} -> List (Val 𝔹) {-[a, b]-} -> List (Val 𝔹) {-[a, b]-}
   apply_bwd v (v1 : v2 : Nil) = match_bwd v1' : match_bwd v2' : Nil
      where v1' × v2' = bwd (constr_bwd v) (fst (match v1) × fst (match v2))

depends :: forall a b . (a -> b) -> UnarySpec a b
depends f = depends2 { f, g: const identity }

depends2 :: forall a b . Unary a b -> UnarySpec a b
depends2 { f, g } = { fwd: f', bwd: g' }
   where
   f' (x × α)    = f x × α
   g' (y × α) x  = g y x × α

dependsBoth :: forall a b c . (a -> b -> c) -> BinarySpec a b c
dependsBoth f = dependsBoth2 { f, g: const identity }

dependsBoth2 :: forall a b c . Binary a b c -> BinarySpec a b c
dependsBoth2 { f, g } = { fwd: f', bwd: g' }
   where
   f' (x × α) (y × β) = f x y × (α ∧ β)
   g' (z × α) (x × y) = (x' × α) × (y' × α) where x' × y' = g z (x × y)

-- If both are zero, depend only on the first.
dependsZero :: forall a b . IsZero a => (a -> a -> b) -> BinarySpec a a b
dependsZero op = { fwd, bwd }
   where
   fwd :: a × 𝔹 -> a × 𝔹 -> b × 𝔹
   fwd (x × α) (y × β)
      | isZero x  = x `op` y × α
      | isZero y  = x `op` y × β
      | otherwise = x `op` y × (α ∧ β)
   bwd :: b × 𝔹 -> a × a -> (a × 𝔹) × (a × 𝔹)
   bwd (_ × α) (x × y)
      | isZero x  = (x × α) × (y × false)
      | isZero y  = (x × false) × (y × α)
      | otherwise = (x × α) × (y × α)

class As a b where
   as :: a -> b

union1 :: forall a1 b . (a1 -> b) -> (Number -> b) -> a1 + Number -> b
union1 f _ (Left x)   = f x
union1 _ g (Right x)  = g x

-- Biased towards g: if arguments are of mixed types, we try to coerce to an application of g.
union :: forall a1 b1 c1 a2 b2 c2 c . As c1 c => As c2 c => As a1 a2 => As b1 b2 =>
         (a1 -> b1 -> c1) -> (a2 -> b2 -> c2) -> a1 + a2 -> b1 + b2 -> c
union f _ (Left x) (Left y)     = as (f x y)
union _ g (Left x) (Right y)    = as (g (as x) y)
union _ g (Right x) (Right y)   = as (g x y)
union _ g (Right x) (Left y)    = as (g x (as y))

-- Helper to avoid some explicit type annotations when defining primitives.
unionStr :: forall a b . As a a => As b String => (b -> b -> a) -> (String -> String -> a) -> b + String -> b + String -> a
unionStr = union

instance asIntIntOrNumber :: As Int (Int + Number) where
   as = Left

instance asNumberIntOrNumber :: As Number (Int + Number) where
   as = Right

instance asIntNumber :: As Int Number where
   as = toNumber

instance asBooleanBoolean :: As Boolean Boolean where
   as = identity

instance asIntOrNumberString :: As (Int + Number) String where
   as _ = error "Non-uniform argument types"
