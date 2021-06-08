module Primitive where

import Partial.Unsafe (unsafePartial)
import Prelude hiding (absurd, apply, div)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Profunctor.Choice ((|||))
import Data.Tuple (fst)
import Bindings (Bind)
import DataType (cFalse, cPair, cTrue)
import Lattice (𝔹, (∧), expand)
import Pretty (prettyP)
import Util (Endo, type (×), (×), type (+), error)
import Util.SnocList (SnocList)
import Val (PrimOp(..), Val(..))

-- Mediates between Val and underlying data, analously to pattern-matching and construction for data types.
class ToFrom a where
   constr :: a × 𝔹 -> Val 𝔹
   constr_bwd :: Val 𝔹 × Val 𝔹 -> a × 𝔹   -- equivalent to match_fwd (except at Val)
   match :: Val 𝔹 -> a × 𝔹                -- only defined for non-holes (except at Val)

unwrap :: forall a . ToFrom a => Val 𝔹 -> a
unwrap = match >>> fst

match_fwd :: forall a . ToFrom a => Val 𝔹 × Val 𝔹 -> a × 𝔹
match_fwd (v × v') = match (expand v v')

match_bwd :: forall a . ToFrom a => a × 𝔹 -> Val 𝔹
match_bwd = constr

-- Analogous to "variable" case in pattern-matching (or "use existing subvalue" case in construction).
instance toFromVal :: ToFrom (Val Boolean) where
   constr = fst                        -- construction rights not required
   constr_bwd (v × _) = (v × false)    -- return unit of disjunction rather than conjunction
   match = (_ × true)                  -- construction rights are always provided

instance toFromInt :: ToFrom Int where
   match (Int α n)   = n × α
   match v           = error ("Int expected; got " <> prettyP v)

   constr (n × α) = Int α n
   constr_bwd v = match_fwd v

instance toFromNumber :: ToFrom Number where
   match (Float α n) = n × α
   match v           = error ("Float expected; got " <> prettyP v)

   constr (n × α) = Float α n
   constr_bwd v = match_fwd v

instance toFromString :: ToFrom String where
   match (Str α str) = str × α
   match v           = error ("Str expected; got " <> prettyP v)

   constr (str × α) = Str α str
   constr_bwd v = match_fwd v

instance toFromIntOrNumber :: ToFrom (Int + Number) where
   constr (Left n × α)   = Int α n
   constr (Right n × α)  = Float α n

   constr_bwd v = match_fwd v

   match (Int α n)    = Left n × α
   match (Float α n)  = Right n × α
   match v            = error ("Int or Float expected; got " <> prettyP v)

instance toFromIntOrNumberOrString :: ToFrom (Either (Either Int Number) String) where
   constr (Left (Left n) × α)  = Int α n
   constr (Left (Right n) × α) = Float α n
   constr (Right str × α)      = Str α str

   constr_bwd v = match_fwd v

   match (Int α n)   = Left (Left n) × α
   match (Float α n) = Left (Right n) × α
   match (Str α str) = Right str × α
   match v           = error ("Int, Float or Str expected; got " <> prettyP v)

instance toFromIntAndInt :: ToFrom ((Int × Boolean) × (Int × Boolean)) where
   constr (nβ × mβ' × α) = Constr α cPair (constr nβ : constr mβ' : Nil)
   constr_bwd v = match_fwd v

   match (Constr α c (v : v' : Nil)) | c == cPair  = match v × match v' × α
   match v                                         = error ("Pair expected; got " <> prettyP v)

instance toFromMatrixRep :: ToFrom (Array (Array (Val Boolean)) × (Int × Boolean) × (Int × Boolean)) where
   match (Matrix α r) = r × α
   match v            = error ("Matrix expected; got " <> prettyP v)

   constr (r × α) = Matrix α r
   constr_bwd v = match_fwd v

instance toFromRecordRep :: ToFrom (SnocList (Bind (Val Boolean))) where
   match (Record α xvs) = xvs × α
   match v              = error ("Record expected; got " <> prettyP v)

   constr (xvs × α) = Record α xvs
   constr_bwd v = match_fwd v

instance toFromValAndVal :: ToFrom (Val Boolean × Val Boolean) where
   constr (v × v' × α) = Constr α cPair (v : v' : Nil)
   constr_bwd v = match_fwd v

   match (Constr α c (v : v' : Nil)) | c == cPair   = v × v' × α
   match v                                          = error ("Pair expected; got " <> prettyP v)

instance toFromBoolean :: ToFrom Boolean where
   match (Constr α c Nil)
      | c == cTrue   = true × α
      | c == cFalse  = false × α
   match v = error ("Boolean expected; got " <> prettyP v)

   constr (true × α)   = Constr α cTrue Nil
   constr (false × α)  = Constr α cFalse Nil

   constr_bwd v = match_fwd v

class IsZero a where
   isZero :: a -> Boolean

instance isZeroInt :: IsZero Int where
   isZero = ((==) 0)

instance isZeroNumber :: IsZero Number where
   isZero = ((==) 0.0)

instance isZeroEither :: (IsZero a, IsZero b) => IsZero (a + b) where
   isZero = isZero ||| isZero

type Unary a b = {
   fwd :: a -> b,
   bwd :: b -> Endo a
}

type UnarySlicer a b = {
   fwd :: a × 𝔹 -> b × 𝔹,
   bwd :: b × 𝔹 -> a -> a × 𝔹
}

type Binary a b c = {
   fwd :: a -> b -> c,
   bwd :: c -> Endo (a × b)
}

type BinarySlicer a b c = {
   fwd :: a × 𝔹 -> b × 𝔹 -> c × 𝔹,
   bwd :: c × 𝔹 -> a × b -> (a × 𝔹) × (b × 𝔹)
}

unary_ :: forall a b . ToFrom a => ToFrom b => UnarySlicer a b -> Val 𝔹
unary_ { fwd, bwd } = flip Primitive Nil $ PrimOp {
   arity: 1,
   op: unsafePartial apply,
   op_fwd: unsafePartial apply_fwd,
   op_bwd: unsafePartial apply_bwd
}
   where
   apply :: Partial => List (Val 𝔹) {-[a]-} -> Val 𝔹 {-b-}
   apply (v : Nil) = constr (fwd (match v))

   apply_fwd :: Partial => List (Val 𝔹 × Val 𝔹) {-[(a, a)]-} -> Val 𝔹 {-b-}
   apply_fwd (v × u : Nil) = constr (fwd (match_fwd (v × u)))

   apply_bwd :: Partial => Val 𝔹 × Val 𝔹 {-(b, b)-} -> List (Val 𝔹) {-[a]-} -> List (Val 𝔹) {-[a]-}
   apply_bwd (v × u) (u1 : Nil) = match_bwd v1 : Nil
      where v1 = bwd (constr_bwd (v × u)) (unwrap u1)

binary_ :: forall a b c . ToFrom a => ToFrom b => ToFrom c => BinarySlicer a b c -> Val 𝔹
binary_ { fwd, bwd } = flip Primitive Nil $ PrimOp {
   arity: 2,
   op: unsafePartial apply,
   op_fwd: unsafePartial apply_fwd,
   op_bwd: unsafePartial apply_bwd
}
   where
   apply :: Partial => List (Val 𝔹) {-[a, b]-} -> Val 𝔹 {-c-}
   apply (v : v' : Nil) = constr (fwd (match v) (match v'))

   apply_fwd :: Partial => List (Val 𝔹 × Val 𝔹) {-[(a, a), (b, b)]-} -> Val 𝔹 {-c-}
   apply_fwd (v1 × u1 : v2 × u2 : Nil) = constr (fwd (match_fwd (v1 × u1)) (match_fwd (v2 × u2)))

   apply_bwd :: Partial => Val 𝔹 × Val 𝔹 {-(c, c)-} -> List (Val 𝔹) {-[a, b]-} -> List (Val 𝔹) {-[a, b]-}
   apply_bwd (v × u) (u1 : u2 : Nil) = match_bwd v1 : match_bwd v2 : Nil
      where v1 × v2 = bwd (constr_bwd (v × u)) (unwrap u1 × unwrap u2)

withInverse1 :: forall a b . (a -> b) -> Unary a b
withInverse1 fwd = { fwd, bwd: const identity }

withInverse2 :: forall a b c . (a -> b -> c) -> Binary a b c
withInverse2 fwd = { fwd, bwd: const identity }

unary :: forall a b . ToFrom a => ToFrom b => Unary a b -> Val 𝔹
unary { fwd, bwd } = unary_ { fwd: fwd', bwd: bwd' }
   where
   fwd' (x × α)    = fwd x × α
   bwd' (y × α) x  = bwd y x × α

binary :: forall a b c . ToFrom a => ToFrom b => ToFrom c => Binary a b c -> Val 𝔹
binary { fwd, bwd } = binary_ { fwd: fwd', bwd: bwd' }
   where
   fwd' (x × α) (y × β) = fwd x y × (α ∧ β)
   bwd' (z × α) (x × y) = (x' × α) × (y' × α) where x' × y' = bwd z (x × y)

-- If both are zero, depend only on the first.
binaryZero :: forall a b . IsZero a => ToFrom a => ToFrom b => Binary a a b -> Val 𝔹
binaryZero { fwd, bwd } = binary_ { fwd: fwd', bwd: bwd' }
   where
   fwd' :: a × 𝔹 -> a × 𝔹 -> b × 𝔹
   fwd' (x × α) (y × β) =
      fwd x y × if isZero x then α else if isZero y then β else α ∧ β
   bwd' :: b × 𝔹 -> a × a -> (a × 𝔹) × (a × 𝔹)
   bwd' (z × α) (x × y) =
      if isZero x then (x' × α) × (y' × false) else if isZero y then (x' × false) × (y' × α) else (x' × α) × (y' × α)
      where x' × y' = bwd z (x × y)

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
