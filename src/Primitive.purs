module Primitive where

import Prelude hiding (absurd, apply, div, top)

import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Profunctor.Choice ((|||))
import Data.Tuple (fst)
import DataType (cFalse, cPair, cTrue)
import Dict (Dict)
import Lattice (class BoundedJoinSemilattice, class BoundedLattice, class BoundedMeetSemilattice, class MeetSemilattice, (∧), bot, top)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Util (Endo, type (×), (×), type (+), error)
import Val (class Highlightable, MatrixRep, PrimOp(..), Val(..))

-- Mediates between values of annotation type a and (potential) underlying datatype d, analogous to
-- pattern-matching and construction for data types.
class ToFrom d a where
   constr :: d × a -> Val a
   constr_bwd :: Val a -> d × a -- equivalent to match (except at Val)
   match :: Val a -> d × a

type ToFrom2 d a =
   { constr :: Highlightable a => d × a -> Val a
   , constr_bwd :: Highlightable a => BoundedLattice a => Val a -> d × a -- equivalent to match (except at Val)
   , match :: Highlightable a => BoundedMeetSemilattice a => Val a -> d × a
   }

-- Analogous to "variable" case in pattern-matching (or "use existing subvalue" case in construction).
val :: forall a. ToFrom2 (Val a) a
val =
   { constr: fst -- construction rights not required
   , constr_bwd: (_ × bot) -- return unit of disjunction rather than conjunction
   , match: (_ × top) -- construction rights always provided
   }

int :: forall a. ToFrom2 Int a
int =
   { constr: \(n × α) -> Int α n
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => _
   match' (Int α n) = n × α
   match' v = error ("Int expected; got " <> prettyP v)

number :: forall a. ToFrom2 Number a
number =
   { constr: \(n × α) -> Float α n
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => _
   match' (Float α n) = n × α
   match' v = error ("Float expected; got " <> prettyP v)

string :: forall a. ToFrom2 String a
string =
   { constr: \(str × α) -> Str α str
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => _
   match' (Str α str) = str × α
   match' v = error ("Str expected; got " <> prettyP v)

intOrNumber :: forall a. ToFrom2 (Int + Number) a
intOrNumber =
   { constr: case _ of
      Left n × α -> Int α n
      Right n × α -> Float α n
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => Val a -> (Int + Number) × a
   match' (Int α n) = Left n × α
   match' (Float α n) = Right n × α
   match' v = error ("Int or Float expected; got " <> prettyP v)

intOrNumberOrString :: forall a. ToFrom2 (Int + Number + String) a
intOrNumberOrString =
   { constr: case _ of
      Left (Left n) × α -> Int α n
      Left (Right n) × α -> Float α n
      Right str × α -> Str α str
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => Val a -> (Int + Number + String) × a
   match' (Int α n) = Left (Left n) × α
   match' (Float α n) = Left (Right n) × α
   match' (Str α str) = Right str × α
   match' v = error ("Int, Float or Str expected; got " <> prettyP v)

intPair :: forall a. ToFrom2 ((Int × a) × (Int × a)) a
intPair =
   { constr: \(nβ × mβ' × α) -> Constr α cPair (int.constr nβ : int.constr mβ' : Nil)
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => BoundedMeetSemilattice a => Val a -> ((Int × a) × (Int × a)) × a
   match' (Constr α c (v : v' : Nil)) | c == cPair = int.match v × int.match v' × α
   match' v = error ("Pair expected; got " <> prettyP v)

matrixRep :: forall a. ToFrom2 (Array (Array (Val a)) × (Int × a) × (Int × a)) a
matrixRep =
   { constr: \(r × α) -> Matrix α r
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => Val a -> MatrixRep a × a
   match' (Matrix α r) = r × α
   match' v = error ("Matrix expected; got " <> prettyP v)

record :: forall a. ToFrom2 (Dict (Val a)) a
record =
   { constr: \(xvs × α) -> Record α xvs
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => _
   match' (Record α xvs) = xvs × α
   match' v = error ("Record expected; got " <> prettyP v)

boolean :: forall a. ToFrom2 Boolean a
boolean =
   { constr: case _ of
      true × α -> Constr α cTrue Nil
      false × α -> Constr α cFalse Nil
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => Val a -> Boolean × a
   match' (Constr α c Nil)
      | c == cTrue = true × α
      | c == cFalse = false × α
   match' v = error ("Boolean expected; got " <> prettyP v)

class IsZero a where
   isZero :: a -> Boolean

instance IsZero Int where
   isZero = ((==) 0)

instance IsZero Number where
   isZero = ((==) 0.0)

instance (IsZero a, IsZero b) => IsZero (a + b) where
   isZero = isZero ||| isZero

type Unary d1 d2 =
   { fwd :: d1 -> d2
   , bwd :: d2 -> Endo d1
   }

type UnarySlicer d1 d2 a =
   { d1 :: ToFrom2 d1 a
   , d2 :: ToFrom2 d2 a
   , fwd :: BoundedMeetSemilattice a => d1 × a -> d2 × a
   , bwd :: BoundedJoinSemilattice a => d2 × a -> d1 -> d1 × a
   }

type Binary d1 d2 d3 =
   { fwd :: d1 -> d2 -> d3
   , bwd :: d3 -> Endo (d1 × d2)
   }

type BinarySlicer d1 d2 d3 a =
   { d1 :: ToFrom2 d1 a
   , d2 :: ToFrom2 d2 a
   , d3 :: ToFrom2 d3 a
   , fwd :: BoundedMeetSemilattice a => d1 × a -> d2 × a -> d3 × a
   , bwd :: BoundedJoinSemilattice a => d3 × a -> d1 × d2 -> (d1 × a) × (d2 × a)
   }

unary_ :: forall d1 d2 a. UnarySlicer d1 d2 a -> Val a
unary_ s = flip Primitive Nil $ PrimOp
   { arity: 1
   , op: unsafePartial (apply1 s)
   , op_bwd: unsafePartial (apply1_bwd s)
   }

apply1 :: forall d1 d2 a. Partial => Highlightable a => BoundedMeetSemilattice a => UnarySlicer d1 d2 a -> List (Val a) {-[d1]-} -> Val a {-d2-}
apply1 s (v : Nil) = s.d2.constr (s.fwd (s.d1.match v))

apply1_bwd :: forall d1 d2 a. Partial => Highlightable a => BoundedLattice a => UnarySlicer d1 d2 a -> Val a {-(d2, d2)-} -> List (Val a) {-[d1]-} -> List (Val a) {-[d1]-}
apply1_bwd s v (u1 : Nil) = s.d1.constr (s.bwd (s.d2.constr_bwd v) (fst (s.d1.match u1))) : Nil

binary_ :: forall d1 d2 d3 a. BinarySlicer d1 d2 d3 a -> Val a
binary_ s = flip Primitive Nil $ PrimOp
   { arity: 2
   , op: unsafePartial (apply2 s)
   , op_bwd: unsafePartial (apply2_bwd s)
   }

apply2 :: forall d1 d2 d3 a. Partial => Highlightable a => BoundedMeetSemilattice a => BinarySlicer d1 d2 d3 a -> List (Val a) {-[d1, d2]-} -> Val a {-d3-}
apply2 s (v : v' : Nil) = s.d3.constr (s.fwd (s.d1.match v) (s.d2.match v'))

apply2_bwd :: forall d1 d2 d3 a. Partial => Highlightable a => BoundedLattice a => BinarySlicer d1 d2 d3 a -> Val a {-(d3, d3)-} -> List (Val a) {-[d1, d2]-} -> List (Val a) {-[d1, d2]-}
apply2_bwd s v (u1 : u2 : Nil) = s.d1.constr v1 : s.d2.constr v2 : Nil
   where
   v1 × v2 = s.bwd (s.d3.constr_bwd v) (fst (s.d1.match u1) × fst (s.d2.match u2))

withInverse1 :: forall d1 d2. (d1 -> d2) -> Unary d1 d2
withInverse1 fwd = { fwd, bwd: const identity }

withInverse2 :: forall d1 d2 d3. (d1 -> d2 -> d3) -> Binary d1 d2 d3
withInverse2 fwd = { fwd, bwd: const identity }

unary :: forall d1 d2 a'. (forall a. ToFrom2 d1 a × ToFrom2 d2 a × Unary d1 d2) -> Val a'
unary (d1 × d2 × { fwd, bwd }) = unary_ { d1, d2, fwd: fwd', bwd: bwd' }
   where
   fwd' :: forall a. d1 × a -> d2 × a
   fwd' (x × α) = fwd x × α

   bwd' :: forall a. d2 × a -> d1 -> d1 × a
   bwd' (y × α) x = bwd y x × α

binary :: forall d1 d2 d3 a'. (forall a. ToFrom2 d1 a × ToFrom2 d2 a × ToFrom2 d3 a × Binary d1 d2 d3) -> Val a'
binary (d1 × d2 × d3 × { fwd, bwd }) = binary_ { d1, d2, d3, fwd: fwd', bwd: bwd' }
   where
   fwd' :: forall a. MeetSemilattice a => d1 × a -> d2 × a -> d3 × a
   fwd' (x × α) (y × β) = fwd x y × (α ∧ β)

   bwd' :: forall a. d3 × a -> d1 × d2 -> (d1 × a) × (d2 × a)
   bwd' (z × α) (x × y) = (x' × α) × (y' × α)
      where
      x' × y' = bwd z (x × y)

-- If both are zero, depend only on the first.
binaryZero :: forall d1 d2 a'. IsZero d1 => (forall a. ToFrom2 d1 a × ToFrom2 d2 a × Binary d1 d1 d2) -> Val a'
binaryZero (d1 × d2 × { fwd, bwd }) = binary_ { d1, d2: d1, d3: d2, fwd: fwd', bwd: bwd' }
   where
   fwd' :: forall a. MeetSemilattice a => d1 × a -> d1 × a -> d2 × a
   fwd' (x × α) (y × β) =
      fwd x y × if isZero x then α else if isZero y then β else α ∧ β

   bwd' :: forall a. BoundedJoinSemilattice a => d2 × a -> d1 × d1 -> (d1 × a) × (d1 × a)
   bwd' (z × α) (x × y) =
      if isZero x then (x' × α) × (y' × bot)
      else if isZero y then (x' × bot) × (y' × α)
      else
         (x' × α) × (y' × α)
      where
      x' × y' = bwd z (x × y)

class As a b where
   as :: a -> b

union1 :: forall a1 b. (a1 -> b) -> (Number -> b) -> a1 + Number -> b
union1 f _ (Left x) = f x
union1 _ g (Right x) = g x

-- Biased towards g: if arguments are of mixed types, we try to coerce to an application of g.
union
   :: forall a1 b1 c1 a2 b2 c2 c
    . As c1 c
   => As c2 c
   => As a1 a2
   => As b1 b2
   => (a1 -> b1 -> c1)
   -> (a2 -> b2 -> c2)
   -> a1 + a2
   -> b1 + b2
   -> c
union f _ (Left x) (Left y) = as (f x y)
union _ g (Left x) (Right y) = as (g (as x) y)
union _ g (Right x) (Right y) = as (g x y)
union _ g (Right x) (Left y) = as (g x (as y))

-- Helper to avoid some explicit type annotations when defining primitives.
unionStr :: forall a b. As a a => As b String => (b -> b -> a) -> (String -> String -> a) -> b + String -> b + String -> a
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

instance asEither :: As (Int + Number) Number where
   as (Left n) = as n
   as (Right n) = n
