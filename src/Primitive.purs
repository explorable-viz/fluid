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
import Val (class Highlightable, MatrixRep, OpBwd, OpFwd, PrimOp(..), Val(..))

-- Mediates between values of annotation type a and (potential) underlying datatype d, analogous to
-- pattern-matching and construction for data types. Wasn't able to make a typeclass version of this
-- work with the required higher-rank polymorphism.
type ToFrom d a =
   { constr :: Highlightable a => d × a -> Val a
   , constr_bwd :: Highlightable a => BoundedLattice a => Val a -> d × a -- equivalent to match (except at Val)
   , match :: Highlightable a => BoundedMeetSemilattice a => Val a -> d × a
   }

-- Analogous to "variable" case in pattern-matching (or "use existing subvalue" case in construction).
val :: forall a. ToFrom (Val a) a
val =
   { constr: fst -- construction rights not required
   , constr_bwd: (_ × bot) -- return unit of disjunction rather than conjunction
   , match: (_ × top) -- construction rights always provided
   }

int :: forall a. ToFrom Int a
int =
   { constr: \(n × α) -> Int α n
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => _
   match' (Int α n) = n × α
   match' v = error ("Int expected; got " <> prettyP v)

number :: forall a. ToFrom Number a
number =
   { constr: \(n × α) -> Float α n
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => _
   match' (Float α n) = n × α
   match' v = error ("Float expected; got " <> prettyP v)

string :: forall a. ToFrom String a
string =
   { constr: \(str × α) -> Str α str
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => _
   match' (Str α str) = str × α
   match' v = error ("Str expected; got " <> prettyP v)

intOrNumber :: forall a. ToFrom (Int + Number) a
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

intOrNumberOrString :: forall a. ToFrom (Int + Number + String) a
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

intPair :: forall a. ToFrom ((Int × a) × (Int × a)) a
intPair =
   { constr: \(nβ × mβ' × α) -> Constr α cPair (int.constr nβ : int.constr mβ' : Nil)
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => BoundedMeetSemilattice a => Val a -> ((Int × a) × (Int × a)) × a
   match' (Constr α c (v : v' : Nil)) | c == cPair = int.match v × int.match v' × α
   match' v = error ("Pair expected; got " <> prettyP v)

matrixRep :: forall a. ToFrom (Array (Array (Val a)) × (Int × a) × (Int × a)) a
matrixRep =
   { constr: \(r × α) -> Matrix α r
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => Val a -> MatrixRep a × a
   match' (Matrix α r) = r × α
   match' v = error ("Matrix expected; got " <> prettyP v)

record :: forall a. ToFrom (Dict (Val a)) a
record =
   { constr: \(xvs × α) -> Record α xvs
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => _
   match' (Record α xvs) = xvs × α
   match' v = error ("Record expected; got " <> prettyP v)

dict :: forall a. ToFrom (Dict (a × Val a)) a
dict =
   { constr: \(svs × α) -> Dictionary α svs
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Highlightable a => _
   match' (Dictionary α svs) = svs × α
   match' v = error ("Dictionary expected; got " <> prettyP v)

boolean :: forall a. ToFrom Boolean a
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

type Unary i o =
   { fwd :: i -> o
   , bwd :: o -> Endo i
   }

type UnarySlicer i o a =
   { i :: ToFrom i a
   , o :: ToFrom o a
   , fwd :: BoundedMeetSemilattice a => i × a -> o × a
   , bwd :: BoundedJoinSemilattice a => o × a -> i -> i × a
   }

type Binary i1 i2 o =
   { fwd :: i1 -> i2 -> o
   , bwd :: o -> Endo (i1 × i2)
   }

type BinarySlicer i1 i2 o a =
   { i1 :: ToFrom i1 a
   , i2 :: ToFrom i2 a
   , o :: ToFrom o a
   , fwd :: BoundedMeetSemilattice a => i1 × a -> i2 × a -> o × a
   , bwd :: BoundedJoinSemilattice a => o × a -> i1 × i2 -> (i1 × a) × (i2 × a)
   }

unary_ :: forall i o a'. (forall a. UnarySlicer i o a) -> Val a'
unary_ s =
   flip Primitive Nil $ PrimOp { arity: 1, op: unsafePartial op, op_bwd: unsafePartial op_bwd }
   where
   op :: Partial => OpFwd
   op (v : Nil) = s.o.constr (s.fwd (s.i.match v))

   op_bwd :: Partial => OpBwd
   op_bwd v (u : Nil) = s.i.constr (s.bwd (s.o.constr_bwd v) (fst (s.i.match u))) : Nil

binary_ :: forall i1 i2 o a'. (forall a. BinarySlicer i1 i2 o a) -> Val a'
binary_ s =
   flip Primitive Nil $ PrimOp { arity: 2, op: unsafePartial op, op_bwd: unsafePartial op_bwd }
   where
   op :: Partial => OpFwd
   op (v1 : v2 : Nil) = s.o.constr (s.fwd (s.i1.match v1) (s.i2.match v2))

   op_bwd :: Partial => OpBwd
   op_bwd v (u1 : u2 : Nil) = s.i1.constr v1 : s.i2.constr v2 : Nil
      where
      v1 × v2 = s.bwd (s.o.constr_bwd v) (fst (s.i1.match u1) × fst (s.i2.match u2))

withInverse1 :: forall i o. (i -> o) -> Unary i o
withInverse1 fwd = { fwd, bwd: const identity }

withInverse2 :: forall i1 i2 o. (i1 -> i2 -> o) -> Binary i1 i2 o
withInverse2 fwd = { fwd, bwd: const identity }

unary :: forall i o a'. (forall a. ToFrom i a × ToFrom o a × Unary i o) -> Val a'
unary (i × o × { fwd, bwd }) = unary_ { i, o, fwd: fwd', bwd: bwd' }
   where
   fwd' :: forall a. i × a -> o × a
   fwd' (x × α) = fwd x × α

   bwd' :: forall a. o × a -> i -> i × a
   bwd' (y × α) x = bwd y x × α

binary :: forall i1 i2 o a'. (forall a. ToFrom i1 a × ToFrom i2 a × ToFrom o a × Binary i1 i2 o) -> Val a'
binary (i1 × i2 × o × { fwd, bwd }) = binary_ { i1, i2, o, fwd: fwd', bwd: bwd' }
   where
   fwd' :: forall a. MeetSemilattice a => i1 × a -> i2 × a -> o × a
   fwd' (x × α) (y × β) = fwd x y × (α ∧ β)

   bwd' :: forall a. o × a -> i1 × i2 -> (i1 × a) × (i2 × a)
   bwd' (z × α) (x × y) = (x' × α) × (y' × α)
      where
      x' × y' = bwd z (x × y)

-- If both are zero, depend only on the first.
binaryZero :: forall i o a'. IsZero i => (forall a. ToFrom i a × ToFrom o a × Binary i i o) -> Val a'
binaryZero (i × o × { fwd, bwd }) = binary_ { i1: i, i2: i, o, fwd: fwd', bwd: bwd' }
   where
   fwd' :: forall a. MeetSemilattice a => i × a -> i × a -> o × a
   fwd' (x × α) (y × β) =
      fwd x y × if isZero x then α else if isZero y then β else α ∧ β

   bwd' :: forall a. BoundedJoinSemilattice a => o × a -> i × i -> (i × a) × (i × a)
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
