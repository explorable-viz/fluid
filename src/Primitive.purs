module Primitive where

import Prelude hiding (absurd, apply, div, top)

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Exists (mkExists)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Profunctor.Choice ((|||))
import Data.Set (insert, singleton)
import DataType (cFalse, cPair, cTrue)
import Dict (Dict)
import Graph (new)
import Lattice (Raw, (∧), bot, erase)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Util (type (+), type (×), (×), error)
import Val (class Ann, ForeignOp'(..), Fun(..), MatrixRep', OpBwd, OpFwd, OpGraph, Val(..))

-- Mediate between values of annotation type a and (potential) underlying datatype d, analogous to
-- pattern-matching and construction for data types. Wasn't able to make a typeclass version of this
-- work with the required higher-rank polymorphism.
type ToFrom d a =
   { constr :: d × a -> Val a
   , constr_bwd :: Val a -> d × a -- equivalent to match (except at Val)
   , match :: Val a -> d × a
   }

int :: forall a. ToFrom Int a
int =
   { constr: \(n × α) -> Int α n
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: _
   match' (Int α n) = n × α
   match' v = error ("Int expected; got " <> prettyP (erase v))

number :: forall a. ToFrom Number a
number =
   { constr: \(n × α) -> Float α n
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: _
   match' (Float α n) = n × α
   match' v = error ("Float expected; got " <> prettyP (erase v))

string :: forall a. ToFrom String a
string =
   { constr: \(str × α) -> Str α str
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: _
   match' (Str α str) = str × α
   match' v = error ("Str expected; got " <> prettyP (erase v))

intOrNumber :: forall a. ToFrom (Int + Number) a
intOrNumber =
   { constr: case _ of
        Left n × α -> Int α n
        Right n × α -> Float α n
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Val a -> (Int + Number) × a
   match' (Int α n) = Left n × α
   match' (Float α n) = Right n × α
   match' v = error ("Int or Float expected; got " <> prettyP (erase v))

intOrNumberOrString :: forall a. ToFrom (Int + Number + String) a
intOrNumberOrString =
   { constr: case _ of
        Left n × α -> Int α n
        Right (Left n) × α -> Float α n
        Right (Right str) × α -> Str α str
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Val a -> (Int + Number + String) × a
   match' (Int α n) = Left n × α
   match' (Float α n) = Right (Left n) × α
   match' (Str α str) = Right (Right str) × α
   match' v = error ("Int, Float or Str expected; got " <> prettyP (erase v))

intPair :: forall a. ToFrom ((Int × a) × (Int × a)) a
intPair =
   { constr: \((nβ × mβ') × α) -> Constr α cPair (int.constr nβ : int.constr mβ' : Nil)
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Val a -> ((Int × a) × (Int × a)) × a
   match' (Constr α c (v : v' : Nil)) | c == cPair = (int.match v × int.match v') × α
   match' v = error ("Pair expected; got " <> prettyP (erase v))

matrixRep :: forall a. Ann a => ToFrom (MatrixRep' a) a
matrixRep =
   { constr: \(m × α) -> Matrix α m
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Ann a => Val a -> MatrixRep' a × a
   match' (Matrix α m) = m × α
   match' v = error ("Matrix expected; got " <> prettyP v)

record :: forall a. Ann a => ToFrom (Dict (Val a)) a
record =
   { constr: \(xvs × α) -> Record α xvs
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Ann a => _
   match' (Record α xvs) = xvs × α
   match' v = error ("Record expected; got " <> prettyP v)

boolean :: forall a. ToFrom Boolean a
boolean =
   { constr: case _ of
        true × α -> Constr α cTrue Nil
        false × α -> Constr α cFalse Nil
   , constr_bwd: match'
   , match: match'
   }
   where
   match' :: Val a -> Boolean × a
   match' (Constr α c Nil)
      | c == cTrue = true × α
      | c == cFalse = false × α
   match' v = error ("Boolean expected; got " <> prettyP (erase v))

class IsZero a where
   isZero :: a -> Boolean

instance IsZero Int where
   isZero = ((==) 0)

instance IsZero Number where
   isZero = ((==) 0.0)

instance (IsZero a, IsZero b) => IsZero (a + b) where
   isZero = isZero ||| isZero

-- Need to be careful about type variables escaping higher-rank quantification.
type Unary i o a =
   { i :: ToFrom i a
   , o :: ToFrom o a
   , fwd :: i -> o
   }

type Binary i1 i2 o a =
   { i1 :: ToFrom i1 a
   , i2 :: ToFrom i2 a
   , o :: ToFrom o a
   , fwd :: i1 -> i2 -> o
   }

type BinaryZero i o a =
   { i :: ToFrom i a
   , o :: ToFrom o a
   , fwd :: i -> i -> o
   }

unary :: forall i o a'. (forall a. Unary i o a) -> Val a'
unary op =
   Fun $ flip Foreign Nil
      $ mkExists
      $ ForeignOp' { arity: 1, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   op' :: Partial => OpGraph
   op' (v : Nil) =
      op.o.constr <$> ((op.fwd x × _) <$> lift (new (singleton α)))
      where
      x × α = op.i.match v

   fwd :: Partial => OpFwd (Raw Val)
   fwd (v : Nil) = pure $ erase v × op.o.constr (op.fwd x × α)
      where
      x × α = op.i.match v

   bwd :: Partial => OpBwd (Raw Val)
   bwd (u × v) = op.i.constr (x × α) : Nil
      where
      _ × α = op.o.constr_bwd v
      (x × _) = op.i.match u

binary :: forall i1 i2 o a'. (forall a. Binary i1 i2 o a) -> Val a'
binary op =
   Fun $ flip Foreign Nil
      $ mkExists
      $ ForeignOp' { arity: 2, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   op' :: Partial => OpGraph
   op' (v1 : v2 : Nil) =
      op.o.constr <$> ((op.fwd x y × _) <$> lift (new (singleton α # insert β)))
      where
      (x × α) × (y × β) = op.i1.match v1 × op.i2.match v2

   fwd :: Partial => OpFwd (Raw Val × Raw Val)
   fwd (v1 : v2 : Nil) = pure $ (erase v1 × erase v2) × op.o.constr (op.fwd x y × (α ∧ β))
      where
      (x × α) × (y × β) = op.i1.match v1 × op.i2.match v2

   bwd :: Partial => OpBwd (Raw Val × Raw Val)
   bwd ((u1 × u2) × v) = op.i1.constr (x × α) : op.i2.constr (y × α) : Nil
      where
      _ × α = op.o.constr_bwd v
      (x × _) × (y × _) = op.i1.match u1 × op.i2.match u2

-- If both are zero, depend only on the first.
binaryZero :: forall i o a'. IsZero i => (forall a. BinaryZero i o a) -> Val a'
binaryZero op =
   Fun $ flip Foreign Nil
      $ mkExists
      $ ForeignOp' { arity: 2, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }
   where
   op' :: Partial => OpGraph
   op' (v1 : v2 : Nil) =
      let
         αs =
            if isZero x then singleton α
            else if isZero y then singleton β
            else singleton α # insert β
      in
         op.o.constr <$> ((op.fwd x y × _) <$> lift (new αs))
      where
      (x × α) × (y × β) = op.i.match v1 × op.i.match v2

   fwd :: Partial => OpFwd (Raw Val × Raw Val)
   fwd (v1 : v2 : Nil) =
      pure $ (erase v1 × erase v2) ×
         op.o.constr (op.fwd x y × if isZero x then α else if isZero y then β else α ∧ β)
      where
      (x × α) × (y × β) = op.i.match v1 × op.i.match v2

   bwd :: Partial => OpBwd (Raw Val × Raw Val)
   bwd ((u1 × u2) × v) = op.i.constr (x × β1) : op.i.constr (y × β2) : Nil
      where
      _ × α = op.o.constr_bwd v
      (x × _) × (y × _) = op.i.match u1 × op.i.match u2
      β1 × β2 =
         if isZero x then α × bot
         else if isZero y then bot × α
         else α × α

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
unionStr
   :: forall a b
    . As a a
   => As b String
   => (b -> b -> a)
   -> (String -> String -> a)
   -> b + String
   -> b + String
   -> a
unionStr = union

instance asIntIntOrNumber :: As Int (Int + a) where
   as = Left

instance asNumberIntOrNumber :: As Number (a + Number) where
   as = Right

instance asIntNumber :: As Int Number where
   as = toNumber

instance asBooleanBoolean :: As Boolean Boolean where
   as = identity

instance asNumberString :: As Number String where
   as _ = error "Non-uniform argument types"

instance asIntNumberOrString :: As Int (Number + a) where
   as = toNumber >>> Left

instance asIntorNumberNumber :: As (Int + Number) Number where
   as (Left n) = as n
   as (Right n) = n