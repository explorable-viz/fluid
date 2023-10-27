module Primitive where

import Prelude hiding (absurd, apply, div, top)
import Bindings (Bind)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Profunctor.Choice ((|||))
import Data.Set (singleton, insert)
import DataType (cFalse, cPair, cTrue)
import Dict (Dict)
import Graph.GraphWriter (new)
import Lattice (class BoundedJoinSemilattice, Raw, (∧), bot, erase)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Util (type (+), type (×), (×), error)
import Val (class Ann, ForeignOp(..), ForeignOp'(..), Fun(..), MatrixRep, OpBwd, OpFwd, OpGraph, Val(..))

-- Mediate between values of annotation type a and (potential) underlying datatype d, analogous to
-- pattern-matching and construction for data types. Wasn't able to make a typeclass version of this
-- work with the required higher-rank polymorphism.
type ToFrom d a =
   { pack :: d × a -> Val a
   , unpack :: Val a -> d × a
   }

typeError :: forall a b. Val a -> String -> b
typeError v typeName = error (typeName <> " expected; got " <> prettyP (erase v))

int :: forall a. ToFrom Int a
int =
   { pack: \(n × α) -> Int α n
   , unpack
   }
   where
   unpack (Int α n) = n × α
   unpack v = typeError v "Int"

number :: forall a. ToFrom Number a
number =
   { pack: \(n × α) -> Float α n
   , unpack
   }
   where
   unpack (Float α n) = n × α
   unpack v = typeError v "Float"

string :: forall a. ToFrom String a
string =
   { pack: \(str × α) -> Str α str
   , unpack
   }
   where
   unpack (Str α str) = str × α
   unpack v = typeError v "Str"

intOrNumber :: forall a. ToFrom (Int + Number) a
intOrNumber =
   { pack: case _ of
        Left n × α -> Int α n
        Right n × α -> Float α n
   , unpack
   }
   where
   unpack (Int α n) = Left n × α
   unpack (Float α n) = Right n × α
   unpack v = typeError v "Int or Float"

intOrNumberOrString :: forall a. ToFrom (Int + Number + String) a
intOrNumberOrString =
   { pack: case _ of
        Left n × α -> Int α n
        Right (Left n) × α -> Float α n
        Right (Right str) × α -> Str α str
   , unpack
   }
   where
   unpack (Int α n) = Left n × α
   unpack (Float α n) = Right (Left n) × α
   unpack (Str α str) = Right (Right str) × α
   unpack v = typeError v "Int, Float or Str"

intPair :: forall a. ToFrom ((Int × a) × (Int × a)) a
intPair =
   { pack: \((nβ × mβ') × α) -> Constr α cPair (int.pack nβ : int.pack mβ' : Nil)
   , unpack
   }
   where
   unpack (Constr α c (v : v' : Nil)) | c == cPair = (int.unpack v × int.unpack v') × α
   unpack v = typeError v "Pair"

matrixRep :: forall a. Ann a => ToFrom (MatrixRep a) a
matrixRep =
   { pack: \(m × α) -> Matrix α m
   , unpack
   }
   where
   unpack (Matrix α m) = m × α
   unpack v = typeError v "Matrix"

record :: forall a. Ann a => ToFrom (Dict (Val a)) a
record =
   { pack: \(xvs × α) -> Record α xvs
   , unpack
   }
   where
   unpack (Record α xvs) = xvs × α
   unpack v = typeError v "Record"

boolean :: forall a. ToFrom Boolean a
boolean =
   { pack: case _ of
        true × α -> Constr α cTrue Nil
        false × α -> Constr α cFalse Nil
   , unpack
   }
   where
   unpack (Constr α c Nil)
      | c == cTrue = true × α
      | c == cFalse = false × α
   unpack v = typeError v "Boolean"

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

unary :: forall i o a'. BoundedJoinSemilattice a' => String -> (forall a. Unary i o a) -> Bind (Val a')
unary id f =
   id × Fun bot (Foreign (ForeignOp (id × op)) Nil)
   where
   op :: Exists ForeignOp'
   op = mkExists $
      ForeignOp' { arity: 1, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }

   op' :: Partial => OpGraph
   op' (v : Nil) =
      f.o.pack <$> ((f.fwd x × _) <$> new (singleton α))
      where
      x × α = f.i.unpack v

   fwd :: Partial => OpFwd (Raw Val)
   fwd (v : Nil) = pure $ erase v × f.o.pack (f.fwd x × α)
      where
      x × α = f.i.unpack v

   bwd :: Partial => OpBwd (Raw Val)
   bwd (u × v) = f.i.pack (x × α) : Nil
      where
      _ × α = f.o.unpack v
      (x × _) = f.i.unpack u

binary :: forall i1 i2 o a'. BoundedJoinSemilattice a' => String -> (forall a. Binary i1 i2 o a) -> Bind (Val a')
binary id f =
   id × Fun bot (Foreign (ForeignOp (id × op)) Nil)
   where
   op :: Exists ForeignOp'
   op = mkExists $
      ForeignOp' { arity: 2, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }

   op' :: Partial => OpGraph
   op' (v1 : v2 : Nil) =
      f.o.pack <$> ((f.fwd x y × _) <$> new (singleton α # insert β))
      where
      (x × α) × (y × β) = f.i1.unpack v1 × f.i2.unpack v2

   fwd :: Partial => OpFwd (Raw Val × Raw Val)
   fwd (v1 : v2 : Nil) = pure $ (erase v1 × erase v2) × f.o.pack (f.fwd x y × (α ∧ β))
      where
      (x × α) × (y × β) = f.i1.unpack v1 × f.i2.unpack v2

   bwd :: Partial => OpBwd (Raw Val × Raw Val)
   bwd ((u1 × u2) × v) = f.i1.pack (x × α) : f.i2.pack (y × α) : Nil
      where
      _ × α = f.o.unpack v
      (x × _) × (y × _) = f.i1.unpack u1 × f.i2.unpack u2

-- If both are zero, depend only on the first.
binaryZero :: forall i o a'. BoundedJoinSemilattice a' => IsZero i => String -> (forall a. BinaryZero i o a) -> Bind (Val a')
binaryZero id f =
   id × Fun bot (Foreign (ForeignOp (id × op)) Nil)
   where
   op :: Exists ForeignOp'
   op = mkExists $
      ForeignOp' { arity: 2, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }

   op' :: Partial => OpGraph
   op' (v1 : v2 : Nil) =
      let
         αs =
            if isZero x then singleton α
            else if isZero y then singleton β
            else singleton α # insert β
      in
         f.o.pack <$> ((f.fwd x y × _) <$> new αs)
      where
      (x × α) × (y × β) = f.i.unpack v1 × f.i.unpack v2

   fwd :: Partial => OpFwd (Raw Val × Raw Val)
   fwd (v1 : v2 : Nil) =
      pure $ (erase v1 × erase v2) ×
         f.o.pack (f.fwd x y × if isZero x then α else if isZero y then β else α ∧ β)
      where
      (x × α) × (y × β) = f.i.unpack v1 × f.i.unpack v2

   bwd :: Partial => OpBwd (Raw Val × Raw Val)
   bwd ((u1 × u2) × v) = f.i.pack (x × β1) : f.i.pack (y × β2) : Nil
      where
      _ × α = f.o.unpack v
      (x × _) × (y × _) = f.i.unpack u1 × f.i.unpack u2
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