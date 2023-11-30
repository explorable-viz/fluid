module Primitive where

import Prelude hiding (absurd, apply, div, top)
import Bindings (Bind)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Profunctor.Choice ((|||))
import Data.Set.NonEmpty (singleton, insert)
import DataType (cFalse, cPair, cTrue)
import Dict (Dict)
import Graph.WithGraph (new)
import Lattice (class BoundedJoinSemilattice, Raw, (∧), bot, erase)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Util (type (+), type (×), (×), error)
import Val (BaseVal(..), ForeignOp(..), ForeignOp'(..), Fun(..), MatrixRep, OpBwd, OpFwd, OpGraph, Val(..))

-- Mediate between values of annotation type a and (potential) underlying datatype d, analogous to
-- pattern-matching and construction for data types. Wasn't able to make a typeclass version of this
-- work with the required higher-rank polymorphism.
type ToFrom d a =
   { pack :: d × a -> Val a
   , unpack :: Val a -> d × a
   }

type ToFrom2 d a =
   { pack :: d -> BaseVal a
   , unpack :: BaseVal a -> d
   }

unpack2 :: forall d a. ToFrom2 d a -> Val a -> d × a
unpack2 toFrom (Val α v) = toFrom.unpack v × α

pack2 :: forall d a. ToFrom2 d a -> d × a -> Val a
pack2 toFrom (v × α) = Val α (toFrom.pack v)

typeError :: forall a b. Val a -> String -> b
typeError v typeName = error (typeName <> " expected; got " <> prettyP (erase v))

typeError2 :: forall a b. BaseVal a -> String -> b
typeError2 v typeName = error (typeName <> " expected; got " <> prettyP (erase v))

int2 :: forall a. ToFrom2 Int a
int2 =
   { pack: Int
   , unpack: case _ of
        Int n -> n
        v -> typeError2 v "Int"
   }

number2 :: forall a. ToFrom2 Number a
number2 =
   { pack: Float
   , unpack: case _ of
        Float n -> n
        v -> typeError2 v "Float"
   }

string2 :: forall a. ToFrom2 String a
string2 =
   { pack: Str
   , unpack: case _ of
        Str str -> str
        v -> typeError2 v "Str"
   }

intOrNumber :: forall a. ToFrom (Int + Number) a
intOrNumber =
   { pack: case _ of
        Left n × α -> Val α (Int n)
        Right n × α -> Val α (Float n)
   , unpack
   }
   where
   unpack (Val α (Int n)) = Left n × α
   unpack (Val α (Float n)) = Right n × α
   unpack v = typeError v "Int or Float"

intOrNumber2 :: forall a. ToFrom2 (Int + Number) a
intOrNumber2 =
   { pack: case _ of
        Left n -> Int n
        Right n -> Float n
   , unpack: case _ of
        Int n -> Left n
        Float n -> Right n
        v -> typeError2 v "Int or Float"
   }

intOrNumberOrString :: forall a. ToFrom (Int + Number + String) a
intOrNumberOrString =
   { pack: case _ of
        Left n × α -> Val α (Int n)
        Right (Left n) × α -> Val α (Float n)
        Right (Right str) × α -> Val α (Str str)
   , unpack
   }
   where
   unpack (Val α (Int n)) = Left n × α
   unpack (Val α (Float n)) = Right (Left n) × α
   unpack (Val α (Str str)) = Right (Right str) × α
   unpack v = typeError v "Int, Float or Str"

intOrNumberOrString2 :: forall a. ToFrom2 (Int + Number + String) a
intOrNumberOrString2 =
   { pack: case _ of
        Left n -> Int n
        Right (Left n) -> Float n
        Right (Right str) -> Str str
   , unpack: case _ of
        Int n -> Left n
        Float n -> Right (Left n)
        Str str -> Right (Right str)
        v -> typeError2 v "Int, Float or Str"
   }

intPair :: forall a. ToFrom ((Int × a) × (Int × a)) a
intPair =
   { pack: \((nβ × mβ') × α) -> Val α (Constr cPair (pack2 int2 nβ : pack2 int2 mβ' : Nil))
   , unpack
   }
   where
   unpack (Val α (Constr c (v : v' : Nil))) | c == cPair = (unpack2 int2 v × unpack2 int2 v') × α
   unpack v = typeError v "Pair"

intPair2 :: forall a. ToFrom2 ((Int × a) × (Int × a)) a
intPair2 =
   { pack: \(nβ × mβ') -> Constr cPair (pack2 int2 nβ : pack2 int2 mβ' : Nil)
   , unpack: case _ of
        Constr c (v : v' : Nil) | c == cPair -> unpack2 int2 v × unpack2 int2 v'
        v -> typeError2 v "Pair"
   }

matrixRep :: forall a. ToFrom (MatrixRep a) a
matrixRep =
   { pack: \(m × α) -> Val α (Matrix m)
   , unpack
   }
   where
   unpack (Val α (Matrix m)) = m × α
   unpack v = typeError v "Matrix"

matrixRep2 :: forall a. ToFrom2 (MatrixRep a) a
matrixRep2 =
   { pack: Matrix
   , unpack: case _ of
        Matrix m -> m
        v -> typeError2 v "Matrix"
   }

record :: forall a. ToFrom (Dict (Val a)) a
record =
   { pack: \(xvs × α) -> Val α (Record xvs)
   , unpack
   }
   where
   unpack (Val α (Record xvs)) = xvs × α
   unpack v = typeError v "Record"

record2 :: forall a. ToFrom2 (Dict (Val a)) a
record2 =
   { pack: Record
   , unpack: case _ of
        Record xvs -> xvs
        v -> typeError2 v "Record"
   }

boolean :: forall a. ToFrom Boolean a
boolean =
   { pack: case _ of
        true × α -> Val α (Constr cTrue Nil)
        false × α -> Val α (Constr cFalse Nil)
   , unpack
   }
   where
   unpack (Val α (Constr c Nil))
      | c == cTrue = true × α
      | c == cFalse = false × α
   unpack v = typeError v "Boolean"

boolean2 :: forall a. ToFrom2 Boolean a
boolean2 =
   { pack: if _ then Constr cTrue Nil else Constr cFalse Nil
   , unpack: case _ of
        Constr c Nil
           | c == cTrue -> true
           | c == cFalse -> false
        v -> typeError2 v "Boolean"
   }

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
   { i :: ToFrom2 i a
   , o :: ToFrom2 o a
   , fwd :: i -> o
   }

type Binary i1 i2 o a =
   { i1 :: ToFrom2 i1 a
   , i2 :: ToFrom2 i2 a
   , o :: ToFrom2 o a
   , fwd :: i1 -> i2 -> o
   }

type BinaryZero i o a =
   { i :: ToFrom2 i a
   , o :: ToFrom2 o a
   , fwd :: i -> i -> o
   }

unary :: forall i o a'. BoundedJoinSemilattice a' => String -> (forall a. Unary i o a) -> Bind (Val a')
unary id f =
   id × Val bot (Fun (Foreign (ForeignOp (id × op)) Nil))
   where
   op :: Exists ForeignOp'
   op = mkExists $
      ForeignOp' { arity: 1, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }

   op' :: Partial => OpGraph
   op' (Val α v : Nil) =
      pack2 f.o <$> ((f.fwd (f.i.unpack v) × _) <$> new (singleton α))

   fwd :: Partial => OpFwd (Raw BaseVal)
   fwd (Val α v : Nil) = pure $ erase v × pack2 f.o (f.fwd (f.i.unpack v) × α)

   bwd :: Partial => OpBwd (Raw BaseVal)
   bwd (u × Val α _) = pack2 f.i (f.i.unpack u × α) : Nil

binary :: forall i1 i2 o a'. BoundedJoinSemilattice a' => String -> (forall a. Binary i1 i2 o a) -> Bind (Val a')
binary id f =
   id × Val bot (Fun (Foreign (ForeignOp (id × op)) Nil))
   where
   op :: Exists ForeignOp'
   op = mkExists $
      ForeignOp' { arity: 2, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }

   op' :: Partial => OpGraph
   op' (Val α v1 : Val β v2 : Nil) =
      pack2 f.o <$> ((f.fwd (f.i1.unpack v1) (f.i2.unpack v2) × _) <$> new (singleton α # insert β))

   fwd :: Partial => OpFwd (Raw BaseVal × Raw BaseVal)
   fwd (Val α v1 : Val β v2 : Nil) =
      pure $ (erase v1 × erase v2) × pack2 f.o (f.fwd (f.i1.unpack v1) (f.i2.unpack v2) × (α ∧ β))

   bwd :: Partial => OpBwd (Raw BaseVal × Raw BaseVal)
   bwd ((u1 × u2) × Val α _) = pack2 f.i1 (f.i1.unpack u1 × α) : pack2 f.i2 (f.i2.unpack u2 × α) : Nil

-- If both are zero, depend only on the first.
binaryZero :: forall i o a'. BoundedJoinSemilattice a' => IsZero i => String -> (forall a. BinaryZero i o a) -> Bind (Val a')
binaryZero id f =
   id × Val bot (Fun (Foreign (ForeignOp (id × op)) Nil))
   where
   op :: Exists ForeignOp'
   op = mkExists $
      ForeignOp' { arity: 2, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }

   op' :: Partial => OpGraph
   op' (Val α v1 : Val β v2 : Nil) =
      pack2 f.o <$> ((f.fwd x y × _) <$> new αs)
      where
      x × y = f.i.unpack v1 × f.i.unpack v2
      αs =
         if isZero x then singleton α
         else if isZero y then singleton β
         else singleton α # insert β

   fwd :: Partial => OpFwd (Raw BaseVal × Raw BaseVal)
   fwd (Val α v1 : Val β v2 : Nil) =
      pure $ (erase v1 × erase v2) ×
         pack2 f.o (f.fwd x y × if isZero x then α else if isZero y then β else α ∧ β)
      where
      x × y = f.i.unpack v1 × f.i.unpack v2

   bwd :: Partial => OpBwd (Raw BaseVal × Raw BaseVal)
   bwd ((u1 × u2) × Val α _) = pack2 f.i (x × β1) : pack2 f.i (y × β2) : Nil
      where
      x × y = f.i.unpack u1 × f.i.unpack u2
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