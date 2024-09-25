module Primitive where

import Prelude hiding (absurd, apply, div, top)
import Bind (Bind)
import Data.Either (Either(..))
import Data.Exists (Exists, mkExists)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Profunctor.Choice ((|||))
import Data.Set (insert)
import DataType (cFalse, cPair, cTrue)
import Dict (Dict)
import Graph.WithGraph (new)
import Lattice (class BoundedJoinSemilattice, Raw, (∧), bot, erase)
import Partial.Unsafe (unsafePartial)
import Pretty (prettyP)
import Util (type (+), type (×), (×), error, singleton)
import Val (BaseVal(..), ForeignOp(..), ForeignOp'(..), Fun(..), MatrixRep, OpBwd, OpFwd, OpGraph, Val(..))

-- Mediate between wrapped values and underlying datatype d. Wasn't able to make a typeclass version
-- work with required higher-rank polymorphism.
type ToFrom d a =
   { pack :: d -> BaseVal a
   , unpack :: BaseVal a -> d
   }

unpack :: forall d a. ToFrom d a -> Val a -> d × a
unpack toFrom (Val α v) = toFrom.unpack v × α

pack :: forall d a. ToFrom d a -> d × a -> Val a
pack toFrom (v × α) = Val α (toFrom.pack v)

typeError :: forall a b. BaseVal a -> String -> b
typeError v typeName = error (typeName <> " expected; got " <> prettyP (erase v))

int :: forall a. ToFrom Int a
int =
   { pack: Int
   , unpack: case _ of
        Int n -> n
        v -> typeError v "Int"
   }

number :: forall a. ToFrom Number a
number =
   { pack: Float
   , unpack: case _ of
        Float n -> n
        v -> typeError v "Float"
   }

string :: forall a. ToFrom String a
string =
   { pack: Str
   , unpack: case _ of
        Str str -> str
        v -> typeError v "Str"
   }

intOrNumber :: forall a. ToFrom (Int + Number) a
intOrNumber =
   { pack: case _ of
        Left n -> Int n
        Right n -> Float n
   , unpack: case _ of
        Int n -> Left n
        Float n -> Right n
        v -> typeError v "Int or Float"
   }

intOrNumberOrString :: forall a. ToFrom (Int + Number + String) a
intOrNumberOrString =
   { pack: case _ of
        Left n -> Int n
        Right (Left n) -> Float n
        Right (Right str) -> Str str
   , unpack: case _ of
        Int n -> Left n
        Float n -> Right (Left n)
        Str str -> Right (Right str)
        v -> typeError v "Int, Float or Str"
   }

intPair :: forall a. ToFrom ((Int × a) × (Int × a)) a
intPair =
   { pack: \(nβ × mβ') -> Constr cPair (pack int nβ : pack int mβ' : Nil)
   , unpack: case _ of
        Constr c (v : v' : Nil) | c == cPair -> unpack int v × unpack int v'
        v -> typeError v "Pair"
   }

matrixRep :: forall a. ToFrom (MatrixRep a) a
matrixRep =
   { pack: Matrix
   , unpack: case _ of
        Matrix m -> m
        v -> typeError v "Matrix"
   }

record :: forall a. ToFrom (Dict (Val a)) a
record =
   { pack: Record
   , unpack: case _ of
        Record xvs -> xvs
        v -> typeError v "Record"
   }

boolean :: forall a. ToFrom Boolean a
boolean =
   { pack: if _ then Constr cTrue Nil else Constr cFalse Nil
   , unpack: case _ of
        Constr c Nil
           | c == cTrue -> true
           | c == cFalse -> false
        v -> typeError v "Boolean"
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
   id × Val bot (Fun (Foreign (ForeignOp (id × op)) Nil))
   where
   op :: Exists ForeignOp'
   op = mkExists $
      ForeignOp' { arity: 1, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }

   op' :: Partial => OpGraph
   op' (Val α v : Nil) =
      pack f.o <$> ((f.fwd (f.i.unpack v) × _) <$> new (singleton α))

   fwd :: Partial => OpFwd (Raw BaseVal)
   fwd (Val α v : Nil) = pure $ erase v × pack f.o (f.fwd (f.i.unpack v) × α)

   bwd :: Partial => OpBwd (Raw BaseVal)
   bwd (u × Val α _) = pack f.i (f.i.unpack u × α) : Nil

binary :: forall i1 i2 o a'. BoundedJoinSemilattice a' => String -> (forall a. Binary i1 i2 o a) -> Bind (Val a')
binary id f =
   id × Val bot (Fun (Foreign (ForeignOp (id × op)) Nil))
   where
   op :: Exists ForeignOp'
   op = mkExists $
      ForeignOp' { arity: 2, op': unsafePartial op', op: unsafePartial fwd, op_bwd: unsafePartial bwd }

   op' :: Partial => OpGraph
   op' (Val α v1 : Val β v2 : Nil) =
      pack f.o <$> ((f.fwd (f.i1.unpack v1) (f.i2.unpack v2) × _) <$> new (singleton α # insert β))

   fwd :: Partial => OpFwd (Raw BaseVal × Raw BaseVal)
   fwd (Val α v1 : Val β v2 : Nil) =
      pure $ (erase v1 × erase v2) × pack f.o (f.fwd (f.i1.unpack v1) (f.i2.unpack v2) × (α ∧ β))

   bwd :: Partial => OpBwd (Raw BaseVal × Raw BaseVal)
   bwd ((u1 × u2) × Val α _) = pack f.i1 (f.i1.unpack u1 × α) : pack f.i2 (f.i2.unpack u2 × α) : Nil

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
      pack f.o <$> ((f.fwd x y × _) <$> new αs)
      where
      x × y = f.i.unpack v1 × f.i.unpack v2
      αs =
         if isZero x then singleton α
         else if isZero y then singleton β
         else singleton α # insert β

   fwd :: Partial => OpFwd (Raw BaseVal × Raw BaseVal)
   fwd (Val α v1 : Val β v2 : Nil) =
      pure $ (erase v1 × erase v2) ×
         pack f.o (f.fwd x y × if isZero x then α else if isZero y then β else α ∧ β)
      where
      x × y = f.i.unpack v1 × f.i.unpack v2

   bwd :: Partial => OpBwd (Raw BaseVal × Raw BaseVal)
   bwd ((u1 × u2) × Val α _) = pack f.i (x × β1) : pack f.i (y × β2) : Nil
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