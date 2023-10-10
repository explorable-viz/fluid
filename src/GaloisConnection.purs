module GaloisConnection where

import Prelude

import Data.Newtype (class Newtype)
import Lattice (class BooleanLattice, neg)
import Util (Endo)

newtype GaloisConnection a b = GC { fwd :: a -> b, bwd :: b -> a }

derive instance Newtype (GaloisConnection a b) _

deMorgan :: forall a b. BooleanLattice a => BooleanLattice b => Endo (a -> b)
deMorgan = (neg >>> _) >>> (_ >>> neg)

-- Could unify deMorgan and dual but would need to reify notion of opposite category.
dual :: forall a b. BooleanLattice a => BooleanLattice b => GaloisConnection a b -> GaloisConnection b a
dual (GC { fwd, bwd }) = GC { fwd: deMorgan bwd, bwd: deMorgan fwd }

instance Semigroupoid GaloisConnection where
   compose (GC { fwd: fwd1, bwd: bwd1 }) (GC { fwd: fwd2, bwd: bwd2 }) =
      GC { fwd: fwd1 <<< fwd2, bwd: bwd1 >>> bwd2 }

instance Category GaloisConnection where
   identity = GC { fwd: identity, bwd: identity }
