module GaloisConnection where

import Prelude
import Lattice (class BooleanLattice, neg)

-- Galois connections are actually more general (a and b need only be posets, not Boolean lattices).
type GC a b = {
   fwd :: a -> b,
   bwd :: b -> a
}

deMorgan :: forall a b. BooleanLattice a => BooleanLattice b => (a -> b) -> a -> b
deMorgan f = neg >>> f >>> neg

-- Could unify deMorgan and dual but would need to reify notion of opposite category.
dual :: forall a b. BooleanLattice a => BooleanLattice b => GC a b -> GC b a
dual { fwd, bwd } = { fwd: deMorgan bwd, bwd: deMorgan fwd }
