module GaloisConnection where

import Prelude

import Lattice (class BooleanLattice, neg)
import Util (Endo)

-- a and b are posets, but we don't enforce that here.
type GaloisConnection a b = {
   fwd :: a -> b,
   bwd :: b -> a
}

deMorgan :: forall a b. BooleanLattice a => BooleanLattice b => Endo (a -> b)
deMorgan f = neg >>> f >>> neg

-- Could unify deMorgan and dual but would need to reify notion of opposite category.
dual :: forall a b. BooleanLattice a => BooleanLattice b => GaloisConnection a b -> GaloisConnection b a
dual { fwd, bwd } = { fwd: deMorgan bwd, bwd: deMorgan fwd }
