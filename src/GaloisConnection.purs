module GaloisConnection where

import Prelude

import Lattice (class BooleanLattice, neg)
import Util (Endo)

-- a and b are posets, but we don't enforce that here. Use record rather than type class so we can extend with
-- explicit value-level representation of domain/codomain.
type GaloisConnection a b r =
   { dom :: a
   , codom :: b
   , fwd :: a -> b
   , bwd :: b -> a
   | r
   }

deMorgan :: forall a b. BooleanLattice a => BooleanLattice b => Endo (a -> b)
deMorgan = (neg >>> _) >>> (_ >>> neg)

-- Could unify deMorgan and dual but would need to reify notion of opposite category.
dual :: forall a b r. BooleanLattice a => BooleanLattice b => GaloisConnection a b r -> GaloisConnection b a r
dual gc@{ dom, codom, fwd, bwd } = gc { dom = codom, codom = dom, fwd = deMorgan bwd, bwd = deMorgan fwd }
