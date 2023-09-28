module GaloisConnection where

import Prelude
import Data.Function (identity) as F
import Lattice (class BooleanLattice, neg)
import Util (Endo)

-- a and b are posets, but we don't enforce that here. Use record rather than type class so we can extend with
-- explicit value-level representation of domain/codomain.
type GaloisConnection a b =
   { dom :: a
   , codom :: b
   , fwd :: a -> b
   , bwd :: b -> a
   }

deMorgan :: forall a b. BooleanLattice a => BooleanLattice b => Endo (a -> b)
deMorgan = (neg >>> _) >>> (_ >>> neg)

-- Could unify deMorgan and dual but would need to reify notion of opposite category.
dual :: forall a b. BooleanLattice a => BooleanLattice b => GaloisConnection a b -> GaloisConnection b a
dual gc = gc { dom = gc.codom, codom = gc.dom, fwd = deMorgan gc.bwd, bwd = deMorgan gc.fwd }

identity :: forall a. a -> GaloisConnection a a
identity a = { dom: a, codom: a, fwd: F.identity, bwd: F.identity }
