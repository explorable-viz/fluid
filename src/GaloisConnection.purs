module GaloisConnection where

import Prelude

import BoolAlg (BoolAlg)
import Util (Endo)

-- a and b are posets, but we don't enforce that here. Use record rather than type class so we can extend with
-- explicit value-level representation of index (e.g. graph or trace) for families of GCs.
type GaloisConnection a b =
   { fwd :: a -> b
   , bwd :: b -> a
   }

deMorgan :: forall a b. BoolAlg a -> BoolAlg b -> Endo (a -> b)
deMorgan 𝒶 𝒷 = (𝒶.neg >>> _) >>> (_ >>> 𝒷.neg)

-- Could unify deMorgan and dual but would need to reify notion of opposite category.
dual :: forall a b. BoolAlg a -> BoolAlg b -> GaloisConnection a b -> GaloisConnection b a
dual 𝒶 𝒷 { fwd, bwd } = { fwd: deMorgan 𝒷 𝒶 bwd, bwd: deMorgan 𝒶 𝒷 fwd }
