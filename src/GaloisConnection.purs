module GaloisConnection where

import Prelude

import BoolAlg (BoolAlg)
import Util (Endo)

-- Galois connections are more general, this is specialised to Boolean algebras.
type GaloisConnection a b r =
   { fwd :: a -> b
   , bwd :: b -> a
   | r
   }

deMorgan :: forall a b. BoolAlg a -> BoolAlg b -> Endo (a -> b)
deMorgan 𝒶 𝒷 = (𝒶.neg >>> _) >>> (_ >>> 𝒷.neg)

-- Could unify deMorgan and dual but would need to reify notion of opposite category.
dual :: forall a b r. BoolAlg a -> BoolAlg b -> GaloisConnection a b r -> GaloisConnection b a r
dual 𝒶 𝒷 gc@{ fwd, bwd } = gc { fwd = deMorgan 𝒷 𝒶 bwd, bwd = deMorgan 𝒶 𝒷 fwd }
