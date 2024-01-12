module GaloisConnection where

import Prelude hiding (top)

import Data.Newtype (class Newtype)
import Data.Profunctor.Strong ((***)) as Strong
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple (uncurry)
import Lattice (class BooleanLattice, class BoundedMeetSemilattice, class MeetSemilattice, class Neg, neg, top, (∧))
import Util (Endo, type (×), (×), dup)

newtype GaloisConnection a b = GC { fwd :: a -> b, bwd :: b -> a }

derive instance Newtype (GaloisConnection a b) _

deMorgan :: forall a b. Neg a => Neg b => Endo (a -> b)
deMorgan = (neg >>> _) >>> (_ >>> neg)

-- Could unify deMorgan and dual but would need to reify notion of opposite category.
dual :: forall a b. Neg a => Neg b => GaloisConnection a b -> GaloisConnection b a
dual (GC { fwd, bwd }) = GC { fwd: deMorgan bwd, bwd: deMorgan fwd }

relatedInputs :: forall a b. Neg a => BooleanLattice b => GaloisConnection a b -> GaloisConnection a (a × b)
relatedInputs gc = gc >>> diag >>> (dual gc *** identity)

relatedOutputs :: forall a b. BooleanLattice a => Neg b => GaloisConnection a b -> GaloisConnection b (b × a)
relatedOutputs gc = (gc *** identity) <<< diag <<< dual gc

instance Semigroupoid GaloisConnection where
   compose (GC { fwd: fwd1, bwd: bwd1 }) (GC { fwd: fwd2, bwd: bwd2 }) =
      GC { fwd: fwd1 <<< fwd2, bwd: bwd1 >>> bwd2 }

instance Category GaloisConnection where
   identity = GC { fwd: identity, bwd: identity }

-- Galois connections have products. Data.Profunctor requires pre-/post-composability with arbitrary functions
-- (which may not have adjoints), similar to how Haskell's Control.Arrow requires an injection from arbitrary
-- functions. So for now side-step Data.Profunctor[.Strong] and define products directly.
splitStrong
   :: forall a b c d
    . GaloisConnection a b
   -> GaloisConnection c d
   -> GaloisConnection (a × c) (b × d)
splitStrong (GC { fwd: fwd1, bwd: bwd1 }) (GC { fwd: fwd2, bwd: bwd2 }) =
   GC { fwd: fwd1 Strong.*** fwd2, bwd: bwd1 Strong.*** bwd2 }

first :: forall a b c. GaloisConnection a b -> GaloisConnection (a × c) (b × c)
first = (_ *** identity)

second :: forall a b c. GaloisConnection a b -> GaloisConnection (c × a) (c × b)
second = (identity *** _)

fanout :: forall a b c. MeetSemilattice a => GaloisConnection a b -> GaloisConnection a c -> GaloisConnection a (b × c)
fanout f g = diag >>> (f *** g)

diag :: forall a. MeetSemilattice a => GaloisConnection a (a × a)
diag = GC { fwd: dup, bwd: uncurry (∧) }

fst :: forall a b. BoundedMeetSemilattice b => GaloisConnection (a × b) a
fst = GC { fwd: Tuple.fst, bwd: \a -> a × top }

snd :: forall a b. BoundedMeetSemilattice a => GaloisConnection (a × b) b
snd = GC { fwd: Tuple.snd, bwd: \b -> top × b }

infixr 3 splitStrong as ***
