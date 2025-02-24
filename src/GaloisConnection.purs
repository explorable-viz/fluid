module GaloisConnection where

import Prelude hiding (join, top)

import Data.Newtype (class Newtype)
import Data.Profunctor.Strong ((***)) as Strong
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple (uncurry)
import Lattice (class BoundedMeetSemilattice, class JoinSemilattice, class Neg, neg, top, (∨))
import Util (Endo, type (×), (×), dup)

newtype GaloisConnection a b = GC
   { fwd :: a -> b -- upper adjoint, meet-preserving
   , bwd :: b -> a -- lower adjoint, join-preserving
   }

derive instance Newtype (GaloisConnection a b) _

deMorgan :: forall a b. Neg a => Neg b => Endo (a -> b)
deMorgan = (neg >>> _) >>> (_ >>> neg)

-- Could unify deMorgan and dual but would need to reify notion of opposite category.
dual :: forall a b. Neg a => Neg b => GaloisConnection a b -> GaloisConnection b a
dual (GC { fwd, bwd }) = GC { fwd: deMorgan bwd, bwd: deMorgan fwd }

-- TODO: restate in terms of (&&&).
relatedInputs
   :: forall a b
    . Neg a
   => Neg b
   => JoinSemilattice b
   => GaloisConnection a b
   -> GaloisConnection (a × b) a
relatedInputs f = (f *** identity) >>> meet >>> dual f

relatedOutputs
   :: forall a b
    . Neg a
   => JoinSemilattice a
   => Neg b
   => GaloisConnection a b
   -> GaloisConnection (b × a) b
relatedOutputs f = (dual f *** identity) >>> meet >>> f

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

fanout :: forall a b c. Neg a => JoinSemilattice a => GaloisConnection a b -> GaloisConnection a c -> GaloisConnection a (b × c)
fanout f g = join >>> (f *** g)

meet :: forall a. Neg a => JoinSemilattice a => GaloisConnection (a × a) a
meet = dual join

join :: forall a. JoinSemilattice a => GaloisConnection a (a × a)
join = GC { fwd: dup, bwd: uncurry (∨) }

unfst :: forall a b. BoundedMeetSemilattice b => GaloisConnection a (a × b)
unfst = GC { fwd: \a -> a × top, bwd: Tuple.fst }

unsnd :: forall a b. BoundedMeetSemilattice a => GaloisConnection b (a × b)
unsnd = GC { fwd: \b -> top × b, bwd: Tuple.snd }

infixr 3 splitStrong as ***
infixr 3 fanout as &&&
