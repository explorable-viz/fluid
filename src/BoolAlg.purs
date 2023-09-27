module BoolAlg where

import Prelude
import Data.Profunctor.Strong ((***))
import Data.Set (Set, intersection, union)
import Data.Set (difference, empty) as S
import Lattice (𝔹)
import Util (type (×), (×), Endo)

-- Candidate replacement for Lattice.purs, using records rather than type classes as the latter are too
-- inflexible for the granularity of instande we require. Also flatten the hiearchy of types.

-- Sucks a bit as a type class, let's try a record.
type BoolAlg a =
   { top :: a
   , bot :: a
   , meet :: a -> a -> a
   , join :: a -> a -> a
   , neg :: Endo a
   }

bool :: BoolAlg 𝔹
bool =
   { top: true
   , bot: false
   , meet: (&&)
   , join: (||)
   , neg: not
   }

powerset :: forall a. Ord a => Set a -> BoolAlg (Set a)
powerset xs =
   { top: xs
   , bot: S.empty
   , meet: intersection
   , join: union
   , neg: (xs `S.difference` _)
   }

slices :: forall f. Apply f => f 𝔹 -> BoolAlg (f 𝔹)
slices x =
   { top: x <#> const bool.top
   , bot: x <#> const bool.bot
   , meet: \y z -> bool.meet <$> y <*> z
   , join: \y z -> bool.join <$> y <*> z
   , neg: (_ <#> bool.neg)
   }

prod :: forall a b. BoolAlg a -> BoolAlg b -> BoolAlg (a × b)
prod l1 l2 =
   { top: l1.top × l2.top
   , bot: l1.bot × l2.bot
   , meet: \(x1 × y1) (x2 × y2) -> x1 `l1.meet` x2 × y1 `l2.meet` y2
   , join: \(x1 × y1) (x2 × y2) -> x1 `l1.join` x2 × y1 `l2.join` y2
   , neg: l1.neg *** l2.neg
   }
