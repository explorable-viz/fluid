module BoolAlg where

import Prelude

import Control.Apply (lift2)
import Control.Biapply (bilift2)
import Data.Profunctor.Strong ((***))
import Data.Set (Set, intersection, union)
import Data.Set (difference, empty) as S
import Lattice (𝔹)
import Util (type (×), (×), Endo)

-- Candidate replacement for Lattice.purs, using records rather than type classes as the latter are too
-- inflexible for the granularity of instande we require. Also flatten the hiearchy of types.
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

slices :: forall f a b. Apply f => BoolAlg a -> f b -> BoolAlg (f a)
slices 𝒶 x =
   { top: x <#> const 𝒶.top
   , bot: x <#> const 𝒶.bot
   , meet: lift2 𝒶.meet
   , join: lift2 𝒶.join
   , neg: (_ <#> 𝒶.neg)
   }

prod :: forall a b. BoolAlg a -> BoolAlg b -> BoolAlg (a × b)
prod 𝒶 𝒷 =
   { top: 𝒶.top × 𝒷.top
   , bot: 𝒶.bot × 𝒷.bot
   , meet: 𝒶.meet `bilift2` 𝒷.meet
   , join: 𝒶.join `bilift2` 𝒷.join
   , neg: 𝒶.neg *** 𝒷.neg
   }
