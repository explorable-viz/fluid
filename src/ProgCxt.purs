module ProgCxt where

import Prelude

import Bind (Bind)
import Data.List (List, zipWith)
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong (second)
import Data.Traversable (class Foldable, class Traversable)
import Expr (Expr, Module)
import Val (Env)

-- Module context (plus datasets, reflecting current ad hoc approach to those).
newtype ProgCxt a = ProgCxt
   { primitives :: Env a
   , mods :: List (Module a) -- in reverse order
   , datasets :: List (Bind (Expr a))
   }

-- ======================
-- boilerplate
-- ======================
derive instance Newtype (ProgCxt a) _
derive instance Functor ProgCxt
derive instance Traversable ProgCxt
derive instance Foldable ProgCxt

instance Apply ProgCxt where
   apply (ProgCxt fζ) (ProgCxt ζ) =
      ProgCxt
         { primitives: fζ.primitives <*> ζ.primitives
         , mods: fζ.mods `zipWith (<*>)` ζ.mods
         , datasets: (second (<*>) <$> fζ.datasets) `zipWith (<*>)` ζ.datasets
         }
