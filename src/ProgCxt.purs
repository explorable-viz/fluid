module ProgCxt where

import Prelude

import Data.List (List, zipWith)
import Data.Newtype (class Newtype)
import Data.Set (unions)
import Data.Traversable (class Foldable, class Traversable)
import Expr (Module)
import Graph (class Vertices, Vertex, vertices)
import Util.Set ((∪))
import Val (Env)

newtype ProgCxt a = ProgCxt
   { primitives :: Env a
   , mods :: List (Module a) -- in reverse order
   }

instance Vertices (ProgCxt Vertex) where
   vertices (ProgCxt { primitives, mods }) =
      vertices primitives
         ∪ unions (vertices <$> mods)

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
         }
