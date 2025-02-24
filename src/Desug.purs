module Desug where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Desugarable (desug, desugBwd)
import Effect.Exception (Error)
import Expr (Expr)
import GaloisConnection (GaloisConnection(..))
import Lattice (class BoundedLattice, Raw)
import SExpr (Expr) as S
import Util (defined)

-- Core-language slicing can produce "partial" slices, but these are not (yet) tolerated by desugaring.
type Desugaring a =
   { gc :: GaloisConnection (S.Expr a) (Expr a)
   , e :: Raw Expr -- original (non-partial) desugared expression
   }

desugGC
   :: forall a m
    . MonadError Error m
   => Eq a
   => BoundedLattice a
   => Raw S.Expr
   -> m (Desugaring a)
desugGC s = pure $ { gc: GC { fwd, bwd }, e }
   where
   e = defined $ desug s
   fwd s' = defined $ desug s'
   bwd e' = desugBwd e' s
