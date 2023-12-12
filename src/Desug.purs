module Desug where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Desugarable (desug, desugBwd)
import Effect.Exception (Error)
import Expr (Expr)
import GaloisConnection (GaloisConnection(..))
import Lattice (class BoundedLattice, Raw, expand)
import SExpr (Expr) as S
import Util (successful)

-- Core-language slicing can produce "partial" slices, but these are not (yet) tolerated by desugaring.
type Desugaring a =
   { gc :: GaloisConnection (S.Expr a) (Expr a)
   , e :: Raw Expr -- original (non-partial) desugared expression
   }

desugGC
   :: forall a m
    . MonadError Error m
   => BoundedLattice a
   => Raw S.Expr
   -> m (Desugaring a)
desugGC s = pure $ { gc: GC { fwd, bwd }, e }
   where
   e = successful $ desug s
   fwd s' = successful $ desug s'
   bwd e' = desugBwd (expand e' e) s
