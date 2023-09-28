module Desug where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Desugarable (desug, desugBwd)
import Effect.Exception (Error)
import Expr (Expr)
import GaloisConnection (GaloisConnection(..))
import Lattice (class BoundedJoinSemilattice, Raw)
import SExpr (Expr) as S
import Util (successful)

desugGC
   :: forall a m
    . MonadError Error m
   => BoundedJoinSemilattice a
   => Raw S.Expr
   -> m (GaloisConnection (S.Expr a) (Expr a))
desugGC s0 = do
   let
      fwd s = successful $ desug s
      bwd e = desugBwd e s0
   pure $ GC { fwd, bwd }
