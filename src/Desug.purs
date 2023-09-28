module Desug where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Desugarable (desug, desugBwd)
import Effect.Exception (Error)
import Expr (Expr)
import GaloisConnection (GaloisConnection)
import Lattice (class BoundedJoinSemilattice, Raw, botOf)
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
      dom = botOf s0
      codom = botOf $ successful $ desug s0
      fwd s = successful $ desug s
      bwd e = desugBwd e s0
   pure { dom, codom, fwd, bwd }
