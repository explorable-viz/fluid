module Desug where

import Desugarable (desug, desugBwd)
import Prelude
import Control.Monad.Error.Class (class MonadError)
import Effect.Exception (Error)
import Expr (Expr)
import GaloisConnection (GaloisConnection)
import Lattice (Raw, class BoundedJoinSemilattice)
import SExpr (Expr) as S
import Util (successful)

newtype DesugGaloisConnection a = DesugGaloisConnection
   ( GaloisConnection (S.Expr a) (Expr a)
        ()
   )

desugGC
   :: forall a m
    . MonadError Error m
   => BoundedJoinSemilattice a
   => Raw S.Expr
   -> m (DesugGaloisConnection a)
desugGC s0 = do
   let
      fwd s = successful $ desug s
      bwd e = desugBwd e s0
   pure (DesugGaloisConnection { fwd, bwd })
