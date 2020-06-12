module Elim where

import Prelude hiding (top)
import Data.Maybe (Maybe(..))
import Bindings (Var)
import Lattice (class Selectable, (∧?), mapα, maybeZipWithα)
import Util ((≟))

data Elim k =
   ElimVar Var k |
   ElimBool { true :: k, false :: k } |
   ElimPair (Elim (Elim k)) |
   ElimList { nil :: k, cons :: Elim (Elim k) }

derive instance elimFunctor :: Functor Elim

instance elimSelectable :: Selectable k => Selectable (Elim k) where
   mapα f (ElimVar x κ) = ElimVar x (mapα f κ)
   mapα f (ElimBool { true: κ, false: κ' })   = ElimBool { true: mapα f κ, false: mapα f κ' }
   mapα f (ElimPair σ)                        = ElimPair $ map (mapα f) σ
   mapα f (ElimList { nil: κ, cons: σ })      = ElimList { nil: mapα f κ, cons: map (mapα f) σ }

   maybeZipWithα f (ElimVar x κ) (ElimVar x' κ') =
      ElimVar <$> x ≟ x' <*> κ ∧? κ'
   maybeZipWithα f (ElimBool { true : κ1, false : κ2 }) (ElimBool { true : κ1', false : κ2' }) =
      (\κ κ' -> ElimBool { true : κ, false : κ' }) <$> maybeZipWithα f κ1 κ1' <*> maybeZipWithα f κ2 κ2'
   maybeZipWithα f (ElimPair σ) (ElimPair σ') =
      ElimPair <$> maybeZipWithα f σ σ'
   maybeZipWithα f (ElimList { nil: κ1, cons: σ1 }) (ElimList { nil: κ2, cons: σ2 }) =
      (\κ σ -> ElimList { nil: κ, cons: σ }) <$> maybeZipWithα f κ1 κ2 <*> maybeZipWithα f σ1 σ2
   maybeZipWithα _ _ _ = Nothing