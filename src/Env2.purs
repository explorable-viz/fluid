module Env2 where

import Prelude
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (insert, pop, toUnfoldable)
import Data.NonEmpty ((:|))
import Data.Tuple (uncurry)
import Bindings2 (Bind(..), Bindings, (↦))
import Bindings2New (Bindings2)
import Partial.Unsafe (unsafePartial)
import Util2 (Endo, (×), fromJust)
import Util.SnocList2 (SnocList(..), (:-))
import Val2 (Val)

type Env2 a = Bindings2 (NonEmptyList (Val a))

-- A "singleton" env is one where every variable is mapped to a singleton list.
-- Second arg here is a singleton.
update :: forall a . Env2 a -> Endo (Env2 a)
update γ γ' = update' γ (uncurry Bind <$> toUnfoldable γ')

update' :: forall a . Env2 a -> Bindings (NonEmptyList (Val a)) -> Env2 a
update' γ Lin                          = γ
update' γ (γ' :- x ↦ NonEmptyList us)  =
   let NonEmptyList (_ :| vs) × γ'' = fromJust (x <> " not found") $ pop x γ
       v = unsafePartial (\(y :| Nil) -> y) us
   in insert x (NonEmptyList $ v :| vs) (update' γ'' γ')
