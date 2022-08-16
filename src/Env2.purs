module Env2 where

import Prelude
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Map (insert, pop, toUnfoldable)
import Data.NonEmpty ((:|))
import Data.Tuple (uncurry)
import Bindings2 (Bind(..), Bindings, (↦))
import Bindings2New (Bindings2)
import Util2 ((×), fromJust)
import Util.SnocList2 (SnocList(..), (:-))
import Val2 (Val)

type Env2 a = Bindings2 (NonEmptyList (Val a))
type SingletonEnv a = Bindings2 (Val a)

update :: forall a . Env2 a -> SingletonEnv a -> Env2 a
update γ γ' = update' γ (uncurry Bind <$> toUnfoldable γ')

update' :: forall a . Env2 a -> Bindings (Val a) -> Env2 a
update' γ Lin              = γ
update' γ (γ' :- x ↦ v)    =
   let NonEmptyList (_ :| vs) × γ'' = fromJust (x <> " not found") $ pop x γ
   in insert x (NonEmptyList $ v :| vs) (update' γ'' γ')
