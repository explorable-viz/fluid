module Env2 where

import Prelude
import Data.List.NonEmpty (NonEmptyList, cons, cons', singleton, tail)
import Data.Map (insert, pop, toUnfoldable)
import Data.Maybe (Maybe(..))
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
   let vs × γ'' = pop x γ # fromJust (x <> " not found")
   in update' γ'' γ' # insert x (cons' v $ tail vs)

concat :: forall a . Env2 a -> Bindings (Val a) -> Env2 a
concat γ Lin            = γ
concat γ (γ' :- x ↦ v)  =
   case pop x γ of
   Nothing -> concat γ γ' # insert x (singleton v)
   Just (vs × γ'') -> concat γ'' γ' # insert x (v `cons` vs)
