module Env2 where

import Prelude
import Data.List.NonEmpty (NonEmptyList, cons, cons', singleton, tail)
import Data.Map (Map, insert, pop, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (uncurry)
import Bindings2 (Bind(..), Bindings, (↦), Var)
import Util2 ((×), definitely)
import Util.SnocList2 (SnocList(..), (:-))
import Val2 (Val)

type Env2 a = Map Var (NonEmptyList (Val a))
type SingletonEnv a = Map Var (Val a)

update :: forall a . Env2 a -> SingletonEnv a -> Env2 a
update γ γ' = update' γ (uncurry Bind <$> toUnfoldable γ')

update' :: forall a . Env2 a -> Bindings (Val a) -> Env2 a
update' γ Lin              = γ
update' γ (γ' :- x ↦ v)    =
   let vs × γ'' = pop x γ # definitely ("contains " <> x)
   in update' γ'' γ' # insert x (cons' v $ tail vs)

concat :: forall a . Env2 a -> SingletonEnv a -> Env2 a
concat γ γ' = concat' γ (uncurry Bind <$> toUnfoldable γ')

concat' :: forall a . Env2 a -> Bindings (Val a) -> Env2 a
concat' γ Lin            = γ
concat' γ (γ' :- x ↦ v)  =
   case pop x γ of
   Nothing -> concat' γ γ' # insert x (singleton v)
   Just (vs × γ'') -> concat' γ'' γ' # insert x (v `cons` vs)
