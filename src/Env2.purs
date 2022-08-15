module Env2 where

import Prelude
import Data.List (List, (:))
import Data.Map (insert, pop, toUnfoldable)
import Data.Tuple (uncurry)
import Bindings2 (Bind(..), Bindings, (↦))
import Bindings2New (Bindings2)
import Partial.Unsafe (unsafePartial)
import Util2 (Endo, type (×), (×), fromJust)
import Util.SnocList2 (SnocList(..), (:-))
import Val2 (Val)

type Env2 a = Bindings2 (List (Val a))

-- A "singleton" env is one where every variable is mapped to a singleton list.
-- Second arg here is a singleton.
update :: forall a . Env2 a -> Endo (Env2 a)
update γ γ' = update' γ (uncurry Bind <$> toUnfoldable γ')

update' :: forall a . Env2 a -> Bindings (List (Val a)) -> Env2 a
update' γ Lin              = γ
update' γ (γ' :- x ↦ us)  =
   let vs × γ'' = fromJust (x <> " not found") $ pop x γ :: List (Val a) × Env2 a
   in unsafePartial $ insert x (replaceHead (fromSingleton us) vs) (γ'' `update'` γ')

replaceHead :: Partial => forall a . a -> Endo (List a)
replaceHead y (_ : xs) = y : xs

fromSingleton :: Partial => forall a . List a -> a
fromSingleton (x : _) = x
