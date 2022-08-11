module Test.Spec.Mocha (
  runMocha,
  MOCHA()
  ) where

import Prelude

import Data.Either (either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Test.Spec (SpecT, collect)
import Test.Spec.Tree (Item(..), Tree(..))

foreign import data MOCHA :: Type

foreign import itAsync
  :: Boolean
  -> String
   -> (Effect Unit
       -> (Error -> Effect Unit)
       -> Effect Unit)
   -> Effect Unit

foreign import itPending
   :: String
   -> Effect Unit

foreign import describe
  :: Boolean
  -> String
  -> Effect Unit
  -> Effect Unit

registerGroup
  :: ∀ m
   . Tree m (Item Aff Unit)
  -> Effect Unit
registerGroup tree =
  case tree of
    Leaf name (Just (Item { isFocused, example })) ->
        itAsync isFocused name cb
        where
          cb onSuccess onError =
            runAff_ (either onError (const onSuccess)) (example (\a -> a unit))
    Leaf name Nothing ->
      itPending name
    Node _ t ->
      traverse_ registerGroup t

runMocha
  :: ∀ a
   . SpecT Aff Unit Effect a
  -> Effect Unit
runMocha spec =
   traverse_ registerGroup =<< collect spec
