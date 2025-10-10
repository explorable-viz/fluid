module Util.Array where

import Prelude
import Data.Array (foldl, index, snoc)
import Data.Maybe (Maybe(..), maybe)

-- Defined in Data.Array 7.2.x but upgrading the package set seems to throw away optparse.
-- Never use sequence to transpose arrays.
transpose :: forall a. Array (Array a) -> Array (Array a)
transpose xs = go 0 []
   where
   go :: Int -> Array (Array a) -> Array (Array a)
   go idx allArrays = case buildNext idx of
      Nothing -> allArrays
      Just next -> go (idx + 1) (snoc allArrays next)

   buildNext :: Int -> Maybe (Array a)
   buildNext idx = do
      xs # flip foldl Nothing \acc nextArr -> do
         maybe acc (\el -> Just $ maybe [ el ] (flip snoc el) acc) $ index nextArr idx
