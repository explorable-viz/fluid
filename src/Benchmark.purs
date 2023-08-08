module Benchmark where

import Prelude

import Data.Foldable (foldl)
import Data.Set (fromFoldable)
import Effect (Effect)
import Effect.Console (logShow, log)
import Graph (GraphImpl, Vertex(..), extend, fwdSlice, inE)

main :: Effect Unit
main = graphTestScript

--runSuite [ benchOutStar ]

graphTestScript :: Effect Unit
graphTestScript = do
   let
      ids = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]
      graph = foldl (\g α -> extend (Vertex (show α)) (fromFoldable $ map (Vertex <<< show) [ α + 2, α + 3 ]) g) mempty ids :: GraphImpl
   let
      slice = fwdSlice (fromFoldable [ (Vertex "13"), (Vertex "12"), Vertex "11" ]) graph
   log ("Outedges: " <> show (inE (fromFoldable [ (Vertex "11") ]) graph))
   --logShow graph
   logShow slice

