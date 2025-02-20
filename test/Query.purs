module Test.Query where

import Prelude

import Control.Promise (fromAff)
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty)
import Data.TraversableWithIndex (forWithIndex)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import EvalGraph (graphEval)
import Graph (Vertex, VertexData, runQuery, typeName, unpack)
import Lattice (erase)
import Module (File(..), Folder(..))
import Module.Node (loadProgCxt, prepConfig)
import Pretty (prettyP)
import Unsafe.Coerce (unsafeCoerce)
import Util ((×))
import Val (BaseVal(..), MatrixDim(..), MatrixRep(..))

main :: Effect Unit
main = void $ fromAff do
   let fluidSrcPaths = [ Folder "test/fluid", Folder "fluid" ]
   progCxt <- loadProgCxt fluidSrcPaths testQuery.imports []
   { e, gconfig } <- prepConfig fluidSrcPaths (File testQuery.file) progCxt
   ge <- graphEval gconfig e
   let outs = runQuery findMat ge.g
   _ <- forWithIndex (fromFoldable outs) \i out -> do
      logShow i
      log $ (prettyP <<< erase) out
      log ""
   pure unit

type TestQuerySpec a =
   { imports :: Array String
   , file :: String
   , fwd_expect :: String
   , query :: VertexData -> Maybe a
   , intermediates :: Set a
   }

testQuery :: TestQuerySpec (MatrixDim Vertex)
testQuery =
   { file: "slicing/convolution/edgeDetect"
   , imports:
        [ "lib/convolution"
        , "slicing/convolution/filter/edge-detect"
        , "slicing/convolution/test-image"
        ]
   , fwd_expect: ""
   , query: findMatDim
   , intermediates: empty
   }

findMatDim :: VertexData -> Maybe (MatrixDim Vertex)
findMatDim vd = case unpack typeName vd of
   "MatrixDim" -> let md = unpack unsafeCoerce vd in Just md
   _ -> Nothing

findMat :: VertexData -> Maybe (BaseVal Vertex)
findMat vd = case unpack typeName vd of
   "BaseVal" -> case unpack unsafeCoerce vd of
      m@(Matrix (MatrixRep (_ × MatrixDim (i × _) × MatrixDim (j × _))))
         | i == 3 && j == 3 -> Just m
         | otherwise -> Nothing
      _ -> Nothing
   _ -> Nothing
