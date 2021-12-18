module Test.Main2 where

import Prelude
import Data.List (List(..), (:))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (before, it)
import DataType (cCons)
import Lattice (𝔹)
import Module (File(..), openDatasetAs)
import Test.Util (Test, run, testBwd)
import Util ((×))
import Val (Val(..))

blah :: Aff Unit
blah = do
   ρ0 × ρ <- openDatasetAs (File "example/linking/renewables") "data"
   pure unit

test_fig :: Aff Unit -> Test Unit
test_fig setup =
   before setup $
      it "hello" \_ -> do
         pure unit

tests :: Array (Test Unit)
tests = [test_fig blah]

main :: Effect Unit
main = void (sequence (run <$> tests))

-- TODO: move to common location.
hole :: Val 𝔹
hole = Hole false

test_scratchpad :: Array (Test Unit)
test_scratchpad = [
   testBwd (File "section-5-example") (File "section-5-example-1.expect")
           (Constr true cCons (hole : (Constr false cCons (hole : (Constr false cCons (hole : hole : Nil)) : Nil)) : Nil))
           "(88 _:_ (6 : (4 : [])))"
]
