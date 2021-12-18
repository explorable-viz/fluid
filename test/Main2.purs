module Test.Main2 where

import Prelude
import Data.List (List(..), (:))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff)
import App.Renderer (Fig)
import DataType (cCons)
import Lattice (𝔹)
import Module (File(..))
import Test.Util (Test, run, selectNth, testBwd)
import Val (Val(..))

test_fig :: Aff Fig -> Test Unit
test_fig _ = pure unit

tests :: Array (Test Unit)
tests = test_scratchpad

main :: Effect Unit
main = void (sequence (run <$> tests))

-- TODO: move to common location.
hole :: Val 𝔹
hole = Hole false

test_scratchpad :: Array (Test Unit)
test_scratchpad = [
   testBwd (File "section-5-example") (File "section-5-example-1.expect")
           (Constr true cCons (hole : (Constr false cCons (hole : (Constr false cCons (hole : hole : Nil)) : Nil)) : Nil))
           "(88 _:_ (6 : (4 : [])))",
   testBwd (File "section-5-example") (File "section-5-example-2.expect")
           (selectNth 1 (Hole true))
           "(_88_ : (_6_ : (_4_ : [])))",
   testBwd (File "section-5-example") (File "section-5-example-3.expect")
           (Constr false cCons (hole : (Constr false cCons (hole : (Constr true cCons (hole : hole : Nil)) : Nil)) : Nil))
           "(88 : (6 : (4 _:_ [])))"
]
