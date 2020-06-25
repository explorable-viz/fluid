module Main where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (trace)
import Bindings
import Bwd (eval_bwd)
import Effect (Effect)
import Effect.Console (log)
import Eval (eval)
import Fwd (eval_fwd)
import Module (successfulParse)
import Parse (program)
import Pretty (pretty, render)
import Primitive (primitives)
import Util (error, (×))
import Val (Val(..))
import Val (RawVal(..)) as V

runExampleBwd :: String -> Effect Unit
runExampleBwd src =
    let e = successfulParse src program
        ρ = Empty
    in  case eval ρ e of
            Left msg -> error msg
            Right (Tuple t v) -> do
                let ρ' × e × α = eval_bwd v t
                log $ render (pretty $ t × v)

letexpr :: String
letexpr = "(1 + 5) * ((let x = 2; let y = 8; x * y) - (let y = 3; y * y))"

composeexpr :: String
composeexpr = "let incr = fun n -> n + 1; incr (incr 3)"


testEnv1 :: Bindings Val
testEnv1 = primitives :+: "x" ↦ Val false (V.Int 0)

testEnv2 :: Bindings Val
testEnv2 = primitives :+: "x" ↦ Val false (V.Int 1)

main :: Effect Unit
main = do
   -- let r = joinDebug "wtf" testEnv1 testEnv2
   -- log $ render $ pretty r
   runExampleBwd composeexpr