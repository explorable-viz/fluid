module Main where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Debug.Trace (trace) as T
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

trace s a = T.trace (pretty s) $ \_-> a
trace' s a = T.trace  s $ \_-> a

runExampleBwd :: String -> Effect Unit
runExampleBwd src =
    let e = successfulParse src program
        ρ = Empty
    in  case eval ρ e of
            Left msg -> error msg
            Right (Tuple t v) -> do
                let ρ' × e' × α = eval_bwd v t
                log $ render (pretty $ e')

testExampleBwd :: String -> Effect Unit
testExampleBwd src =
    let e = successfulParse src program
        ρ = Empty
    in  case eval ρ e of
            Left msg -> error msg
            Right (Tuple t v) -> do
                let ρ' × e' × α = eval_bwd v t
                case eval ρ' e' of
                    Left msg -> error msg
                    Right (Tuple t' v') -> do
                        let bt = (render $ pretty t) == (render $ pretty t')
                            bv = (render $ pretty v) == (render $ pretty v')
                        log $ show (bt && bv)

letexpr :: String
letexpr = "(1 + 5) * ((let x = 2; let y = 8; x * y) - (let y = 3; y * y))"

composeexpr :: String
composeexpr = "let incr = fun n -> n + 1; incr (incr 3)"

factexpr :: String
factexpr = "let fact x =\
            \ match x == 0 as {\
            \    True -> 1;\
            \    False -> x * fact (x - 1)\
            \ };\
            \fact 2"

tailexpr :: String
tailexpr = "let tail zs =\
            \ match zs as {\
            \    Nil -> Nil;\
            \    Cons x xs -> xs\
            \ };\
            \tail (Cons 6 (Cons 3 (Cons 2 Nil)))"

main :: Effect Unit
main = do
   testExampleBwd tailexpr
