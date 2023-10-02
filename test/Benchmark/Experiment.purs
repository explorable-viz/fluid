module Test.Benchmark.PureComp

  where

import Prelude

import Data.JSDate (getMilliseconds, now)
import Effect (Effect)
import Effect.Class.Console (log)

now' :: Effect Number
now' = now >>= getMilliseconds

-- | Fibonacci as a pure function
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- | Benchmarking Fibonacci, where we let-bind its result
benchFibLetBind :: Int -> Effect Int
benchFibLetBind n = do
  t1 <- now'
  let m = fib n                -- If `m` is not yet used
  t2 <- now'
  log (show (t2 - t1) <> "ms") -- prints 0.0 ms
  pure m

-- | Benchmarking Fibonacci, where we let-bind its result and log it
benchFibLetBindPrint :: Int -> Effect Int
benchFibLetBindPrint n = do
  t1 <- now'
  let m = fib n                 -- If `m` is subsequently printed
  log (show m)
  t2 <- now'
  log (show (t2 - t1) <> "ms")  -- prints correct ms
  pure m

-- | Benchmarking Fibonacci, where we monadically bind its result (using `pure`)
benchFibDoBind :: Int -> Effect Int
benchFibDoBind n = do
  t1 <- now'
  m <- pure $ fib n             -- If `fib` is wrapped in `pure`, and the result `m` is not yet used
  t2 <- now'
  log (show (t2 - t1) <> "ms")  -- prints 0.0 ms
  pure m

-- | Benchmarking Fibonacci, where we monadically bind its result (using `pure`) and log it
benchFibDoBindPrint :: Int -> Effect Int
benchFibDoBindPrint n = do
  t1 <- now'
  m <- pure $ fib n               -- If `fib` is wrapped in `pure`, and the result `m` is subsequently printed
  log (show m)
  t2 <- now'
  log (show (t2 - t1) <> "ms")   -- prints correct ms
  pure m

-- | Fibonacci as an effectful function
fibm :: Int -> Effect Int
fibm 0 = pure 0
fibm 1 = pure 1
fibm n = do
  n1 <- fibm (n-1)
  n2 <- fibm (n-2)
  pure (n1 + n2)

-- | Benchmarking the effectful version of Fibonacci, where we monadically bind its result
benchFibmBind :: Int -> Effect Int
benchFibmBind n = do
  t1 <- now'
  m <- fibm n                    -- If `fib` itself is defined as an effectful computation `fibm`
  t2 <- now'
  log (show (t2 - t1) <> "ms")   -- prints correct ms (without needing the result `m` to be used beforehand)
  pure m

benchFibs :: Effect Unit
benchFibs = do
  log ("-- | Benchmarking Fibonacci, where we let-bind its result")
  _ <- benchFibLetBind 40
  log ("-- | Benchmarking Fibonacci, where we let-bind its result and log it")
  _ <- benchFibLetBindPrint 40
  log ("-- | Benchmarking Fibonacci, where we monadically bind its result (using `pure`)")
  _ <- benchFibDoBind 40
  log ("-- | Benchmarking Fibonacci, where we monadically bind its result (using `pure`) and log it")
  _ <- benchFibDoBindPrint 40
  log ("-- | Benchmarking the effectful version of Fibonacci, where we monadically bind its result")
  _ <- benchFibmBind 40
  pure unit
