module Expl where

import Prelude
import Expr (Elim, Env, Var)

data Match = MatchVar Var
           | MatchTrue
           | MatchFalse
           | MatchPair Var Var
           | MatchNil
           | MatchCons Var Var

derive instance eqMatch :: Eq Match
instance showMatch :: Show Match where
  show (MatchVar v )     = "MatchVar " <> v
  show (MatchPair t1 t2) = "MatchPair " <> show t1 <> show t2
  show (MatchNil)        = "MatchNil "
  show (MatchCons x xs)  = "MatchCons " <> x <> " " <> xs
  show (MatchTrue)       = "MatchTrue"
  show (MatchFalse)      = "MatchFalse"

data Trace = TraceBottom
           | TraceVar Var
           | TraceNum Int
           | TracePair Trace Trace
           | TraceNil
           | TraceCons Trace Trace
           | TraceApp Trace Trace Match Trace
           | TraceMatch Trace Match Trace
           | TraceAdd Trace Trace
           | TraceLet Var Trace Trace
           | TraceLetrec Var Trace Trace
           | TraceClosure Env Elim
           | TraceTrue
           | TraceFalse


derive instance eqTrace :: Eq Trace
instance showTrace :: Show Trace where
  show (TraceBottom)        = "TraceBottom"
  show (TraceVar v )        = "TraceVar " <> v
  show (TraceNum n)         = "TraceNum " <> show n
  show (TracePair t1 t2)    = "TracePair " <> show t1 <> show t2
  show (TraceNil)           = "TraceNil "
  show (TraceCons x xs)     = "TraceCons " <> show x <> " " <> show xs
  show (TraceApp t1 t2 m b) = "TraceCons " <> show t1 <> " " <> show t2 <> " " <> show m <> " " <> show b
  show (TraceLet x e1 e2)   = "TraceLet " <> x <> " " <> show e1 <> " " <> show e1
  show (TraceAdd t1 t2)     = "TraceAdd " <> show t1 <> " " <> show t2
  show (TraceMatch t1 m t2) = "TraceMatch " <> show t1 <> " " <> show m <> " " <> show t2
  show (TraceLetrec x t1 t2) = "TraceLetrec " <> x <> " " <> show t1 <> " " <> show t2
  show (TraceClosure env elim) = "TraceClosure " <> show env <> " " <> show elim
  show TraceTrue            = "TraceTrue "
  show TraceFalse           = "TraceFalse "
