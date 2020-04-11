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

data Expl =  ExplBottom
           | ExplVar Var
           | ExplInt Int 
           | ExplPair Expl Expl
           | ExplNil
           | ExplCons Expl Expl
           | ExplApp Expl Expl Match Expl
           | ExplMatch Expl Match Expl
           | ExplAdd Expl Expl
           | ExplLet Var Expl Expl
           | ExplLetrec Var Expl Expl
           | ExplFun Env Elim 
           | ExplTrue
           | ExplFalse


derive instance eqExpl :: Eq Expl
instance showExpl :: Show Expl where
  show (ExplBottom)         = "ExplBottom"
  show (ExplVar v )         = "ExplVar " <> v
  show (ExplInt n)          = "ExplInt " <> show n
  show (ExplPair t1 t2)     = "ExplPair " <> show t1 <> show t2
  show (ExplNil)            = "ExplNil "
  show (ExplCons x xs)      = "ExplCons " <> show x <> " " <> show xs
  show (ExplApp t1 t2 m b)  = "ExplCons " <> show t1 <> " " <> show t2 <> " " <> show m <> " " <> show b
  show (ExplLet x e1 e2)    = "ExplLet " <> x <> " " <> show e1 <> " " <> show e1
  show (ExplAdd t1 t2)      = "ExplAdd " <> show t1 <> " " <> show t2
  show (ExplMatch t1 m t2)  = "ExplMatch " <> show t1 <> " " <> show m <> " " <> show t2
  show (ExplLetrec x t1 t2) = "ExplLetrec " <> x <> " " <> show t1 <> " " <> show t2
  show (ExplFun env elim)   = "ExplClosure " <> show env <> " " <> show elim
  show ExplTrue             = "ExplTrue "
  show ExplFalse            = "ExplFalse "
