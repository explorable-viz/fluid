module Val where

import Data.Eq (class Eq)
import Bindings
import Expr (Elim)

type Env = Bindings Val

data Val = ValBottom
         | ValTrue | ValTrueSel
         | ValFalse | ValFalseSel
         | ValInt Int | ValIntSel Int
         | ValClosure Env String Elim
         | ValPair Val Val | ValPairSel Val Val
         | ValNil | ValNilSel
         | ValCons Val Val | ValConsSel Val Val
         | ValFailure String

derive instance eqVal :: Eq Val
