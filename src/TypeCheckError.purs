module TypeCheckError where

import SExpr (Types(..))
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Prelude

data TypeErr = 
    LookupNil String
    | TypeMismatch String
    | ParseErr String
    | InvalidSyntax String
    | InvalidType String

derive instance Eq TypeErr
derive instance Generic TypeErr _
instance Show TypeErr where
   show c = genericShow c