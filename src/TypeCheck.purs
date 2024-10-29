module TypeCheck where

import Prelude

import Control.Alt ((<|>))
import Data.Array (cons, elem, fromFoldable)
import Data.Either (choose)
import Data.Function (on)
import Data.Identity (Identity)
import Data.List (List(..), length, sortBy, zip, zipWith, (:), (\\))
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, Types(..), VarDef )
import Util (Endo, type (×), (×), type (+), error, onlyIf)
import Data.String(null)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Eq (class Eq, eq)
import Data.Ring
import Data.Field
import Debug

-- Error types for the reduction function
data ReduceError
  = TypeMismatch String String
  | InvalidOperator String
  | OtherError String

-- Allow the error messages to be displayed, make an instance of Show
instance showReduceError :: Show ReduceError where
  show (TypeMismatch expected actual) = "TypeMismatch: expected " <> expected <> ", got " <> actual
  show (InvalidOperator op) = "InvalidOperator: " <> op <> " is not a valid operator"
  show (OtherError msg) = "OtherError: " <> msg


-- Type checking function
typeCheck :: forall a. Expr a -> Boolean
typeCheck expr = case expr of
    -- Var "test"
    Var s -> not (null s)
    -- Op "+"
    Op s -> not (null s)
    -- Int unit 3
    Int _ _ -> true
    -- Float unit 2.3
    Float _ _ -> true
    -- Str unit "hello"
    Str _ _ -> true

    --  Constr unit "Var" ((Var "x") : Nil)
    -- Constr a ctr exprs -> true
    -- Record
    -- Dictionary
    -- Matrix
    -- Lambda
    -- Project exp s
    -- App exp1 exp2

    BinaryApp e1 op e2 -> case reduce (BinaryApp e1 op e2) of
        Left (err) -> false
        Right (exp) -> true

    -- MatchAs
    -- IfElse 

    ListEmpty _ ->  true
    
    -- ListEnum
    -- ListComp
    
    -- Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TCons "Integer") (Int unit 20)) Nil)) (Var "x")
    Let _ _ -> true
    
    -- LetRec
    _ -> false


reduce :: forall a. Expr a -> Either ReduceError (Expr a)
reduce expr = case expr of
    -- Base-level definitions can just return
    Var var -> Right (Var var)
    Op op -> Right (Op op)
    Int a num -> Right (Int a num)
    Float a num -> Right (Float a num)
    Str a string -> Right (Str a string)

    BinaryApp e1 op e2 -> do
        reducedE1 <- reduce e1
        reducedE2 <- reduce e2
        case reducedE1 of
            -- If 1st exp is Int, we only allow Int or another expression
            Int u1 n1 -> case reducedE2 of
                Int u2 n2 -> 
                    case getArithmeticOpFromString op of
                        Just (RingOp operation) -> Right (Int u1 (operation n1 n2))
                        Just (Div operation) -> case division (Left n1) (Left n2) of
                            Left (ans) -> Right (Int u1 ans)
                            _ -> Left (OtherError "Division error")
                        _ -> Left (InvalidOperator op)
                Float _ _ -> Left (TypeMismatch "Int" "Float")
                Str _ _ -> Left (TypeMismatch "Int" "Str")
                _ -> Right (BinaryApp reducedE1 op reducedE2)
            -- If 1st exp is Float, we onlt allow Float or another expression
            Float u1 n1 -> case reducedE2 of
                Float u2 n2 -> 
                    case getArithmeticOpFromString op of
                        Just (RingOp operation) -> Right (Float u1 (operation n1 n2))
                        Just (Div operation) -> case division (Right n1) (Right n2) of
                            Right (ans) -> Right (Float u1 ans)
                            _ -> Left (OtherError "Division error")
                        _ -> Left (InvalidOperator op)
                Int _ _ -> Left (TypeMismatch "Float" "Int")
                Str _ _ -> Left (TypeMismatch "Float" "Str")
                _ -> Right (BinaryApp reducedE1 op reducedE2)
            -- We do not allow operations to be performed on Strings
            Str _ _ -> Left (TypeMismatch "Expr" "Str")
            -- Everything else is an expression
            _ -> Right (BinaryApp reducedE1 op reducedE2)
    
    Let vardefs expr -> Right (Let vardefs expr)
    
    _ -> Left (OtherError "not supported yet")



data ArithmeticOperator = 
    RingOp (forall a. Ring a => a -> a -> a) 
    | Div (Either Int Number -> Either Int Number -> Either Int Number)
    -- | CompareOp (forall a. Ord a => a -> a -> Boolean)
    -- | Equality (forall a. Eq a => a -> a -> Boolean)

getArithmeticOpFromString :: String -> Maybe (ArithmeticOperator)
getArithmeticOpFromString op = case op of
    "+" -> Just (RingOp (+))
    "-" -> Just (RingOp (-))
    "*" -> Just (RingOp (*))
    "/" -> Just (Div division)
    -- ">" -> Just (CompareOp (>))
    -- "<" -> Just (CompareOp (<))
    -- "==" -> Just (Equality (==))
    _ -> Nothing

division :: Either Int Number -> Either Int Number -> Either Int Number
division (Left x) (Left y) = Left (x / y)
division (Right x) (Right y) = Right (x / y)
division _ _ = error "Cannot mix types"


typeCheckTest :: Boolean
typeCheckTest = typeCheck (Constr unit "Var" (Var "x" : Nil))