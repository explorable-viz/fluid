module TypeCheck where

import Prelude

import Control.Alt ((<|>))
import Data.Array (cons, elem, fromFoldable, all)
import Data.Either (choose)
import Data.Function (on)
import Data.Identity (Identity)
import Data.List (List(..), (:), concat, foldr, groupBy, singleton, snoc, sortBy)
import Data.List.NonEmpty (NonEmptyList(..), toList )
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, Types(..) )
import Util (Endo, type (×), (×), type (+), error, onlyIf)
import Data.String(null)
import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Eq (class Eq, eq)
import Data.Ring
import Data.Field
-- import Data.Fractional (Fractional, (/) )

-- Type checking function
typeCheck :: forall a. Expr a -> Boolean
typeCheck expr = case expr of
    Var s -> not (null s)
    Op s -> not (null s)
    Int _ _ -> true
    Float _ _ -> true
    Str _ _ -> true
    -- Constr
    -- Record
    -- Dictionary
    -- Matrix
    -- Lambda
    -- Project exp s
    -- App exp1 exp2
    -- BinaryApp
    -- MatchAs
    -- IfElse 
    ListEmpty _ ->  true
    -- ListEnum
    -- ListComp
    -- Let
    -- LetRec
    _ -> false

reduce :: forall a. Expr a -> Maybe (Expr a)
reduce expr = case expr of
    Var _ -> Nothing
    Op _ -> Nothing
    Int _ _ -> Nothing
    Float _ _ -> Nothing
    Str _ _ -> Nothing
    -- Check with Dominic, this could be for binary operations
    BinaryApp e1 op e2 -> case getArithmeticOpFromString op of
        Just (RingOp operation) ->
            case e1 of
                Int u1 n1 ->
                    case e2 of 
                        Int u2 n2 -> Just (Int u1 (operation n1 n2))
                        Float u2 n2 -> error "Cannot mix data types"
                        _ -> Just (BinaryApp e1 op (maybe e2 identity (reduce e2)))
                Float u1 n1 ->
                    case e2 of 
                        Float u2 n2 -> Just (Float u1 (operation n1 n2))
                        Int u2 n2 -> error "Cannot mix data types"
                        _ -> Just (BinaryApp e1 op (maybe e2 identity (reduce e2)))
                _ -> 
                    case e2 of
                        Int u2 n2 -> Just (BinaryApp (maybe e1 identity (reduce e1)) op e2)
                        Float u2 n2 -> Just (BinaryApp (maybe e1 identity (reduce e1)) op e2)
                        _ -> Just (BinaryApp (maybe e1 identity (reduce e1)) op (maybe e2 identity (reduce e2)))
        Just (Div operation) -> 
            case e1 of
                Int u1 n1 ->
                    case e2 of 
                        Int u2 n2 -> do
                            case division (Left n1) (Left n2) of
                                Left ans -> Just (Int u1 ans)
                                _ -> Nothing
                        Float u2 n2 -> error "Cannot mix data types"
                        _ -> Just (BinaryApp e1 op (maybe e2 identity (reduce e2)))
                Float u1 n1 ->
                    case e2 of 
                        Float u2 n2 -> do
                            case division (Right n1) (Right n2) of
                                Right ans -> Just (Float u1 ans)
                                _ -> Nothing
                        Int u2 n2 -> error "Cannot mix data types"
                        _ -> Just (BinaryApp e1 op (maybe e2 identity (reduce e2)))
                _ -> 
                    case e2 of
                        Int u2 n2 -> Just (BinaryApp (maybe e1 identity (reduce e1)) op e2)
                        Float u2 n2 -> Just (BinaryApp (maybe e1 identity (reduce e1)) op e2)
                        _ -> Just (BinaryApp (maybe e1 identity (reduce e1)) op (maybe e2 identity (reduce e2)))
        _ -> Nothing

    _ -> Nothing

identity :: forall a. a -> a
identity x = x

class Division a where
  divide :: a -> a -> a
instance Division Int where
  divide a b = a / b
instance Division Number where
  divide a b = a / b

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