module TypeCheck where

import Prelude

import Control.Alt ((<|>))
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
import Data.NonEmpty(NonEmpty(..))

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


-- data Expr a
--    | Record a (List (Bind (Expr a)))
--    | Dictionary a (List (Pair (Expr a)))
--    | Matrix a (Expr a) (Var × Var) (Expr a)
--    | Lambda (Clauses a)
--    | Project (Expr a) Var
--    | App (Expr a) (Expr a)
--    | MatchAs (Expr a) (NonEmptyList (Pattern × Expr a))
--    | ListEnum (Expr a) (Expr a)
--    | ListComp a (Expr a) (List (Qualifier a))
--    | Let (VarDefs a) (Expr a)
--    | LetRec (RecDefs a) (Expr a)


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

    -- Constr unit "X" Nil
    Constr a ctr exprs -> case reduce (Constr a ctr exprs) of
        Left (err) -> false
        Right (exp) -> true

    BinaryApp e1 op e2 -> case reduce (BinaryApp e1 op e2) of
        Left (err) -> false
        Right (exp) -> true

    -- MatchAs

    -- (IfElse (BinaryApp (Int unit 3) "==" (Int unit 4)) (BinaryApp (Int unit 3) "+" (Int unit 1)) (BinaryApp (Int unit 3) "-" (Int unit 1)))
    IfElse e1 e2 e3 -> case reduce (IfElse e1 e2 e3) of
        Left (err) -> false
        Right (exp) -> true

    ListEmpty _ ->  true
    
    -- a = unit
    -- expr = Int unit 1
    -- qualifiers = (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit)))
    ListNonEmpty a expr qualifiers -> case reduce (ListNonEmpty a expr qualifiers) of
        Left (err) -> false
        Right (exp) -> true
    
    Let varDefs exp -> true
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
                        -- There is no Boolean data type so use Integers instead 
                        -- 1 = True     0 = False
                        Just (CompareOp operation) -> case (operation n1 n2) of
                            true -> Right (Int u1 1)
                            _ -> Right (Int u1 0)
                        Just (Equality operation) -> case (operation n1 n2) of
                            true -> Right (Int u1 1)
                            _ -> Right (Int u1 0)
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
                        -- There is no Boolean data type so use Integers instead 
                        -- 1 = True     0 = False
                        Just (CompareOp operation) -> case (operation n1 n2) of
                            true -> Right (Int u1 1)
                            _ -> Right (Int u1 0)
                        Just (Equality operation) -> case (operation n1 n2) of
                            true -> Right (Int u1 1)
                            _ -> Right (Int u1 0)
                        _ -> Left (InvalidOperator op)
                Int _ _ -> Left (TypeMismatch "Float" "Int")
                Str _ _ -> Left (TypeMismatch "Float" "Str")
                _ -> Right (BinaryApp reducedE1 op reducedE2)
            -- We do not allow operations to be performed on Strings
            Str _ _ -> Left (TypeMismatch "Expr" "Str")
            -- Everything else is an expression
            _ -> Right (BinaryApp reducedE1 op reducedE2)
        
    IfElse condition thenExpr elseExpr -> do
        reducedCond <- reduce condition
        case reducedCond of
            Int u1 0 -> reduce elseExpr
            Int u1 1 -> reduce thenExpr
            _ -> Left (OtherError "Condition is not a comparison")
    
    Constr a ctr exprs -> do
        reducedExprs <- reduceArgs exprs
        Right (Constr a ctr (reducedExprs))


    ListNonEmpty u exp qualifiers -> do
        reducedExp <- reduce exp
        reducedQ <- reduceList qualifiers
        Right (ListNonEmpty u reducedExp reducedQ)

    _ -> Left (OtherError "not supported yet")


reduceArgs :: forall a. List (Expr a) -> Either ReduceError (List (Expr a))
reduceArgs args = case args of
    Nil -> Right Nil
    x : xs -> do
        reducedExpr <- reduce x
        reducedRest <- reduceArgs xs
        Right (reducedExpr : reducedRest)

reduceList :: forall a. ListRest a -> Either ReduceError (ListRest a)
reduceList rest = case rest of
    End u -> Right (End u)
    Next u e next -> do
        reducedE <- reduce e
        reducedNext <- reduceList next
        Right (Next u reducedE reducedNext)


reduceVarDef :: forall a. VarDef a -> Either ReduceError (VarDef a)
reduceVarDef (VarDef pattern t exp) = do
    reducedExp <- reduce exp
    case (checkVarDefType t reducedExp) of
        Left err -> Left err
        Right _ -> Right (VarDef pattern t reducedExp)

checkVarDefType :: forall a. Types -> Expr a -> Either ReduceError Boolean
checkVarDefType ty exp = case ty of
    TCons "Int" -> case exp of
        Int _ _ -> Right true
        Float _ _ -> Left (TypeMismatch "Int" "Float")
        Str _ _ -> Left (TypeMismatch "Int" "Str")
        _ -> Left (OtherError "Something went wrong")
    TCons "Float" -> case exp of
        Int _ _ -> Left (TypeMismatch "Float" "Int")
        Float _ _ -> Right true
        Str _ _ -> Left (TypeMismatch "Float" "Str")
        _ -> Left (OtherError "Something went wrong")
    TCons "Str" -> case exp of
        Int _ _ -> Left (TypeMismatch "Str" "Int")
        Float _ _ -> Left (TypeMismatch "Str" "Float")
        Str _ _ -> Right true
        _ -> Left (OtherError "Something went wrong")
    _ -> Left (OtherError "Not implemented yet")

-- reduceVarDefs :: forall a.  NonEmptyList (VarDef a) -> Either ReduceError (NonEmptyList (VarDef a))
-- reduceVarDefs (NonEmptyList (NonEmpty (Let varDef exp))) = do
-- reduceVarDefs (Let varDef exp) = do 
--     reducedVarDef <- reduceVarDef varDef
--     reducedExp <- reduce exp
--     Right (Let reducedVarDef reducedExp)


data ArithmeticOperator = 
    RingOp (forall a. Ring a => a -> a -> a) 
    | Div (Either Int Number -> Either Int Number -> Either Int Number)
    | CompareOp (forall a. Ord a => a -> a -> Boolean)
    | Equality (forall a. Eq a => a -> a -> Boolean)

getArithmeticOpFromString :: String -> Maybe (ArithmeticOperator)
getArithmeticOpFromString op = case op of
    "+" -> Just (RingOp (+))
    "-" -> Just (RingOp (-))
    "*" -> Just (RingOp (*))
    "/" -> Just (Div division)
    ">" -> Just (CompareOp (>))
    "<" -> Just (CompareOp (<))
    "==" -> Just (Equality (==))
    _ -> Nothing

division :: Either Int Number -> Either Int Number -> Either Int Number
division (Left x) (Left y) = Left (x / y)
division (Right x) (Right y) = Right (x / y)
division _ _ = error "Cannot mix types"

typeCheckTest = typeCheck (Constr unit "Mylist" ((ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit)))) : Nil))

--typeCheckTest2 = typeCheck ((ListNonEmpty unit (Int unit 1) (End unit)))
--reductionTest = reduceVarDef (VarDef (PVar "x") (TCons "Int") (Int unit 42))


-- Use the videos for synth and check formulas

-- Arithmetic operations work for Int and Float so need to check both are the same
-- e1 + e2
-- infer by looking at '+' and check that e1 is type t and e2 is also type t
-- then return TCons "..."

-- Lookup table
