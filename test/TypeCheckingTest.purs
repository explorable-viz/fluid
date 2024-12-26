module TypeCheckingTest where

import Prelude

import TypeChecking (check, synth, check', synth', pushVarDef, liftTypes, Context)
import Data.List.NonEmpty (NonEmptyList(..), cons)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Tuple (Tuple(..))
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, Types(..), VarDef, DictEntry(..) )
import Bind (Bind, Var, varAnon, (↦), keys)
import Data.List (List(..), sortBy, zip, zipWith, (:), (\\), singleton)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Parsing (runParser)
import Parse (program, expr_)
import TypeCheckError (TypeErr(..))
import Data.Either (Either(..))

logTestResult :: String -> Boolean -> Effect Unit
logTestResult message result = do
    if result then
        log ("PASS: " <> message)
    else
        log ("FAIL: " <> message)

-- Unit tests for the check function
testCheck :: Effect Unit
testCheck = do
    -- Int
    logTestResult "Check Int valid" (check Nil (Int unit 42) (TCons "Int"))
    logTestResult "Check Int invalid" (not (check Nil (Str unit "hello") (TCons "Int")))
    -- Str
    logTestResult "Check Str valid" (check Nil (Str unit "hello") (TCons "Str"))
    logTestResult "Check Str invalid" (not (check Nil (Str unit "hello") (TCons "Int")))
    -- Float
    logTestResult "Check Float valid" (check Nil (Float unit 5.0) (TCons "Float"))
    logTestResult "Check Float invalid" (not (check Nil (Int unit 5) (TCons "Float")))
    -- Var
    let varValid = check (singleton (Tuple "x" (TCons "Int"))) (Var "x") (TCons "Int")
    logTestResult "Check Var valid" varValid
    let varInvalid = check (singleton (Tuple "x" (TCons "Int"))) (Var "y") (TCons "Int")
    logTestResult "Check Var invalid" (not varInvalid)
    -- BinaryApp
    let binaryAppValid = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Int unit 2) "+" (Int unit 5)) (TCons "Int")
    logTestResult "Check BinaryApp valid" binaryAppValid
    let binaryAppBoolValid = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Int unit 2) ">" (Int unit 5)) (TCons "Bool")
    logTestResult "Check BinaryApp boolean valid" binaryAppBoolValid
    let binaryAppInvalid = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Str unit "2") ">" (Int unit 5)) (TCons "Bool")
    logTestResult "Check BinaryApp invalid" (not binaryAppInvalid)
    let binaryAppRecursion = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (BinaryApp (Int unit 3) "+" (Int unit 1)) "==" (Int unit 4)) (TCons "Bool")
    logTestResult "Check BinaryApp with recursion" binaryAppRecursion
    -- Let
    let testPVar = check (singleton (Tuple "x" (TCons "Int"))) (Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TCons "Int") (Int unit 20)) Nil)) (Var "x")) (TCons "Int")
    logTestResult "Check Let with PVar" testPVar
    let testPConstr = check ((Tuple "C" (TList (TCons "Int"))):(Tuple "x" (TCons "Int")):Nil) (Let (NonEmptyList (NonEmpty (VarDef (PConstr "C" ((PVar "x") : Nil)) (TList (TCons "Int")) (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit))))) Nil)) (Constr unit "C" Nil)) (TList (TCons "Int"))
    logTestResult "Check Let with PConstr" testPConstr
    let testPListNonEmpty = check (singleton (Tuple "x" (TList (TCons "Int")))) ((Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TList (TCons "Int")) (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit))))) Nil)) (Var "x"))) (TList (TCons "Int"))
    logTestResult "Check Let with PListNonEmpty" testPListNonEmpty
    let testPListEmpty = check (singleton (Tuple "x" (TList (TCons "Int")))) (Let (NonEmptyList (NonEmpty (VarDef PListEmpty (TList (TCons "Int")) (ListEmpty unit)) Nil)) (Var "x")) (TList (TCons "Int"))
    logTestResult "Check Let with PListEmpty" testPListEmpty
    let testPRecord = check ((Tuple "x" (TCons "Int")) : (Tuple "y" (TCons "Str")) : (Tuple "z" (TDict (TCons "Int") (TCons "Str"))) : Nil) (Let (NonEmptyList (NonEmpty (VarDef (PRecord (("x" ↦ PVar "x"):("y" ↦ PVar "y"):Nil)) (TDict (TCons "Int") (TCons "Str")) (Var "z")) Nil)) (Var "z")) (TDict (TCons "Int") (TCons "Str"))
    logTestResult "Check Let with PRecord" testPRecord
    let letInvalid = check (singleton (Tuple "x" (TCons "Int"))) (Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TCons "Int") (Str unit "20")) Nil)) (Var "x")) (TCons "Int")
    logTestResult "Check Let invalid" (not letInvalid)
    -- ListEmpty
    let listEmptyValid = check Nil (ListEmpty unit) (TList (TCons "unknown"))
    logTestResult "Check ListEmpty valid" listEmptyValid
    let listEmptyInvalid = check Nil (ListEmpty unit) (TCons "Int")
    logTestResult "Check ListEmpty invalid" (not listEmptyInvalid)
    -- ListNonEmpty
    let listNonEmptyValid = check Nil (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit)))) (TList(TCons "Int"))
    logTestResult "Check ListNonEmpty valid" listNonEmptyValid
    let listNonEmptyInvalid = check Nil (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Str unit "3") (End unit)))) (TCons "Int")
    logTestResult "Check ListNonEmpty valid" (not listNonEmptyInvalid)
    -- IfElse
    let ifElseValid = check Nil (IfElse (BinaryApp (Int unit 1) ">" (Int unit 2)) (Int unit 1) (Int unit 0)) (TCons "Int")
    logTestResult "Check IfElse valid" ifElseValid
    let ifElseInvalid = check Nil (IfElse (BinaryApp (Int unit 1) ">" (Int unit 2)) (Int unit 1) (Str unit "hello")) (TCons "Int")
    logTestResult "Check IfElse invalid" (not ifElseInvalid)
    -- Dictionary
    let testDictExpr = check Nil (Dictionary unit ((Tuple (ExprKey (Str unit "a")) (Int unit 1)) : (Tuple (ExprKey (Str unit "b")) (Int unit 2)) : Nil)) (TDict (TCons "Str") (TCons "Int"))
    logTestResult "Check Dictionary with ExprKey valid" (testDictExpr)
    let testDictVar = check ((Tuple "a" (TCons "Str")) : (Tuple "b" (TCons "Str")) : Nil) (Dictionary unit ((Tuple (VarKey unit "a") (Int unit 1)) : (Tuple (VarKey unit "b") (Int unit 2)) : Nil)) (TDict (TCons "Str") (TCons "Int"))
    logTestResult "Check Dictionary with VarKey valid" (testDictVar)
    let testDictInvalid = check ((Tuple "a" (TCons "Str")) : (Tuple "b" (TCons "Str")) : Nil) (Dictionary unit ((Tuple (VarKey unit "a") (Int unit 1)) : (Tuple (ExprKey (BinaryApp (Str unit "2") ">" (Int unit 5))) (Int unit 2)) : Nil)) (TDict (TCons "Str") (TCons "Int"))
    logTestResult "Check Dictionary invalid" (not testDictInvalid)
    -- ListEnum
    let listEnumValid = check ((Tuple "x" (TCons "Str")):(Tuple "y" (TCons "Str")):Nil) (ListEnum (Var "x") (Var "y")) (TList (TCons "Str"))
    logTestResult "Check ListEnum valid" listEnumValid
    let listEnumInvalid = check Nil (ListEnum (Int unit 1) (Str unit "hello")) (TList (TCons "Str"))
    logTestResult "Check ListEnum Invalid" (not listEnumInvalid)
    -- ListComp
    let listCompGuard = check Nil (ListComp unit (Int unit 1) ((ListCompGuard (Int unit 2)):(ListCompGuard (Int unit 3)):Nil)) (TList (TCons "Int"))
    logTestResult "Check ListComp Guard valid" listCompGuard
    let listCompGenerator = check Nil (ListComp unit (Int unit 1) ((ListCompGuard (Int unit 2)) : (ListCompGen (PVar "x") (Int unit 3)) : Nil)) (TList (TCons "Int"))
    logTestResult "Check ListComp Generator valid" listCompGenerator
    let listCompDeclaration = check Nil (ListComp unit (Int unit 1) ((ListCompGuard (Int unit 2)) : (ListCompDecl (VarDef (PVar "x") (TCons "Int") (Int unit 20))) : Nil)) (TList (TCons "Int"))
    logTestResult "Check ListComp Declaration valid" listCompDeclaration
    let listCompInvalid = check Nil (ListComp unit (Int unit 1) ((ListCompGuard (Int unit 2)) : (ListCompDecl (VarDef (PVar "x") (TCons "Int") (Str unit "20"))) : Nil)) (TList (TCons "Int"))
    logTestResult "Check ListComp invalid" (not listCompInvalid)

-- Unit tests for the synth function
testSynth :: Effect Unit
testSynth = do
    -- Int
    logTestResult "Synth Int" ((synth Nil (Int unit 1)) == (Just (TCons "Int")))
    -- Str
    logTestResult "Synth Str" ((synth Nil (Str unit "hello")) == (Just (TCons "Str")))
    -- Float
    logTestResult "Synth Float" ((synth Nil (Float unit 1.0)) == (Just (TCons "Float")))
    -- Var
    let varValid = synth (singleton (Tuple "x" (TCons "Int"))) (Var "x")
    logTestResult "Synth Var valid" (varValid == (Just (TCons "Int")))
    let varInvalid = synth (singleton (Tuple "x" (TCons "Int"))) (Var "y")
    logTestResult "Synth Var invalid" (varInvalid == Nothing)
    -- BinaryApp
    let binaryAppValid = synth Nil (BinaryApp (Int unit 1) "+" (Int unit 2))
    logTestResult "Synth BinaryApp valid" (binaryAppValid == (Just (TCons "Int")))
    let binaryAppVar = synth (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Var "x") "+" (Int unit 2))
    logTestResult "Synth BinaryApp with Var" (binaryAppVar == (Just (TCons "Int")))
    let binaryAppBool = synth (singleton (Tuple "x" (TCons "Float"))) (BinaryApp (Var "x") ">" (Float unit 2.0))
    logTestResult "Synth BinaryApp with Var" (binaryAppBool == (Just (TCons "Bool")))
    let binaryAppInvalid = synth (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (BinaryApp (Int unit 1) "+" (Int unit 2)) "==" (Str unit "test"))
    logTestResult "Synth BinaryApp invalid" (binaryAppInvalid == Nothing)
    -- Let
    let testPVar = synth (singleton (Tuple "x" (TCons "Int"))) (Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TCons "Int") (Int unit 20)) Nil)) (Var "x"))
    logTestResult "Synth Let with PVar" (testPVar == (Just (TCons "Int")))
    let testPConstr = synth ((Tuple "C" (TList (TCons "Int"))):(Tuple "x" (TCons "Int")):Nil) (Let (NonEmptyList (NonEmpty (VarDef (PConstr "C" ((PVar "x") : Nil)) (TList (TCons "Int")) (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit))))) Nil)) (Constr unit "C" Nil))
    logTestResult "Synth Let with PConstr" (testPConstr == (Just (TList (TCons "Int"))))
    let testPListNonEmpty = synth (singleton (Tuple "x" (TList (TCons "Int")))) ((Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TList (TCons "Int")) (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit))))) Nil)) (Var "x")))
    logTestResult "Synth Let with PListNonEmpty" (testPListNonEmpty == (Just (TList (TCons "Int"))))
    let testPListEmpty = synth (singleton (Tuple "x" (TList (TCons "Int")))) (Let (NonEmptyList (NonEmpty (VarDef PListEmpty (TList (TCons "Int")) (ListEmpty unit)) Nil)) (Var "x"))
    logTestResult "Synth Let with PListEmpty" (testPListEmpty == (Just (TList (TCons "Int"))))
    let testPRecord = synth ((Tuple "x" (TCons "Int")) : (Tuple "y" (TCons "Str")) : (Tuple "z" (TDict (TCons "Int") (TCons "Str"))) : Nil) (Let (NonEmptyList (NonEmpty (VarDef (PRecord (("x" ↦ PVar "x"):("y" ↦ PVar "y"):Nil)) (TDict (TCons "Int") (TCons "Str")) (Var "z")) Nil)) (Var "z"))
    logTestResult "Synth Let with PRecord" (testPRecord == (Just (TDict (TCons "Int") (TCons "Str"))))
    let letInvalid = synth (singleton (Tuple "x" (TCons "Int"))) (Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TCons "Int") (Str unit "20")) Nil)) (Var "x"))
    logTestResult "Synth Let invalid" (letInvalid == Nothing)
    -- ListEmpty
    let listEmptyValid = synth Nil (ListEmpty unit)
    logTestResult "Synth ListEmpty valid" (listEmptyValid == (Just (TList (TCons "unknown"))))
    -- ListNonEmpty
    let listNonEmptyValid = synth Nil (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit))))
    logTestResult "Synth ListNonEmpty valid" (listNonEmptyValid == (Just (TList(TCons "Int"))))
    let listNonEmptyInvalid = synth Nil (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Str unit "3") (End unit))))
    logTestResult "Synth ListNonEmpty invalid" (listNonEmptyInvalid == Nothing)
    -- -- IfElse
    let ifElseValid = synth Nil (IfElse (BinaryApp (Int unit 1) ">" (Int unit 2)) (Int unit 1) (Int unit 0))
    logTestResult "Synth IfElse valid" (ifElseValid == (Just (TCons "Int")))
    let ifElseInvalid = synth Nil (IfElse (BinaryApp (Int unit 1) ">" (Int unit 2)) (Int unit 1) (Str unit "hello"))
    logTestResult "Synth IfElse invalid" (ifElseInvalid == Nothing)
    -- Dictionary
    let dictExpr = synth Nil (Dictionary unit ((Tuple (ExprKey (Str unit "a")) (Int unit 1)) : (Tuple (ExprKey (Str unit "b")) (Int unit 2)) : Nil))
    logTestResult "Synth Dictionary valid" (dictExpr == (Just (TDict (TCons "Str") (TCons "Int"))))
    let dictInvalid = synth ((Tuple "a" (TCons "Str")) : (Tuple "b" (TCons "Str")) : Nil) (Dictionary unit ((Tuple (VarKey unit "a") (Int unit 1)) : (Tuple (VarKey unit "b") (Float unit 3.0)) : Nil))
    logTestResult "Synth Dictionary invalid" (dictInvalid == Nothing)
    -- ListEnum
    let listEnum = synth Nil (ListEnum (Int unit 1) (Int unit 2))
    logTestResult "Synth ListEnum valid" (listEnum == (Just (TList (TCons "Int"))))
    let listEnumInvalid = synth Nil (ListEnum (Int unit 1) (Str unit "hello"))
    logTestResult "Synth ListEnum invalid" (listEnumInvalid == Nothing)
    -- ListComp
    let listCompGuard = synth Nil (ListComp unit (Int unit 1) ((ListCompGuard (Int unit 2)):(ListCompGuard (Int unit 3)):Nil))
    logTestResult "Synth ListComp Guard valid" (listCompGuard == (Just (TList (TCons "Int"))))
    let listCompGenerator = synth Nil (ListComp unit (Int unit 1) ((ListCompGuard (Int unit 2)) : (ListCompGen (PVar "x") (Int unit 3)) : Nil))
    logTestResult "Synth ListComp Generator valid" (listCompGenerator == (Just (TList (TCons "Int"))))
    let listCompDeclaration = synth Nil (ListComp unit (Int unit 1) ((ListCompGuard (Int unit 2)) : (ListCompDecl (VarDef (PVar "x") (TCons "Int") (Int unit 20))) : Nil))
    logTestResult "Synth ListComp Declaration valid" (listCompDeclaration == (Just (TList (TCons "Int"))))
    let listCompInvalid = synth Nil (ListComp unit (Int unit 1) ((ListCompGuard (Int unit 2)) : (ListCompDecl (VarDef (PVar "x") (TCons "Int") (Str unit "20"))) : Nil))
    logTestResult "Synth ListComp invalid" (listCompInvalid == Nothing)


-- Test function to check that the context is updated
updateContextTest :: Effect Unit
updateContextTest = do
    let initContext = Nil :: Context
    log ("Initial context: " <> show initContext)
    let updatedContext1 = pushVarDef initContext "x" (TCons "Int")
    log ("Context after adding x: " <> show updatedContext1)
    let updatedContext2 = pushVarDef updatedContext1 "y" (TCons "Str")
    log ("Context after adding y: " <> show updatedContext2) 
    let updatedContext3 = pushVarDef updatedContext2 "z" (TCons "Bool")
    log ("Context after adding z: " <> show updatedContext3) 


testCheck' :: Effect Unit
testCheck' = do 
    let intTest = check' Nil (runParser "1" program) (TCons "Int")
    logTestResult "check' Integer" (intTest == Right true)
    let strTest = check' Nil (runParser "\"a\"" program) (TCons "Str")
    logTestResult "check' String" (strTest == Right true)
    let floatTest = check' Nil (runParser "2.0" program) (TCons "Float")
    logTestResult "check' Float" (floatTest == Right true)
    let binaryAppTest = check' ((Tuple "x" (TCons "Int")):Nil) (runParser "1 + x" program) (TCons "Int")
    logTestResult "check' binaryApp" (binaryAppTest == Right true)
    let varTestValid = check' ((Tuple "x" (TCons "Str")):Nil) (runParser "x" program) (TCons "Str")
    logTestResult "check' var valid" (varTestValid == Right true)
    let varTestInvalid = check' Nil (runParser "x" program) (TCons "Int")
    logTestResult "check' var invalid" (varTestInvalid == (Left (LookupNil "Unbound variable found: x")))
    -- Let
    let varDefTest = check' Nil (runParser "let x :: Int = 20 in x;" expr_) (TCons "Int")
    logTestResult "check' let var" (varDefTest == Right true)
    let varDefEmptyList = check' Nil (runParser "let [] :: [Int] = [] in [];" expr_) (TList (TCons "Int"))
    logTestResult "check' let emptyList" (varDefEmptyList == Right true)
    let varDefConstr = check' ((Tuple "C" (TList (TCons "Int"))):(Tuple "x" (TCons "Int")):Nil) (runParser "let C :: [Int] = [1, 2, 3] in C;" expr_) (TList (TCons "Int"))
    logTestResult "check' let constr" (varDefConstr == Right true)
    let varDefList = check' ((Tuple "a" (TCons "Str")):(Tuple "b" (TCons "Str")):Nil) (runParser "let [a, b] :: [Str] = [\"a\", \"b\"] in [a, b];" expr_) (TList (TCons "Str"))
    logTestResult "check' let nonEmptyList" (varDefList == Right true)
    let varDefDict = check' Nil (runParser "let {} :: {Str, Int} = {} in {};" expr_) (TDict (TCons "Str") (TCons "Int"))
    logTestResult "check' let dict" (varDefDict == Right true)
    -- Empty List
    let emptyList = check' Nil (runParser "[]" program) (TList (TCons "Str"))
    logTestResult "check' empty list" (emptyList == Right true)
    let emptyListInvalid = check' Nil (runParser "[]" program) (TList (TCons "IDK"))
    logTestResult "check' empty list" (emptyListInvalid == Left (InvalidType "Type [IDK] is not accepted"))
    -- NonEmpty List
    let nonEmptyList = check' Nil (runParser "[1, 2, 3]" program) (TList (TCons "Int"))
    logTestResult "check' nonEmpty list" (nonEmptyList == Right true)
    -- If Else
    let ifElseValid = check' ((Tuple "x" (TCons "Int")):Nil) (runParser "if x > 2 then 20 else 40" program) (TCons "Int")
    logTestResult "check' if-else valid" (ifElseValid == Right true)
    let ifElseInvalid = check' ((Tuple "x" (TCons "Int")):Nil) (runParser "if x then 20 else 40" program) (TCons "Int") 
    logTestResult "check' if-else invalid" (ifElseInvalid == (Left (InvalidType "Condition must be of type Bool")))
    let ifElseMismatch = check' Nil (runParser "if 1 > 2 then 20 else \"a\"" expr_) (TCons "Int")
    logTestResult "check' if-else type mismatch" (ifElseMismatch == (Left (TypeMismatch "Cannot match Int with Str")))
    -- Dictionary
    let dictTestVar = check' ((Tuple "a" (TCons "Str")):(Tuple "b" (TCons "Str")):Nil) (runParser "{a:1, b:2}" program) (TDict (TCons "Str") (TCons "Int"))
    logTestResult "check' dictionary varKey" (dictTestVar == Right true)
    let dictTestExpr = check' Nil (runParser "{ [\"a\"]: 5, [\"b\"] : 6 }" program) (TDict (TCons "Str")(TCons "Int"))
    logTestResult "check' dictionary exprKey" (dictTestExpr == Right true)
    let dictTestInvalid = check' Nil (runParser "{ [\"a\"]: 5, [\"b\"] : 6 }" program) (TDict (TCons "Str") (TCons "Str"))
    logTestResult "check' dictionary invalid" (dictTestInvalid == (Left (TypeMismatch "Cannot match {Str, Int} with {Str, Str}")))
    -- Constructor
    let constrTest = check' ((Tuple "C" (TCons "C")):Nil) (runParser "C = 1" expr_) (TCons "C")
    logTestResult "check' constr" (constrTest == Right true)
    let constrTestInvalid = check' Nil (runParser "C" program) (TCons "A")
    logTestResult "check' constr invalid" (constrTestInvalid == (Left (TypeMismatch "Cannot match C with A")))
    -- ListEnum
    let enumTest = check' Nil (runParser "[1 .. 2]" expr_) (TList (TCons "Int"))
    logTestResult "check' listEnum" (enumTest == Right true)
    let enumTestInvalid = check' Nil (runParser "[1 .. 3]" expr_) (TList (TCons "Str"))
    logTestResult "check' listEnum invalid" (enumTestInvalid == (Left (TypeMismatch "Cannot match [Int] with [Str]")))
    -- ListComp 
    let listCompTest = check' ((Tuple "x" (TList (TCons "Int"))):Nil) (runParser "[x | 1, 2, 3]" expr_) (TList (TCons "Int"))
    logTestResult "check' listComp" (listCompTest == Right true)
    let listCompTestInvalid = check' ((Tuple "x" (TList (TCons "Int"))):Nil) (runParser "[x | 1, 2, 3]" expr_) (TList (TCons "Str"))
    logTestResult "check' listComp invalid" (listCompTestInvalid == (Left (TypeMismatch "Cannot match [Int] with [Str]")))
    -- Lambda
    let lambdaTest = check' Nil (runParser "fun x -> x + 1" program) (FunTy (TCons "Int") (TCons "Int"))
    logTestResult "check' lambda" (lambdaTest == Right true)
    let lambdaBool = check' Nil (runParser "fun x -> x > 1" program) (FunTy (TCons "Int") (TCons "Bool"))
    logTestResult "check' lambda bool" (lambdaBool == Right true)
    let lambdaTestInvalid = check' Nil (runParser "fun x -> x + 1" program) (FunTy (TCons "Int") (TCons "Str"))
    logTestResult "check' lambda invalid" (lambdaTestInvalid == (Left (TypeMismatch "Cannot match Str with Int")))
    let lambdaBoolInvalid = check' Nil (runParser "fun x -> x > 1" program) (FunTy (TCons "Int") (TCons "Int"))
    logTestResult "check' lambda bool invalid" (lambdaBoolInvalid == (Left (TypeMismatch "Cannot match Int with Bool")))


-- > synth' ((Tuple "x" (TCons "Int")):Nil) (runParser "fun x -> x > 1" program)
-- (Right (FunTy (TCons "Int") (TCons "Bool")))