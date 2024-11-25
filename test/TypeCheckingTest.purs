module TypeCheckingTest where

import Prelude

import TypeChecking (check, synth, pushVarDef, liftTypes, Context)
import Data.Array (fromFoldable, singleton, foldl, find, elem, findMap, length)
import Data.List.NonEmpty (NonEmptyList(..), cons)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Tuple (Tuple(..))
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, Types(..), VarDef )
import Bind (Bind, Var, varAnon, (↦), keys)
import Data.List (List(..), sortBy, zip, zipWith, (:), (\\))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

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
    logTestResult "Check Int valid" (check [] (Int unit 42) (TCons "Int"))
    logTestResult "Check Int invalid" (not (check [] (Str unit "hello") (TCons "Int")))
    -- Str
    logTestResult "Check Str valid" (check [] (Str unit "hello") (TCons "Str"))
    logTestResult "Check Str invalid" (not (check [] (Str unit "hello") (TCons "Int")))
    -- Float
    logTestResult "Check Float valid" (check [] (Float unit 5.0) (TCons "Float"))
    logTestResult "Check Float invalid" (not (check [] (Int unit 5) (TCons "Float")))
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
    let testPConstr = check [(Tuple "C" (TList (TCons "Int"))), (Tuple "x" (TCons "Int"))] (Let (NonEmptyList (NonEmpty (VarDef (PConstr "C" ((PVar "x") : Nil)) (TList (TCons "Int")) (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit))))) Nil)) (Constr unit "C" Nil)) (TList (TCons "Int"))
    logTestResult "Check Let with PConstr" testPConstr
    let testPListNonEmpty = check (singleton (Tuple "x" (TList (TCons "Int")))) ((Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TList (TCons "Int")) (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit))))) Nil)) (Var "x"))) (TList (TCons "Int"))
    logTestResult "Check Let with PListNonEmpty" testPListNonEmpty
    let testPListEmpty = check (singleton (Tuple "x" (TList (TCons "Int")))) (Let (NonEmptyList (NonEmpty (VarDef PListEmpty (TList (TCons "Int")) (ListEmpty unit)) Nil)) (Var "x")) (TList (TCons "Int"))
    logTestResult "Check Let with PListEmpty" testPListEmpty
    let testPRecord = check [(Tuple "x" (TCons "Int")), (Tuple "y" (TCons "Str")), (Tuple "z" (TCons "Record"))] (Let (NonEmptyList (NonEmpty (VarDef (PRecord (("x" ↦ PVar "x"):("y" ↦ PVar "y"):Nil)) (TCons "Record") (Var "z")) Nil)) (Var "z")) (TCons "Record")
    logTestResult "Check Let with PRecord" testPRecord
    let letInvalid = check (singleton (Tuple "x" (TCons "Int"))) (Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TCons "Int") (Str unit "20")) Nil)) (Var "x")) (TCons "Int")
    logTestResult "Check Let invalid" (not letInvalid)
    -- ListEmpty
    let listEmptyValid = check [] (ListEmpty unit) (TList (TCons "unknown"))
    logTestResult "Check ListEmpty valid" listEmptyValid
    let listEmptyInvalid = check [] (ListEmpty unit) (TCons "Int")
    logTestResult "Check ListEmpty invalid" (not listEmptyInvalid)
    -- ListNonEmpty
    let listNonEmptyValid = check [] (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit)))) (TList(TCons "Int"))
    logTestResult "Check ListNonEmpty valid" listNonEmptyValid
    let listNonEmptyInvalid = check [] (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Str unit "3") (End unit)))) (TCons "Int")
    logTestResult "Check ListNonEmpty valid" (not listNonEmptyInvalid)
    -- IfElse
    let ifElseValid = check [] (IfElse (BinaryApp (Int unit 1) ">" (Int unit 2)) (Int unit 1) (Int unit 0)) (TCons "Int")
    logTestResult "Check IfElse valid" ifElseValid
    let ifElseInvalid = check [] (IfElse (BinaryApp (Int unit 1) ">" (Int unit 2)) (Int unit 1) (Str unit "hello")) (TCons "Int")
    logTestResult "Check IfElse invalid" (not ifElseInvalid)

-- Unit tests for the synth function
testSynth :: Effect Unit
testSynth = do
    -- Int
    logTestResult "Synth Int" ((synth [] (Int unit 1)) == (Just (TCons "Int")))
    -- Str
    logTestResult "Synth Str" ((synth [] (Str unit "hello")) == (Just (TCons "Str")))
    -- Float
    logTestResult "Synth Float" ((synth [] (Float unit 1.0)) == (Just (TCons "Float")))
    -- Var
    let varValid = synth (singleton (Tuple "x" (TCons "Int"))) (Var "x")
    logTestResult "Synth Var valid" (varValid == (Just (TCons "Int")))
    let varInvalid = synth (singleton (Tuple "x" (TCons "Int"))) (Var "y")
    logTestResult "Synth Var invalid" (varInvalid == Nothing)
    -- BinaryApp
    let binaryAppValid = synth [] (BinaryApp (Int unit 1) "+" (Int unit 2))
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
    let testPConstr = synth [(Tuple "C" (TList (TCons "Int"))), (Tuple "x" (TCons "Int"))] (Let (NonEmptyList (NonEmpty (VarDef (PConstr "C" ((PVar "x") : Nil)) (TList (TCons "Int")) (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit))))) Nil)) (Constr unit "C" Nil))
    logTestResult "Synth Let with PConstr" (testPConstr == (Just (TList (TCons "Int"))))
    let testPListNonEmpty = synth (singleton (Tuple "x" (TList (TCons "Int")))) ((Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TList (TCons "Int")) (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit))))) Nil)) (Var "x")))
    logTestResult "Synth Let with PListNonEmpty" (testPListNonEmpty == (Just (TList (TCons "Int"))))
    let testPListEmpty = synth (singleton (Tuple "x" (TList (TCons "Int")))) (Let (NonEmptyList (NonEmpty (VarDef PListEmpty (TList (TCons "Int")) (ListEmpty unit)) Nil)) (Var "x"))
    logTestResult "Synth Let with PListEmpty" (testPListEmpty == (Just (TList (TCons "Int"))))
    let testPRecord = synth [(Tuple "x" (TCons "Int")), (Tuple "y" (TCons "Str")), (Tuple "z" (TCons "Record"))] (Let (NonEmptyList (NonEmpty (VarDef (PRecord (("x" ↦ PVar "x"):("y" ↦ PVar "y"):Nil)) (TCons "Record") (Var "z")) Nil)) (Var "z"))
    logTestResult "Synth Let with PRecord" (testPRecord == (Just (TCons "Record")))
    let letInvalid = synth (singleton (Tuple "x" (TCons "Int"))) (Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TCons "Int") (Str unit "20")) Nil)) (Var "x"))
    logTestResult "Synth Let invalid" (letInvalid == Nothing)
    -- ListEmpty
    let listEmptyValid = synth [] (ListEmpty unit)
    logTestResult "Synth ListEmpty valid" (listEmptyValid == (Just (TList (TCons "unknown"))))
    -- ListNonEmpty
    let listNonEmptyValid = synth [] (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Int unit 3) (End unit))))
    logTestResult "Synth ListNonEmpty valid" (listNonEmptyValid == (Just (TList(TCons "Int"))))
    let listNonEmptyInvalid = synth [] (ListNonEmpty unit (Int unit 1) (Next unit (Int unit 2) (Next unit (Str unit "3") (End unit))))
    logTestResult "Synth ListNonEmpty invalid" (listNonEmptyInvalid == Nothing)
    -- -- IfElse
    let ifElseValid = synth [] (IfElse (BinaryApp (Int unit 1) ">" (Int unit 2)) (Int unit 1) (Int unit 0))
    logTestResult "Synth IfElse valid" (ifElseValid == (Just (TCons "Int")))
    let ifElseInvalid = synth [] (IfElse (BinaryApp (Int unit 1) ">" (Int unit 2)) (Int unit 1) (Str unit "hello"))
    logTestResult "Synth IfElse invalid" (ifElseInvalid == Nothing)


-- Test function to check that the context is updated
updateContextTest :: Effect Unit
updateContextTest = do
    let initContext = [] :: Context
    log ("Initial context: " <> show initContext)
    let updatedContext1 = pushVarDef initContext "x" (TCons "Int")
    log ("Context after adding x: " <> show updatedContext1)
    let updatedContext2 = pushVarDef updatedContext1 "y" (TCons "Str")
    log ("Context after adding y: " <> show updatedContext2) 
    let updatedContext3 = pushVarDef updatedContext2 "z" (TCons "Bool")
    log ("Context after adding z: " <> show updatedContext3) 


-- Test function for lifting types
-- liftTypesTest :: Effect Unit
-- liftTypesTest = do
--     let tst = liftTypes (FunTy (TCons "t0") (FunTy (TCons "t1") (FunTy (TCons "t2") (TCons "tn"))))
--     logTestResult "Lifting types test" (tst == [(Tuple [(TCons "t0"),(TCons "t1"),(TCons "t2"),(TCons "tn")] (TCons "tn"))])