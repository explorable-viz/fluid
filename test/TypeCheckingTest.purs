module TypeCheckingTest where

import Prelude

import TypeChecking (check, synth, pushVarDef, liftTypes, Context)
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


testCheck :: Effect Unit
testCheck = do 
    let intTest = check Nil (runParser "1" program) (TCons "Int")
    logTestResult "check Integer" (intTest == Right true)
    let strTest = check Nil (runParser "\"a\"" program) (TCons "Str")
    logTestResult "check String" (strTest == Right true)
    let floatTest = check Nil (runParser "2.0" program) (TCons "Float")
    logTestResult "check Float" (floatTest == Right true)
    let opTest = check Nil (runParser "(>)" program) (FunTy (FunTy (TCons "Int") (TCons "Int")) (TCons "Bool"))
    logTestResult "check operator" (opTest == Right true)
    let opTestInvalid = check Nil (runParser "(>)" program) (FunTy (FunTy (TCons "Int") (TCons "Float")) (TCons "Bool"))
    logTestResult "check operator invalid" (opTestInvalid == (Left (InvalidType "Cannot match operator '>' with type Int -> Float -> Bool")))
    let binaryAppTest = check ((Tuple "x" (TCons "Int")):Nil) (runParser "1 + x" program) (TCons "Int")
    logTestResult "check binaryApp" (binaryAppTest == Right true)
    let varTestValid = check ((Tuple "x" (TCons "Str")):Nil) (runParser "x" program) (TCons "Str")
    logTestResult "check var valid" (varTestValid == Right true)
    let varTestInvalid = check Nil (runParser "x" program) (TCons "Int")
    logTestResult "check var invalid" (varTestInvalid == (Left (LookupNil "Unbound variable found: x")))
    -- Let
    let varDefTest = check Nil (runParser "let x :: Int = 20 in x;" expr_) (TCons "Int")
    logTestResult "check let var" (varDefTest == Right true)
    let varDefEmptyList = check Nil (runParser "let [] :: [Int] = [] in [];" expr_) (TList (TCons "Int"))
    logTestResult "check let emptyList" (varDefEmptyList == Right true)
    let varDefConstr = check ((Tuple "C" (TList (TCons "Int"))):(Tuple "x" (TCons "Int")):Nil) (runParser "let C :: [Int] = [1, 2, 3] in C;" expr_) (TList (TCons "Int"))
    logTestResult "check let constr" (varDefConstr == Right true)
    let varDefList = check ((Tuple "a" (TCons "Str")):(Tuple "b" (TCons "Str")):Nil) (runParser "let [a, b] :: [Str] = [\"a\", \"b\"] in [a, b];" expr_) (TList (TCons "Str"))
    logTestResult "check let nonEmptyList" (varDefList == Right true)
    let varDefDict = check Nil (runParser "let {} :: {Str, Int} = {} in {};" expr_) (TDict (TCons "Str") (TCons "Int"))
    logTestResult "check let dict" (varDefDict == Right true)
    -- Empty List
    let emptyList = check Nil (runParser "[]" program) (TList (TCons "Str"))
    logTestResult "check empty list" (emptyList == Right true)
    let emptyListInvalid = check Nil (runParser "[]" program) (TList (TCons "IDK"))
    logTestResult "check empty list invalid" (emptyListInvalid == (Left (InvalidType "Type '[IDK]' is not accepted")))
    -- NonEmpty List
    let nonEmptyList = check Nil (runParser "[1, 2, 3]" program) (TList (TCons "Int"))
    logTestResult "check nonEmpty list" (nonEmptyList == Right true)
    -- If Else
    let ifElseValid = check ((Tuple "x" (TCons "Int")):Nil) (runParser "if x > 2 then 20 else 40" program) (TCons "Int")
    logTestResult "check if-else valid" (ifElseValid == Right true)
    let ifElseInvalid = check ((Tuple "x" (TCons "Int")):Nil) (runParser "if x then 20 else 40" program) (TCons "Int") 
    logTestResult "check if-else invalid" (ifElseInvalid == (Left (InvalidType "Condition 'x' must be of type Bool")))
    let ifElseMismatch = check Nil (runParser "if 1 > 2 then 20 else \"a\"" expr_) (TCons "Int")
    logTestResult "check if-else type mismatch" (ifElseMismatch == (Left (TypeMismatch "Cannot match 'Int' with 'Str'")))
    -- Dictionary
    let dictTestVar = check ((Tuple "a" (TCons "Str")):(Tuple "b" (TCons "Str")):Nil) (runParser "{a:1, b:2}" program) (TDict (TCons "Str") (TCons "Int"))
    logTestResult "check dictionary varKey" (dictTestVar == Right true)
    let dictTestExpr = check Nil (runParser "{ [\"a\"]: 5, [\"b\"] : 6 }" program) (TDict (TCons "Str")(TCons "Int"))
    logTestResult "check dictionary exprKey" (dictTestExpr == Right true)
    let dictTestInvalid = check Nil (runParser "{ [\"a\"]: 5, [\"b\"] : 6 }" program) (TDict (TCons "Str") (TCons "Str"))
    logTestResult "check dictionary invalid" (dictTestInvalid == (Left (TypeMismatch "Cannot match '{Str, Int}' with '{Str, Str}'")))
    -- Constructor
    let constrTest = check ((Tuple "C" (TCons "C")):Nil) (runParser "C = 1" expr_) (TCons "C")
    logTestResult "check constr" (constrTest == Right true)
    let constrTestInvalid = check Nil (runParser "C" program) (TCons "A")
    logTestResult "check constr invalid" (constrTestInvalid == (Left (TypeMismatch "Cannot match 'C' with 'A'")))
    -- ListEnum
    let enumTest = check Nil (runParser "[1 .. 2]" expr_) (TList (TCons "Int"))
    logTestResult "check listEnum" (enumTest == Right true)
    let enumTestInvalid = check Nil (runParser "[1 .. 3]" expr_) (TList (TCons "Str"))
    logTestResult "check listEnum invalid" (enumTestInvalid == (Left (TypeMismatch "Cannot match '[Int]' with '[Str]'")))
    -- ListComp 
    let listCompTest = check ((Tuple "x" (TList (TCons "Int"))):Nil) (runParser "[x | 1, 2, 3]" expr_) (TList (TCons "Int"))
    logTestResult "check listComp" (listCompTest == Right true)
    let listCompTestInvalid = check ((Tuple "x" (TList (TCons "Int"))):Nil) (runParser "[x | 1, 2, 3]" expr_) (TList (TCons "Str"))
    logTestResult "check listComp invalid" (listCompTestInvalid == (Left (TypeMismatch "Cannot match '[Int]' with '[Str]'")))
    -- Lambda
    let lambdaTest = check Nil (runParser "fun x -> x + 1" program) (FunTy (TCons "Int") (TCons "Int"))
    logTestResult "check lambda" (lambdaTest == Right true)
    let lambdaBool = check Nil (runParser "fun x -> x > 1" program) (FunTy (TCons "Int") (TCons "Bool"))
    logTestResult "check lambda bool" (lambdaBool == Right true)
    let lambdaTestInvalid = check Nil (runParser "fun x -> x + 1" program) (FunTy (TCons "Int") (TCons "Str"))
    logTestResult "check lambda invalid" (lambdaTestInvalid == (Left (TypeMismatch "Cannot match 'Str' with 'Int'")))
    let lambdaBoolInvalid = check Nil (runParser "fun x -> x > 1" program) (FunTy (TCons "Int") (TCons "Int"))
    logTestResult "check lambda bool invalid" (lambdaBoolInvalid == (Left (TypeMismatch "Cannot match 'Int' with 'Bool'")))
    -- App
    let appTest = check Nil (runParser "fun x -> x + 1" program) (FunTy (TCons "Int") (TCons "Int"))
    logTestResult "check App" (appTest == Right true)
    let appTestInvalid = check Nil (runParser "fun x -> x + 1" program) (FunTy (TCons "Int") (TCons "Bool"))
    logTestResult "check App invalid" (appTestInvalid == (Left (TypeMismatch "Cannot match 'Bool' with 'Int'")))
    -- MatchAs
    let matchAsTest = check ((Tuple ":" (TList (TCons "Int"))):(Tuple "x" (TList (TCons "Int"))):Nil) (runParser "match x as { [] -> []; x -> x };" expr_) (TList (TCons "Int"))
    logTestResult "check MatchAs" (matchAsTest == Right true)
    let matchAsCons =  check ((Tuple ":" (FunTy (FunTy (TCons "a") (TList (TCons "a"))) (TList (TCons "a")))):(Tuple "x" (TCons "a")):(Tuple "xs" (TList (TCons "a"))):Nil) (runParser "match xs as { [] -> []; x : xs -> xs };" expr_) (TList (TCons "a"))
    logTestResult "check MatchAs with Cons" (matchAsCons == Right true)
    let matchAsInvalid = check ((Tuple ":" (FunTy (FunTy (TCons "a") (TList (TCons "a"))) (TList (TCons "a")))):(Tuple "x" (TCons "a")):(Tuple "xs" (TList (TCons "a"))):Nil) (runParser "match xs as { [] -> []; x : xs -> xs };" expr_) (TList (TCons "Str"))
    logTestResult "check MatchAs invalid" (matchAsInvalid == (Left (TypeMismatch "Cannot match '[Str]' with '[a]'")))
    -- DProject
    let dProject = check ((Tuple "x" (TCons "Bool")):Nil) (runParser "x . [1 > 2]" expr_) (TCons "Bool")
    logTestResult "check dProject" (dProject == (Right true))
    let dProjectInvalid = check ((Tuple "x" (TCons "Bool")):Nil) (runParser "x . [1 > 2]" expr_) (TCons "Int")
    logTestResult "check dProject invalid" (dProjectInvalid == (Left (TypeMismatch "Cannot match 'Bool' with 'Int'")))
    let dProjectContextInvalid = check ((Tuple "x" (TCons "Str")):Nil) (runParser "x . [1 > 2]" expr_) (TCons "Bool")
    logTestResult "check dProject with invalid context" (dProjectContextInvalid == (Left (TypeMismatch "Cannot match 'Str' with 'Bool'")))


testSynth :: Effect Unit
testSynth = do 
    let synthInt = synth Nil (runParser "1" program)
    logTestResult "synth Int" (synthInt == Right (TCons "Int"))
    let synthStr = synth Nil (runParser "\"a\"" program)
    logTestResult "synth Str" (synthStr == Right (TCons "Str"))
    let synthFloat = synth Nil (runParser "2.0" program)
    logTestResult "synth Float" (synthFloat == Right (TCons "Float"))
    let opTest = synth Nil (runParser "(>)" program)
    logTestResult "synth operator" (opTest == Right (FunTy (FunTy (TCons "a") (TCons "a")) (TCons "Bool")))
    let binaryAppTest = synth ((Tuple "x" (TCons "Int")):Nil) (runParser "1 + x" program)
    logTestResult "synth binaryApp" (binaryAppTest == Right (TCons "Int"))
    let varTestValid = synth ((Tuple "x" (TCons "Str")):Nil) (runParser "x" program)
    logTestResult "synth var valid" (varTestValid == Right (TCons "Str"))
    let varTestInvalid = synth Nil (runParser "x" program)
    logTestResult "synth var invalid" (varTestInvalid == (Left (LookupNil "Unbound variable found: x")))
    -- Let
    let varDefTest = synth Nil (runParser "let x :: Int = 20 in x;" expr_)
    logTestResult "synth let var" (varDefTest == Right (TCons "Int"))
    let varDefEmptyList = synth Nil (runParser "let [] :: [Int] = [] in [];" expr_)
    logTestResult "synth let emptyList" (varDefEmptyList == Right (TList (TCons "Int")))
    let varDefConstr = synth ((Tuple "C" (TList (TCons "Int"))):(Tuple "x" (TCons "Int")):Nil) (runParser "let C :: [Int] = [1, 2, 3] in C;" expr_)
    logTestResult "synth let constr" (varDefConstr == Right (TList (TCons "Int")))
    let varDefList = synth ((Tuple "a" (TCons "Str")):(Tuple "b" (TCons "Str")):Nil) (runParser "let [a, b] :: [Str] = [\"a\", \"b\"] in [a, b];" expr_)
    logTestResult "synth let nonEmptyList" (varDefList == Right (TList (TCons "Str")))
    let varDefDict = synth Nil (runParser "let {} :: {Str, Int} = {} in {};" expr_)
    logTestResult "synth let dict" (varDefDict == Right (TDict (TCons "Str") (TCons "Int")))
    -- Empty list
    let emptyList = synth Nil (runParser "[]" program)
    logTestResult "synth empty list" (emptyList == Right (TList (TCons "")))
    -- NonEmpty List
    let nonEmptyList = synth Nil (runParser "[1, 2, 3]" program)
    logTestResult "synth nonEmpty list" (nonEmptyList == Right (TList (TCons "Int")))
    -- If Else
    let ifElseValid = synth ((Tuple "x" (TCons "Int")):Nil) (runParser "if x > 2 then 20 else 40" program)
    logTestResult "synth if-else valid" (ifElseValid == Right (TCons "Int"))
    let ifElseInvalid = synth ((Tuple "x" (TCons "Int")):Nil) (runParser "if x then 20 else 40" program)
    logTestResult "synth if-else invalid" (ifElseInvalid == (Left (InvalidType "Type of conditional 'x' must be Bool")))
    let ifElseMismatch = synth Nil (runParser "if 1 > 2 then 20 else \"a\"" expr_)
    logTestResult "synth if-else type mismatch" (ifElseMismatch == (Left (TypeMismatch "Cannot match 'Int' with 'Str'")))
    -- Dictionary
    let dictTestVar = synth ((Tuple "a" (TCons "Str")):(Tuple "b" (TCons "Str")):Nil) (runParser "{a:1, b:2}" program)
    logTestResult "synth dictionary varKey" (dictTestVar == Right (TDict (TCons "Str") (TCons "Int")))
    let dictTestExpr = synth Nil (runParser "{ [\"a\"]: 5, [\"b\"] : 6 }" program) 
    logTestResult "synth dictionary exprKey" (dictTestExpr == Right (TDict (TCons "Str")(TCons "Int")))
    let dictTestInvalid = synth Nil (runParser "{ [\"a\"]: 5, [20] : 6 }" program)
    logTestResult "synth dictionary invalid" (dictTestInvalid == (Left (TypeMismatch "Cannot match expression '{20: 6}' with type {Str, Int}")))
    -- Constructor
    let constrTest = synth ((Tuple "C" (TCons "C")):Nil) (runParser "C = 1" expr_)
    logTestResult "synth constr" (constrTest == Right (TCons "C"))
    -- ListEnum
    let enumTest = synth Nil (runParser "[1 .. 2]" expr_)
    logTestResult "synth listEnum" (enumTest == Right (TList (TCons "Int")))
    let enumTestInvalid = synth Nil (runParser "[1 .. \"3\"]" expr_)
    logTestResult "synth listEnum invalid" (enumTestInvalid == (Left (TypeMismatch "Cannot match 'Int' with 'Str'")))
    -- ListComp 
    let listCompTest = synth ((Tuple "x" (TCons "Int")):Nil) (runParser "[x | 1, 2, 3]" expr_)
    logTestResult "synth listComp" (listCompTest == Right (TList (TCons "Int")))
    let listCompTestInvalid = synth ((Tuple "x" (TCons "Int")):Nil) (runParser "[x | 1, \"2\", 3]" expr_)
    logTestResult "synth listComp invalid" (listCompTestInvalid == (Left (TypeMismatch "Cannot match 'Int' with 'Str'")))
    -- Lambda
    let lambdaTest = synth ((Tuple "x" (TCons "Int")):Nil) (runParser "fun x -> x + 1" program)
    logTestResult "synth lambda" (lambdaTest == Right (FunTy (TCons "Int") (TCons "Int")))
    let lambdaBool = synth ((Tuple "x" (TCons "Int")):Nil) (runParser "fun x -> x > 1" program)
    logTestResult "synth lambda bool" (lambdaBool == Right (FunTy (TCons "Int") (TCons "Bool")))
    let lambdaInvalid = synth Nil (runParser "fun x -> x" program)
    logTestResult "synth lambda invalid" (lambdaInvalid == (Left (LookupNil "Unbound variable: x")))
    -- App
    let appBool = synth ((Tuple "x" (TCons "Int")):Nil) (runParser "fun x -> x > 1" program)
    logTestResult "synth App" (appBool == (Right (FunTy (TCons "Int") (TCons "Bool"))))
    let appInt = synth ((Tuple "x" (TCons "Int")):Nil) (runParser "fun x -> x + 1" program) 
    logTestResult "synth App" (appInt == (Right (FunTy (TCons "Int") (TCons "Int"))))
    -- MatchAs
    let matchAsTest = synth ((Tuple ":" (TList (TCons "Int"))):(Tuple "x" (TList (TCons "Int"))):Nil) (runParser "match x as { [] -> []; x -> x };" expr_)
    logTestResult "synth MatchAs" (matchAsTest == Right (TList (TCons "Int")))
    let matchAsCons = synth ((Tuple ":" (FunTy (FunTy (TCons "a") (TList (TCons "a"))) (TList (TCons "a")))):(Tuple "x" (TCons "a")):(Tuple "xs" (TList (TCons "a"))):Nil) (runParser "match xs as { [] -> []; x : xs -> xs };" expr_)
    logTestResult "synth MatchAs Cons" (matchAsCons == (Right (TList (TCons "a"))))
    -- DProject
    let dProjectTest = synth ((Tuple "x" (TCons "Bool")):Nil) (runParser "x . [1 > 2]" expr_)
    logTestResult "synth dProject" (dProjectTest == (Right (TCons "Bool")))
    let dProjectInvalid = synth ((Tuple "x" (TCons "Int")):Nil) (runParser "x . [1 > 2]" expr_)
    logTestResult "synth dProject invalid" (dProjectInvalid == (Left (TypeMismatch "Cannot match 'Int' with 'Bool'")))
    let dProjectNoVar = synth Nil (runParser "x . [1 > 2]" expr_)
    logTestResult "synth dProject invalid variable exp" (dProjectNoVar == (Left (LookupNil "Unbound variable found: x")))
