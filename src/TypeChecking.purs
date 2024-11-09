module TypeChecking where

import Prelude

import Data.Array (fromFoldable, singleton, foldl, find)
import Data.List.NonEmpty (NonEmptyList(..), cons)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Tuple (Tuple(..))
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, Types(..), VarDef )
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
import Bind (Bind, Var, varAnon, (↦), keys)
import Data.List (List(..), length, sortBy, zip, zipWith, (:), (\\))
import Util (Endo, type (×), (×), type (+), error, onlyIf)

import Effect (Effect)
import Effect.Console (log)  -- For logging

{-
G ::= G, x : A | .
      (x : A) in G
var  --------------
      G |- x : A
      G |- e1 : A -> B     G |- e2 : A
app ------------------------------------
      G |- e1 e2 : B
      G, x : A |- e : B
abs -------------------------
      G |- \x -> e : A -> B
************************************************
G |- e <= A     check
************************************************
G, x : A |- e <= B
------------------------- abs
G |- (\x -> e) <= A -> B
G |- e => A'   A' == A
------------------------ sig
G |- e <= A
************************************************
G |- E => A     synth
************************************************
(x : A) in G
-------------- var
G |- x => A
G |- e1 => A -> B    G |- e1 <= A
---------------------------------- app
G |- e1 e2 => B
G, x : A |- e => B
-------------------------- abs
G |- (\(x : A) -> e) => B
G |- e <= A
------------------ sig
G |- (e : A) => A
-}

-- LOOKUP TABLE FOR OPERATOR TYPES
type OperatorType = {opTy :: Types, argTy :: Array Types}
operatorTypes :: Var -> Maybe OperatorType
operatorTypes op = case op of
      -- Int
      "+" -> Just { opTy: TCons "Int", argTy: [TCons "Int", TCons "Int"] }
      "-" -> Just { opTy: TCons "Int", argTy: [TCons "Int", TCons "Int"] }
      "*" -> Just { opTy: TCons "Int", argTy: [TCons "Int", TCons "Int"] }
      "/" -> Just { opTy: TCons "Int", argTy: [TCons "Int", TCons "Int"] }
      ">" -> Just { opTy: TCons "Bool", argTy: [TCons "Int", TCons "Int"] }
      ">=" -> Just { opTy: TCons "Bool", argTy: [TCons "Int", TCons "Int"] }
      "<=" -> Just { opTy: TCons "Bool", argTy: [TCons "Int", TCons "Int"] }
      "<" -> Just { opTy: TCons "Bool", argTy: [TCons "Int", TCons "Int"] }
      "==" -> Just { opTy: TCons "Bool", argTy: [TCons "Int", TCons "Int"] }
      -- Float
      "+" -> Just { opTy: TCons "Float", argTy: [TCons "Float", TCons "Float"] }
      "-" -> Just { opTy: TCons "Float", argTy: [TCons "Float", TCons "Float"] }
      "*" -> Just { opTy: TCons "Float", argTy: [TCons "Float", TCons "Float"] }
      "/" -> Just { opTy: TCons "Float", argTy: [TCons "Float", TCons "Float"] }
      ">" -> Just { opTy: TCons "Bool", argTy: [TCons "Float", TCons "Float"] }
      ">=" -> Just { opTy: TCons "Bool", argTy: [TCons "Float", TCons "Float"] }
      "<=" -> Just { opTy: TCons "Bool", argTy: [TCons "Float", TCons "Float"] }
      "<" -> Just { opTy: TCons "Bool", argTy: [TCons "Float", TCons "Float"] }
      "==" -> Just { opTy: TCons "Bool", argTy: [TCons "Float", TCons "Float"] }
      -- Str
      "==" -> Just { opTy: TCons "Bool", argTy: [TCons "Str", TCons "Str"] }
      -- Undefined
      _ -> Nothing

-- Helper function to check Array
allEqual :: forall a. Eq a => a -> Array a -> Boolean
allEqual t1 arr = foldl (\acc x -> acc && (x == t1)) true arr

type Identifier = String
type Context = Array (Tuple Identifier Types)

check :: forall a. Context -> Expr a -> Types -> Boolean
check g (Int u n) (TCons "Int") = true
check g (Str u s) (TCons "Str") = true
check g (Float u n) (TCons "Float") = true
check g (BinaryApp e1 op e2) (TCons "Int") = 
      case check g e1 (TCons "Int") of
            true -> case check g e2 (TCons "Int") of
                  true -> true
                  _ -> false
            _ -> false
check g (BinaryApp e1 op e2) (TCons "Bool") = 
      case check g e1 (TCons "Bool") of
            true -> case check g e2 (TCons "Bool") of
                  true -> true
                  _ -> false
            _ -> false
check g (Var varName) ty = case find (\(Tuple n t) -> n == varName) g of
      Just (Tuple _ t) -> show t == show ty
      Nothing -> false

check g (Let defs expr) expectedType = case defs of
      NonEmptyList (NonEmpty (VarDef (PVar varName) varType val) Nil) -> case val of
            -- Only for Integers currently
            Int _ 20 -> 
                  -- Add the PVar to the context
                  let updatedContext = pushVarDef g varName varType
                  in check updatedContext expr expectedType
            _ -> false
      _ -> false
check _ _ _ = false

pushVarDef :: Context -> String -> Types -> Context
pushVarDef g varName varType = g <> singleton (Tuple varName varType)


synth :: forall a. Context -> Expr a -> Maybe Types
synth g (Int _ _) = Just (TCons "Int")
synth g (Str _ _) = Just (TCons "Str")
synth g (Float _ _) = Just (TCons "Float")
synth g (BinaryApp e1 op e2) = 
      case synth g e1 of
            Nothing -> Nothing
            Just t1 -> case synth g e2 of
                  Nothing -> Nothing
                  Just t2 -> case operatorTypes op of
                        Just {opTy, argTy} ->  -- Corrected field order
                              if t1 == t2 && allEqual t1 argTy then
                                    Just opTy
                              else
                                    Nothing
                        Nothing -> Nothing
synth g (Var varName) = case find (\(Tuple n t) -> n == varName) g of
      Just (Tuple _ t) -> Just t
      _ -> Nothing
synth _ _ = Nothing


------------------------------ TESTING ---------------------------------------------------------------------
exampleCheckInt = check (singleton (Tuple "x" (TCons "Int"))) (Int unit 42) (TCons "Int")
exampleCheckString = check (singleton (Tuple "x" (TCons "Str"))) (Str unit "hello") (TCons "Str")
exampleCheckFloat = check (singleton (Tuple "x" (TCons "Float"))) (Float unit 5.0) (TCons "Float")
exampleCheckInvalid = check (singleton (Tuple "x" (TCons "Str"))) (Str unit "hello") (TCons "Int")
exampleBinaryApp = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Int unit 2) "+" (Int unit 5)) (TCons "Int")
exampleBinaryAppBool = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Int unit 2) ">" (Int unit 5)) (TCons "Bool")
exampleBinaryAppInvalid = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Str unit "2") ">" (Int unit 5)) (TCons "Bool")
recursionCheck = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (BinaryApp (Int unit 2) "+" (Int unit 1)) "+" (Int unit 3)) (TCons "Int")
recursionCompCheck = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (BinaryApp (Int unit 2) "+" (Int unit 1)) "==" (Int unit 4)) (TCons "Int")
exampleSynthBool = synth (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Int unit 2) "+" (Int unit 5))

context = singleton (Tuple "x" (TCons "Int"))
context'' = singleton (Tuple "y" (TCons "Str"))
resultCheck = check context (Var "x") (TCons "Int")
resultSynth = synth context (Var "x") 
resultCheckInvalid = check context (Var "y") (TCons "Int")
resultSynthInvalid = synth context'' (Var "x")

exampleLet = check (singleton (Tuple "x" (TCons "Int"))) (Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TCons "Int") (Int unit 20)) Nil)) (Var "x")) (TCons "Int")
exampleLetInvalid = check (singleton (Tuple "x" (TCons "Int"))) (Let (NonEmptyList (NonEmpty (VarDef (PVar "x") (TCons "Int") (Str unit "20")) Nil)) (Var "x")) (TCons "Int")


-- Test function to check that the context is updated
runTest :: Effect Unit
runTest = do
  -- Initialize the context as empty
  let initContext = [] :: Context
  -- First, add "x" of type "Int"
  let updatedContext1 = pushVarDef initContext "x" (TCons "Int")
  -- Log the context after first update
  log ("Context after adding x: " <> show updatedContext1) 
  -- Then, add "y" of type "Str"
  let updatedContext2 = pushVarDef updatedContext1 "y" (TCons "Str")
  -- Log the context after second update
  log ("Context after adding y: " <> show updatedContext2) 
  -- Finally, add "z" of type "Bool"
  let updatedContext3 = pushVarDef updatedContext2 "z" (TCons "Bool")
  -- Log the context after third update
  log ("Context after adding z: " <> show updatedContext3) 

