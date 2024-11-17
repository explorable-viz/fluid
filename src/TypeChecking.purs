module TypeChecking where

import Prelude

import Data.Array (fromFoldable, singleton, foldl, find, elem, findMap)
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

-- List of accepted types
acceptedTypes :: Array String
acceptedTypes = ["Int", "Str", "Float", "Bool", "Record"]

isValidType :: Types -> Boolean
isValidType (TCons ty) = elem ty acceptedTypes
isValidType (TList ty) = isValidType ty

-- LOOKUP TABLE FOR OPERATOR TYPES
type OperatorType = {opTy :: Types, argTy :: Array Types}
operatorTypes :: Var -> Maybe (Array OperatorType)
operatorTypes op = case op of
      -- Int, Float and Str
      "+" -> Just [{ opTy: TCons "Int", argTy: [TCons "Int", TCons "Int"] }, { opTy: TCons "Float", argTy: [TCons "Float", TCons "Float"] }]
      "-" -> Just [{ opTy: TCons "Int", argTy: [TCons "Int", TCons "Int"] }, { opTy: TCons "Float", argTy: [TCons "Float", TCons "Float"] }]
      "*" -> Just [{ opTy: TCons "Int", argTy: [TCons "Int", TCons "Int"] }, { opTy: TCons "Float", argTy: [TCons "Float", TCons "Float"] }]
      "/" -> Just [{ opTy: TCons "Int", argTy: [TCons "Int", TCons "Int"] }, { opTy: TCons "Float", argTy: [TCons "Float", TCons "Float"] }]
      ">" -> Just [{ opTy: TCons "Bool", argTy: [TCons "Int", TCons "Int"] }, { opTy: TCons "Bool", argTy: [TCons "Float", TCons "Float"] }]
      ">=" -> Just [{ opTy: TCons "Bool", argTy: [TCons "Int", TCons "Int"] }, { opTy: TCons "Bool", argTy: [TCons "Float", TCons "Float"] }]
      "<=" -> Just [{ opTy: TCons "Bool", argTy: [TCons "Int", TCons "Int"] }, { opTy: TCons "Bool", argTy: [TCons "Float", TCons "Float"] }]
      "<" -> Just [{ opTy: TCons "Bool", argTy: [TCons "Int", TCons "Int"] }, { opTy: TCons "Bool", argTy: [TCons "Float", TCons "Float"] }]
      "==" -> Just [{ opTy: TCons "Bool", argTy: [TCons "Int", TCons "Int"] }, { opTy: TCons "Bool", argTy: [TCons "Float", TCons "Float"] }, { opTy: TCons "Bool", argTy: [TCons "Str", TCons "Str"] }]
      -- Undefined
      _ -> Nothing

-- Helper function to check Array
allEqual :: forall a. Eq a => a -> Array a -> Boolean
allEqual t1 arr = foldl (\acc x -> acc && (x == t1)) true arr

type Identifier = String
type Context = Array (Tuple Identifier Types)

-- data Expr a
--    | Constr a Ctr (List (Expr a))
--    | Record a (List (Bind (Expr a)))
--    | Dictionary a (List (Pair (Expr a)))
--    | Matrix a (Expr a) (Var × Var) (Expr a)
--    | Lambda (Clauses a)
--    | Project (Expr a) Var
--    | App (Expr a) (Expr a)
--    | MatchAs (Expr a) (NonEmptyList (Pattern × Expr a))
--    | ListEnum (Expr a) (Expr a)
--    | ListComp a (Expr a) (List (Qualifier a))
--    | LetRec (RecDefs a) (Expr a)

check :: forall a. Context -> Expr a -> Types -> Boolean
check g (Int u n) (TCons "Int") = true
check g (Str u s) (TCons "Str") = true
check g (Float u n) (TCons "Float") = true
check g (BinaryApp e1 op e2) ty = (synth g (BinaryApp e1 op e2)) == Just ty

-- Var needs a lookup to see if it's in the context
check g (Var varName) ty = case lookup g varName of
      Just t -> if isValidType t then show t == show ty else false
      Nothing -> false

check g (Let varDefs expr) ty = case varDefs of
      NonEmptyList (NonEmpty (VarDef pattern ty' val) Nil) -> case check g val ty' of
            true -> 
                  if isValidType ty then 
                        case (checkPattern g pattern) of
                              Just t -> case pattern of
                                    PVar varName -> 
                                          let updatedG = pushVarDef g varName ty'
                                          in check updatedG expr ty
                                    PListEmpty -> case ty' of
                                          TList _ -> true
                                          _ -> false
                                    _ -> t == ty
                              Nothing -> case pattern of
                                    PVar varName ->
                                          let updatedG = pushVarDef g varName ty'
                                          in check updatedG expr ty
                                    _ -> false
                  else
                        false
            false -> false
      _ -> false

-- The empty list
check g (ListEmpty u) (TList _) = true 
-- NonEmpty List
check g (ListNonEmpty u expr rest) (TList ty) = case check g expr ty of
      true -> checkNonEmptyList rest ty
      false -> false
-- If else
-- First expr needs to be Bool, the other 2 need to be the same type
check g (IfElse e1 e2 e3) ty = 
      if (check g e1 (TCons "Bool")) then
            (synth g e2) == (synth g e3)
      else
            false
-- Record data structures can have different types
-- check g (Record _ exprs) ty = true
check g expr ty = (synth g expr) == Just ty

checkPatternList :: Context -> List Pattern -> List Types -> Maybe Types
checkPatternList _ Nil Nil = Just (TCons "unknown")
checkPatternList g (x : xs) (t : ts) = case checkPattern g x of
  Just _ -> checkPatternList g xs ts
  Nothing -> Nothing
checkPatternList _ _ _ = Nothing

getCtrTy :: Types -> List Types
getCtrTy (TCons _) = Nil
getCtrTy (TList t) = (t : Nil)

lookup :: Context -> String -> Maybe Types
lookup g x = case find (\(Tuple n t) -> n == x) g of
      Just (Tuple _ t) -> Just t
      Nothing -> Nothing

checkListRestPattern :: Context -> ListRestPattern -> Types -> Maybe Types
checkListRestPattern g PEnd (TList t) = Just (TList t)
checkListRestPattern g (PNext pattern listRestPattern) (TList t) = do
  case (checkPattern g pattern) of
      Just ty -> checkListRestPattern g listRestPattern ty
      Nothing -> Nothing
checkListRestPattern _ _ _ = Nothing

checkBind :: Context -> Bind Pattern -> Maybe Types
checkBind g (x ↦ pattern) = do
      expectedTy <- lookup g x
      patternTy <- checkPattern g pattern
      if patternTy == expectedTy then Just (expectedTy) else Nothing

checkBindings :: Context -> List (Bind Pattern) -> Maybe Types
checkBindings _ Nil = Just (TCons "Record")
checkBindings g (b : bs) = do
      _ <- checkBind g b
      checkBindings g bs

checkPattern :: Context -> Pattern -> Maybe Types
checkPattern g (PVar x) = case lookup g x of
      Just existingType -> Just existingType
      Nothing -> Nothing
checkPattern g (PConstr ctr patterns) = do
      ctrTy <- lookup g ctr
      let argTy = getCtrTy ctrTy
      case checkPatternList g patterns argTy of
            Just _ -> Just ctrTy
            Nothing -> Nothing
checkPattern g (PRecord bindings) = checkBindings g bindings
checkPattern g (PListEmpty) = Just (TList (TCons "unknown"))
checkPattern g (PListNonEmpty head tail) = do
      case (checkPattern g head) of
            Just ty -> case checkListRestPattern g tail ty of
                  Just ty' -> if ty == ty' then Just (ty) else Nothing
                  _ -> Nothing
            _ -> Nothing

checkNonEmptyList :: forall a. ListRest a -> Types -> Boolean
checkNonEmptyList (End _) ty = true
checkNonEmptyList (Next _ nextElem rest) ty = check [] nextElem ty && checkNonEmptyList rest ty


synthRest :: forall a. Context -> ListRest a -> Types -> Maybe Types
synthRest g (End _) expectedType = Just (TList expectedType)
synthRest g (Next _ exp rest) expectedType = do
  nextType <- synth g exp
  if nextType == expectedType
    then synthRest g rest expectedType
    else Nothing


pushVarDef :: Context -> String -> Types -> Context
pushVarDef g varName varType = case lookup g varName of
      Just _ -> g -- No modification if the variable already exists
      Nothing -> g <> singleton (Tuple varName varType)

synth :: forall a. Context -> Expr a -> Maybe Types
synth g (Int _ _) = Just (TCons "Int")
synth g (Str _ _) = Just (TCons "Str")
synth g (Float _ _) = Just (TCons "Float")
synth g (BinaryApp e1 op e2) = case synth g e1 of
      Nothing -> Nothing
      Just t1 -> case synth g e2 of
            Nothing -> Nothing
            Just t2 -> case operatorTypes op of
                  Just operatorTypesArray -> 
                        let 
                              checkOperatorType :: OperatorType -> Maybe Types
                              checkOperatorType { opTy, argTy } = if t1 == t2 && allEqual t1 argTy then Just opTy else Nothing
                        in
                              findMap checkOperatorType operatorTypesArray
                  Nothing -> Nothing
synth g (Var varName) = lookup g varName
synth g (Let varDefs expr) = case varDefs of
      NonEmptyList (NonEmpty (VarDef pattern ty' val) Nil) -> case check g val ty' of
            true -> 
                  case (checkPattern g pattern) of
                        Just t -> case pattern of 
                              -- varName already exists
                              PVar varName -> if t == ty' then Just t else Nothing
                                    -- let updatedG = pushVarDef g varName ty'
                                    -- in synth updatedG expr
                              PListEmpty -> Just ty'
                              _ -> if t == ty' then Just t else Nothing
                        Nothing -> case pattern of 
                              PVar varName ->
                                    let updatedG = pushVarDef g varName ty'
                                    in synth updatedG expr
                              _ -> Nothing
            false -> Nothing
      _ -> Nothing
synth g (ListEmpty _) = Just (TList (TCons "unknown"))
synth g (ListNonEmpty _ exp rest) = do
      headTy <- synth g exp
      restTy <- synthRest g rest headTy
      Just (TList (headTy))
synth g (IfElse e1 e2 e3) =
      if ((synth g e1) == Just (TCons "Bool")) then
            do
                  e2' <- synth g e2
                  e3' <- synth g e3
                  if (e2' == e3') then 
                        Just e2'
                  else
                        Nothing
      else
            Nothing
synth _ _ = Nothing
