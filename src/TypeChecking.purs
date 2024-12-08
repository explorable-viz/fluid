module TypeChecking where

import Prelude

import Data.Array (fromFoldable, foldl, elem, findMap, concat, head)
import Data.List.NonEmpty (NonEmptyList(..), cons)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Tuple (Tuple(..), snd)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, Types(..), VarDef )
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
import Bind (Bind, Var, varAnon, (↦), keys)
import Data.List (List(..), length, sortBy, zip, zipWith, (:), (\\), nub, find, singleton)
import Util (Endo, type (×), (×), type (+), error, onlyIf)
import Data.Foldable (all)
import Data.Traversable (traverse)
import Util.Pair (Pair(..))
import Data.Semigroup

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
acceptedTypes = ["Int", "Str", "Float", "Bool", "Dictionary"]

isValidType :: Types -> Boolean
isValidType (TCons ty) = elem ty acceptedTypes
isValidType (TList ty) = isValidType ty
isValidType (FunTy t1 t2) = isValidType t1 && isValidType t2
isValidType (TRecord fields) = all (\(Tuple _ ty) -> isValidType ty) fields

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
type Context = List (Tuple Identifier Types)

-- data Expr a
--    | Matrix a (Expr a) (Var × Var) (Expr a)
--    | Lambda (Clauses a)
--    | Project (Expr a) Var
--    | App (Expr a) (Expr a)
--    | MatchAs (Expr a) (NonEmptyList (Pattern × Expr a))
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
                        case checkPattern' g pattern ty' of
                              Just updatedG -> case pattern of
                                    PVar varName -> 
                                          let updatedG' = pushVarDef updatedG varName ty'
                                          in check updatedG' expr ty
                                    _ -> true
                              Nothing -> case pattern of
                                    PVar varName -> 
                                          let updatedG' = pushVarDef g varName ty'
                                          in check updatedG' expr ty
                                    _ -> false
                  else 
                        false
            _ -> false
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

check g (Record u exprs) (TRecord fieldTypes) = 
      -- Just the names (the "a" and "b")
      if isUnique (extractFieldNames exprs) then
            -- Get the expressions
            checkRecordFieldTypes g (extractRecordExprs exprs) fieldTypes
      else
            false
      where 
      checkRecordFieldTypes :: forall a. Context -> List (Expr a) -> List (Tuple String Types) -> Boolean
      checkRecordFieldTypes g Nil Nil = true
      checkRecordFieldTypes g (expr : exprs) (t : ts) = do
            if check g expr (snd t) then 
                  checkRecordFieldTypes g exprs ts
            else
                  false
      checkRecordFieldTypes _ _ _ = false

check g (Constr u ctr exprs) (TCons ctrName) = 
      if ctr == ctrName then
            -- check the list of expressions
            case exprs of
                  Nil -> true
                  (x : Nil) -> case synth g x of
                        Just _ -> true
                        _ -> false
                  (x : xs) -> case synth g x of
                        Just _ -> 
                              all(\y -> case synth g y of
                                    Just _ -> true
                                    _ -> false
                              ) xs
                        _ -> false
                  _ -> true
      else
            false
check g (Dictionary _ exprs) (TCons "Dictionary") = 
      case traverse (\(Pair key val) -> do
            case (synth g key) of
                  Just _ -> case (synth g val) of
                        Just ty -> Just ty
                        _ -> Nothing
                  _ -> Nothing
      ) exprs of
            Just _ -> true
            Nothing -> false
check g (ListEnum e1 e2) (TList ty) = case synth g e1 of
      Nothing -> false
      Just e1Synth -> case synth g e2 of
            Nothing -> false
            Just e2Synth -> e1Synth == e2Synth && e1Synth == ty

check g (ListComp u expr qualifiers) (TList ty) = 
      if (check g expr ty) then 
            case qualifiers of
                  (q : qs) -> case q of
                        Guard guardExpr -> check g guardExpr ty && check g (ListComp u expr qs) (TList ty) 
                        Generator pattern genExpr -> 
                              let newContext = (checkPattern' g pattern ty) in
                              case newContext of
                                    Nothing -> false
                                    Just context -> check context genExpr ty && check context (ListComp u expr qs) (TList ty)
                        Declaration varDef -> case varDef of
                              (VarDef pattern ty' val) ->
                                    let newContext = (checkPattern' g pattern ty') in
                                    case newContext of
                                          Nothing -> false
                                          Just context -> check context val ty' && check context (ListComp u expr qs) (TList ty)
                        _ -> false
                  Nil -> true
      else 
            false
check g expr ty = (synth g expr) == Just ty

extractFieldNames :: forall a. List (Bind (Expr a)) -> List Var
extractFieldNames Nil = Nil
extractFieldNames (x : xs) = case x of
      (varName ↦ _) -> varName : extractFieldNames xs

extractRecordExprs :: forall a. List (Bind (Expr a)) -> List (Expr a)
extractRecordExprs Nil = Nil
extractRecordExprs (x : xs) = case x of
      (_ ↦ varExpr) -> varExpr : extractRecordExprs xs


isUnique :: List Var -> Boolean
isUnique vars = length vars == length (nub vars)

lookup :: Context -> String -> Maybe Types
lookup g x = case find (\(Tuple n t) -> n == x) g of
      Just (Tuple _ t) -> Just t
      Nothing -> Nothing

checkPatterns' :: Context -> List Types -> List Pattern -> Maybe Context
checkPatterns' g Nil Nil = Just g
checkPatterns' g Nil ((PVar var):Nil) = Just g
checkPatterns' g (arg : args) (p : ps) =
      if length (arg : args) == length (p : ps) then
            do
            g' <- checkPattern' g p arg
            g'' <- checkPatterns' g args ps
            Just (append g' g'')
      else
            Nothing
checkPatterns' _ _ _ = Nothing

checkPattern' :: Context -> Pattern -> Types -> Maybe Context
checkPattern' g (PVar x) ty = Just (singleton (Tuple x ty))
checkPattern' g (PConstr ctr patterns) ty = case (lookup g ctr) of
      Just ty' -> case liftTypes ty' of
            Tuple argTy returnTy -> do
                  if returnTy == ty then
                        checkPatterns' g argTy patterns
                  else
                        Nothing -- some error
      _ -> Nothing
checkPattern' g (PListEmpty) ty = case ty of
      TList _ -> Just g
      _ -> Nothing
checkPattern' g (PListNonEmpty head tail) ty = case ty of
      TList ty' -> do
            g' <- checkPattern' g head (TList ty')
            checkListPattern g' tail (TList ty')
      _ -> Nothing
checkPattern' g (PRecord bindings) ty = case ty of
      TRecord fieldTypes -> checkRecordFields g bindings fieldTypes
      _ -> Nothing
checkPattern' _ _ _ = Nothing

checkBind' :: Context -> Bind Pattern -> Types -> Maybe Context
checkBind' g (x ↦ pattern) ty = checkPattern' g pattern ty

checkRecordFields :: Context -> List (Bind Pattern) -> List (Tuple String Types) -> Maybe Context
checkRecordFields g Nil _ = Just g
checkRecordFields g (b : bs) ty = 
      case ty of
            (t : ts) -> do
                  g' <- checkBind' g b (snd t)
                  checkRecordFields g' bs ts
            Nil -> Nothing
checkRecordFields _ _ _ = Nothing

checkListPattern :: Context -> ListRestPattern -> Types -> Maybe Context
checkListPattern g (PEnd) _ = Just g
checkListPattern g (PNext next rest) ty = do
      case ty of
            TList ty' -> do
              g' <- checkPattern' g next ty'
              checkListPattern g' rest ty
            _ -> Nothing


-- Function: 
-- from a definition C:t0 -> (t1 -> ...(tn-1 -> tn)) recursively processes the list of patterns
-- (FunTy t1 t2) (p : ps) ---> check t1 p ... checkPat t2 ps
-- check pattern = A, this needs to be equal to tn

-- Function to extract argument type and return type
-- c = t0 -> (t1 -> (t2 -> tn))
-- fn should return ([t0, t1, t2], tn)
liftTypes :: Types -> (Tuple (List Types) Types)
liftTypes (TCons ty) = (Tuple Nil (TCons ty))
liftTypes (TList ty) = (Tuple Nil (TList ty))
liftTypes (FunTy ty1 ty2) = case liftTypes ty2 of
      Tuple args ret -> (Tuple (ty1:args) ret)
liftTypes (TRecord fields) = case fields of
      Nil -> Tuple Nil (TRecord Nil)
      (f : fs) -> case liftTypes (snd f) of
            Tuple args ret -> case liftTypes (TRecord fs) of
                  Tuple args' ret' -> Tuple (args <> args') (TRecord (f : fs))

checkNonEmptyList :: forall a. ListRest a -> Types -> Boolean
checkNonEmptyList (End _) ty = true
checkNonEmptyList (Next _ nextElem rest) ty = check Nil nextElem ty && checkNonEmptyList rest ty


synthRest :: forall a. Context -> ListRest a -> Types -> Maybe Types
synthRest g (End _) expectedType = Just (TList expectedType)
synthRest g (Next _ exp rest) expectedType = do
  nextType <- synth g exp
  if nextType == expectedType
    then synthRest g rest expectedType
    else Nothing


pushVarDef :: Context -> String -> Types -> Context
pushVarDef g varName varType = (Tuple varName varType) : g

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
            true -> case checkPattern' g pattern ty' of
                  Just updatedG -> case pattern of
                        PVar varName -> 
                              let updatedG' = pushVarDef updatedG varName ty'
                              in synth updatedG' expr
                        PListEmpty -> Just ty'
                        _ -> Just ty' 
                  Nothing -> case pattern of
                        PVar varName -> 
                              let updatedG' = pushVarDef g varName ty'
                              in synth updatedG' expr
                        _ -> Nothing
            _ -> Nothing
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

synth g (Record _ exprs) = let
      synthFields = getSynthRecord g (extractFieldNames exprs) (extractRecordExprs exprs) in
      if synthFields == Nil then 
            Nothing
      else 
            Just (TRecord synthFields)
      where 
            getSynthRecord :: forall a. Context -> List Var -> List (Expr a) -> List (Tuple String Types)
            getSynthRecord _ Nil Nil = Nil
            getSynthRecord g (n : ns) (expr : exprs) = 
                  case synth g expr of
                        Nothing -> Nil
                        Just ty -> (Tuple n ty) : getSynthRecord g ns exprs
            getSynthRecord _ _ _ = Nil


synth g (Constr _ ctr exprs) = case traverse (synth g) exprs of
      Just _ -> Just (TCons ctr)
      _ -> Nothing
synth g (Dictionary _ exprs) = 
      case traverse (\(Pair key val) -> do
            case (synth g key) of
                  Just _ -> case (synth g val) of
                        Just ty -> Just ty
                        _ -> Nothing
                  _ -> Nothing
      ) exprs of
            Just _ -> Just (TCons "Dictionary")
            Nothing -> Nothing
synth g (ListEnum e1 e2) = case synth g e1 of
      Nothing -> Nothing
      Just e1Synth -> case synth g e2 of
            Nothing -> Nothing
            Just e2Synth -> if e1Synth == e2Synth then Just (TList e2Synth) else Nothing
synth g (ListComp u expr qualifiers) = 
      case synth g expr of
            Nothing -> Nothing
            Just ty -> case qualifiers of
                  (q : qs) -> case q of
                        Guard guardExpr -> do
                              ty' <- synth g guardExpr
                              if ty == ty' then 
                                    synth g (ListComp u expr qs)
                              else 
                                    Nothing
                        Generator pattern genExpr ->
                              let newContext = checkPattern' g pattern ty in 
                              case newContext of
                                    Nothing -> Nothing
                                    Just context ->
                                          if check context genExpr ty then 
                                                synth context (ListComp u expr qs)
                                          else
                                                Nothing
                        Declaration varDef -> case varDef of
                              (VarDef pattern ty' val) ->
                                    let newContext = checkPattern' g pattern ty' in
                                    case newContext of
                                          Nothing -> Nothing
                                          Just context -> 
                                                if check context val ty' then 
                                                      synth context (ListComp u expr qs)
                                                else
                                                      Nothing
                  _ -> Just (TList ty)
synth g (App exp1 exp2) =
  -- Make sure both expressions are valid
  case synth g exp1 of
      Nothing -> Nothing
      Just ty1' -> case synth g exp2 of
            Nothing -> Nothing
            Just ty2' -> Just (FunTy ty1' ty2')
synth _ _ = Nothing
