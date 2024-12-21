module TypeChecking where

import Prelude

import Data.Array (fromFoldable, foldl, elem, findMap, concat, head)
import Data.List.NonEmpty (NonEmptyList(..), cons)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Tuple (Tuple(..), snd, fst)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, Types(..), VarDef, DictEntry(..) )
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
import Bind (Bind, Var, varAnon, (↦), keys)
import Data.List (List(..), length, sortBy, zip, zipWith, (:), (\\), nub, find, singleton)
import Util (Endo, type (×), (×), type (+), error, onlyIf)
import Data.Foldable (all)
import Data.Traversable (traverse)
import Util.Pair (Pair(..))
import Data.Semigroup
import Data.Either (Either(..))
import TypeCheckError (TypeErr(..))
import Parsing (ParseError)

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
acceptedTypes = ["Int", "Str", "Float", "Bool", ""]

isValidType :: Types -> Boolean
isValidType (TCons ty) = elem ty acceptedTypes
isValidType (TList ty) = isValidType ty
isValidType (FunTy t1 t2) = isValidType t1 && isValidType t2
isValidType (TDict ty1 ty2) = isValidType ty1 && isValidType ty2

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

check g (Dictionary u entries) (TDict keyTy valTy) = case entries of 
      (x : xs) -> case x of 
            Tuple (ExprKey expr1) expr2 -> case synth g expr1 of
                  Nothing -> false
                  Just ty1 -> case synth g expr2 of
                        Nothing -> false
                        Just ty2 -> (ty1 == keyTy) && (ty2 == valTy) && (check g (Dictionary u xs) (TDict keyTy valTy))
            Tuple (VarKey _ var) expr -> case lookup g var of
                  Nothing -> false
                  Just ty1 -> case synth g expr of
                        Nothing -> false
                        Just ty2 -> (ty1 == keyTy) && (ty2 == valTy) && (check g (Dictionary u xs) (TDict keyTy valTy))
      _ -> true


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
check g (ListEnum e1 e2) (TList ty) = case synth g e1 of
      Nothing -> false
      Just e1Synth -> case synth g e2 of
            Nothing -> false
            Just e2Synth -> e1Synth == e2Synth && e1Synth == ty

check g (ListComp u expr qualifiers) (TList ty) = 
      if (check g expr ty) then 
            case qualifiers of
                  (q : qs) -> case q of
                        ListCompGuard guardExpr -> check g guardExpr ty && check g (ListComp u expr qs) (TList ty) 
                        ListCompGen pattern genExpr -> 
                              let newContext = (checkPattern' g pattern ty) in
                              case newContext of
                                    Nothing -> false
                                    Just context -> check context genExpr ty && check context (ListComp u expr qs) (TList ty)
                        ListCompDecl varDef -> case varDef of
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
      TDict keyTy valTy -> checkRecordFields' g bindings (TDict keyTy valTy)
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

checkRecordFields' :: Context -> List (Bind Pattern) -> Types -> Maybe Context
checkRecordFields' g Nil _ = Just g
checkRecordFields' g (b : bs) (TDict keyTy valTy) = do
      g' <- checkBind' g b valTy
      checkRecordFields' g' bs (TDict keyTy valTy)
checkRecordFields' _ _ _ = Nothing

checkListPattern :: Context -> ListRestPattern -> Types -> Maybe Context
checkListPattern g (PListEnd) _ = Just g
checkListPattern g (PListNext next rest) ty = do
      case ty of
            TList ty' -> do
              g' <- checkPattern' g next ty'
              checkListPattern g' rest ty
            _ -> Nothing
checkListPattern g (PListVar var) ty = Just g


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
liftTypes (TDict ty1 ty2) = case liftTypes ty2 of
      Tuple args ret -> Tuple (ty1 : args) ret

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

synth g (Constr _ ctr exprs) = case traverse (synth g) exprs of
      Just _ -> Just (TCons ctr)
      _ -> Nothing

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
                        ListCompGuard guardExpr -> do
                              ty' <- synth g guardExpr
                              if ty == ty' then 
                                    synth g (ListComp u expr qs)
                              else 
                                    Nothing
                        ListCompGen pattern genExpr ->
                              let newContext = checkPattern' g pattern ty in 
                              case newContext of
                                    Nothing -> Nothing
                                    Just context ->
                                          if check context genExpr ty then 
                                                synth context (ListComp u expr qs)
                                          else
                                                Nothing
                        ListCompDecl varDef -> case varDef of
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

synth g (Dictionary u entries) = case entries of 
      (x : xs) -> case x of 
            Tuple (ExprKey expr1) expr2 -> case synth g expr1 of
                  Nothing -> Nothing
                  Just keyTy -> case synth g expr2 of
                        Nothing -> Nothing
                        Just valTy -> 
                              if (check g (Dictionary u xs) (TDict keyTy valTy)) then
                                    Just (TDict keyTy valTy)
                              else
                                    Nothing
            Tuple (VarKey _ var) expr -> case lookup g var of
                  Nothing -> Nothing
                  Just keyTy -> case synth g expr of
                        Nothing -> Nothing
                        Just valTy -> 
                              if (check g (Dictionary u xs) (TDict keyTy valTy)) then
                                    Just (TDict keyTy valTy)
                              else
                                    Nothing
      Nil -> Nothing

-- synth g (App exp1 exp2) =
--   -- Make sure both expressions are valid
--   case synth g exp1 of
--       Nothing -> Nothing
--       Just ty1' -> case synth g exp2 of
--             Nothing -> Nothing
--             Just ty2' -> Just (FunTy ty1' ty2')
synth _ _ = Nothing



synth' :: forall a b. Context -> Either a (Expr b) -> Either TypeErr Types
synth' _ (Left err) = Left (ParseErr "Parse error")
synth' g (Right (Int _ _)) = Right (TCons "Int")
synth' g (Right (Str _ _)) = Right (TCons "Str")
synth' g (Right (Float _ _)) = Right (TCons "Float")
synth' g (Right (BinaryApp e1 op e2)) = case synth' g (Right e1) of
      Left _ -> Left (InvalidSyntax "Cannot synthetize expression")
      Right t1 -> case synth' g (Right e2) of
            Left _ -> Left (InvalidSyntax "Cannot synthetize expression")
            Right t2 -> case operatorTypes op of
                  Just operatorTypesArray -> 
                        let 
                              checkOperatorType :: OperatorType -> Maybe Types
                              checkOperatorType { opTy, argTy } = if t1 == t2 && allEqual t1 argTy then Just opTy else Nothing
                        in
                              case findMap checkOperatorType operatorTypesArray of
                                    Nothing -> Left (InvalidType ("Operator not supported for " <> (prettyTypes t1)))
                                    Just opTy -> Right opTy
                  Nothing -> Left (InvalidSyntax (op <> " not supoprted"))
synth' g (Right (Var varName)) = case lookup g varName of
      Nothing -> Left (LookupNil ("Unbound variable found: " <> varName))
      Just t -> Right t

synth' g (Right (Let varDefs expr)) = case varDefs of
      NonEmptyList (NonEmpty (VarDef pattern ty' val) Nil) -> case check' g (Right val) ty' of
            Right true -> case checkPattern' g pattern ty' of
                  Just updatedG -> case pattern of
                        PVar varName -> 
                              let updatedG' = pushVarDef updatedG varName ty'
                              in synth' updatedG' (Right expr)
                        PListEmpty -> Right ty'
                        _ -> Right ty' 
                  Nothing -> case pattern of
                        PVar varName -> 
                              let updatedG' = pushVarDef g varName ty'
                              in synth' updatedG' (Right expr)
                        _ -> Left (InvalidType "Cannot match type of pattern")
            _ -> Left (InvalidType "Cannot match type of pattern")
      _ -> Left (InvalidSyntax "Cannot match varDef definition")
synth' g (Right (ListEmpty _)) = Right (TList (TCons ""))
synth' g (Right (ListNonEmpty _ exp rest)) = do
      headTy <- synth' g (Right exp)
      restTy <- synthRest' g rest headTy
      Right (TList (headTy))
synth' g (Right (IfElse e1 e2 e3)) =
      if ((synth' g (Right e1)) == Right (TCons "Bool")) then
            do
                  e2' <- synth' g (Right e2)
                  e3' <- synth' g (Right e3)
                  if (e2' == e3') then 
                        Right e2'
                  else
                        Left (TypeMismatch ("Cannot match " <> (prettyTypes e2') <> " with " <> (prettyTypes e3')))
      else
            Left (InvalidType ("Contifional type must be Bool"))
synth' g (Right (Constr u ctr exprs)) = case exprs of 
      (e : es) -> case synth' g (Right e) of
            Left _ -> Left (InvalidSyntax "Expression cannot be synthesized")
            Right ty -> synth' g (Right (Constr u ctr es))
      Nil -> Right (TCons ctr)
synth' g (Right (ListEnum e1 e2)) = case synth' g (Right e1) of
      Left _ -> Left (InvalidSyntax "Expression cannot be synthesized")
      Right e1Synth -> case synth' g (Right e2) of
            Left _ -> Left (InvalidSyntax "Expression cannot be synthesized")
            Right e2Synth -> if e1Synth == e2Synth then Right (TList e2Synth) else Left (TypeMismatch ("Cannot match " <> (prettyTypes e1Synth) <> " with " <> (prettyTypes e2Synth)))
synth' g (Right (ListComp u expr qualifiers)) = 
      case synth' g (Right expr) of
            Left _ -> Left (InvalidSyntax "Expression cannot be synthesized")
            Right ty -> case qualifiers of
                  (q : qs) -> case q of
                        ListCompGuard guardExpr -> do
                              ty' <- synth' g (Right guardExpr)
                              if ty == ty' then 
                                    synth' g (Right (ListComp u expr qs))
                              else 
                                    Left (TypeMismatch ("Cannot match " <> (prettyTypes ty) <> " with " <> (prettyTypes ty')))
                        ListCompGen pattern genExpr ->
                              let newContext = checkPattern' g pattern ty in 
                              case newContext of
                                    Nothing -> Left (InvalidSyntax "Pattern is invalid")
                                    Just context -> case check' context (Right genExpr) ty of
                                          Right true -> synth' context (Right (ListComp u expr qs))
                                          _ -> Left (TypeMismatch ("Cannot match expression with type " <> (prettyTypes ty)))
                        ListCompDecl varDef -> case varDef of
                              (VarDef pattern ty' val) ->
                                    let newContext = checkPattern' g pattern ty' in
                                    case newContext of
                                          Nothing -> Left (InvalidSyntax "Pattern is invalid")
                                          Just context -> case check' context (Right val) ty' of
                                                Right true -> synth' context (Right (ListComp u expr qs))
                                                _ -> Left (TypeMismatch ("Cannot match expression with type " <> (prettyTypes ty')))
                  _ -> Right (TList ty)
synth' g (Right (Dictionary u entries)) = case entries of 
      (x : xs) -> case x of 
            Tuple (ExprKey expr1) expr2 -> case synth' g (Right expr1) of
                  Left _ -> Left (InvalidSyntax "Cannot synthesize expression")
                  Right keyTy -> case synth' g (Right expr2) of
                        Left _ -> Left (InvalidSyntax "Cannot synthesize expression")
                        Right valTy -> case check' g (Right (Dictionary u xs)) (TDict keyTy valTy) of
                              Right true -> Right (TDict keyTy valTy)
                              _ -> Left (TypeMismatch ("Cannot match expression with type " <> (prettyTypes (TDict keyTy valTy))))
            Tuple (VarKey _ var) expr -> case lookup g var of
                  Nothing -> Left (LookupNil ("Unbound variable found: " <> var))
                  Just keyTy -> case synth' g (Right expr) of
                        Left _ -> Left (InvalidSyntax "Cannot synthesize expression")
                        Right valTy -> case check' g (Right (Dictionary u xs)) (TDict keyTy valTy) of
                              Right true -> Right (TDict keyTy valTy)
                              _ -> Left (TypeMismatch ("Cannot match expression with type " <> (prettyTypes (TDict keyTy valTy))))
      Nil -> Left (InvalidSyntax "Null dictionary found")
synth' _ _ = Left (InvalidSyntax "Not supported yet")


synthRest' :: forall a. Context -> ListRest a -> Types -> Either TypeErr Types
synthRest' g (End _) expectedType = Right (TList expectedType)
synthRest' g (Next _ exp rest) expectedType = do
  nextType <- synth' g (Right exp)
  if nextType == expectedType
    then synthRest' g rest expectedType
    else Left (TypeMismatch ("Cannot match " <> (prettyTypes nextType) <> " with " <> (prettyTypes expectedType)))


check' :: forall a b. Context -> Either a (Expr b) -> Types -> Either TypeErr Boolean
check' g (Left err) _ = Left (ParseErr "Parse error")
check' g (Right (Int u n)) (TCons "Int") = Right true
check' g (Right (Str u s)) (TCons "Str") = Right true
check' g (Right (Float u n)) (TCons "Float") = Right true
check' g (Right (BinaryApp e1 op e2)) ty = case synth' g (Right (BinaryApp e1 op e2)) of
      Left err -> Left err
      Right ty' -> 
            if ty' == ty then 
                  Right true
            else 
                  Left (TypeMismatch ("Cannot match " <> (prettyTypes ty) <> " with " <> (prettyTypes ty')))
check' g (Right (Var varName)) ty = case lookup g varName of
      Just t -> if isValidType t then Right (show t == show ty) else Right false
      Nothing -> Left (LookupNil ("Unbound variable found: " <> varName))
check' g (Right (Let varDefs expr)) ty = case varDefs of
      NonEmptyList (NonEmpty (VarDef pattern ty' val) Nil) -> case check' g (Right val) ty' of
            Right true -> 
                  if isValidType ty then 
                        case checkPattern' g pattern ty' of
                              Just updatedG -> case pattern of
                                    PVar varName -> 
                                          let updatedG' = pushVarDef updatedG varName ty'
                                          in check' updatedG' (Right expr) ty
                                    _ -> Right true
                              Nothing -> case pattern of
                                    PVar varName -> 
                                          let updatedG' = pushVarDef g varName ty'
                                          in check' updatedG' (Right expr) ty
                                    _ -> Right false
                  else 
                        Left (InvalidType (prettyTypes ty <> " is not a valid type"))
            Right _ -> Left (InvalidSyntax "Invalid syntax")
            Left _ -> Left (ParseErr "Parse error")
      _ -> Left (InvalidSyntax "Invalid syntax")
-- -- The empty list
check' g (Right (ListEmpty u)) (TList (TCons ty)) = 
      if elem ty acceptedTypes then 
            Right true
      else
            Left (InvalidType ("Type " <> (prettyTypes (TList (TCons ty))) <> " is not accepted"))
-- -- NonEmpty List
check' g (Right (ListNonEmpty u expr rest)) (TList ty) = case check' g (Right expr) ty of
      Right true -> Right (checkNonEmptyList rest ty)
      Right false -> case synth g expr of
            Nothing -> Left (InvalidSyntax "Invalid syntax")
            Just ty' -> Left (TypeMismatch ("Can't match " <> (prettyTypes ty') <> " with " <> (prettyTypes (TList ty))))
      _ -> Left (ParseErr "Parse error")
-- -- If else
check' g (Right (IfElse e1 e2 e3)) ty = case (check' g (Right e1) (TCons "Bool")) of
      Right true -> do 
            e2Synth <- synth' g (Right e2)
            e3Synth <- synth' g (Right e3)
            if e2Synth == e3Synth then Right true else Left (TypeMismatch ("Cannot match " <> (prettyTypes e2Synth) <> " with " <> (prettyTypes e3Synth)))
      Right false -> Left (InvalidType "Condition must be of type Bool")
      Left _ -> Left (ParseErr "Parse error")

check' g (Right (Dictionary u entries)) (TDict keyTy valTy) = case entries of 
      (x : xs) -> case x of 
            Tuple (ExprKey expr1) expr2 -> case synth' g (Right expr1) of
                  Left err -> Left err
                  Right ty1 -> case synth' g (Right expr2) of
                        Left err' -> Left err'
                        Right ty2 -> case check' g (Right (Dictionary u xs)) (TDict keyTy valTy) of
                              Left _ -> case synth' g (Right (Dictionary u xs)) of
                                    Left err -> Left err
                                    Right ty -> Left (TypeMismatch ("Cannot match " <> (prettyTypes ty) <> " with " <> (prettyTypes (TDict keyTy valTy))))
                              Right false -> case (synth' g (Right (Dictionary u xs))) of
                                    Left err'' -> Left err''
                                    Right ty' -> Left (TypeMismatch ("Can't match " <> (prettyTypes ty') <> " with " <> (prettyTypes (TDict keyTy valTy))))
                              Right true -> case ((ty1 == keyTy) && (ty2 == valTy)) of
                                    false -> Left (TypeMismatch ("Cannot match types, comparing " <> ((prettyTypes ty1) <> " with " <> (prettyTypes keyTy)) <> " and " <> ((prettyTypes ty1) <> " with " <> (prettyTypes valTy))))
                                    true -> Right true
            Tuple (VarKey _ var) expr -> case lookup g var of
                  Nothing -> Left (LookupNil ("Unbound variable found: " <> var))
                  Just ty1 -> case synth' g (Right expr) of
                        Left err -> Left err
                        Right ty1' -> case check' g (Right (Dictionary u xs)) (TDict keyTy valTy) of
                              Left _ -> case synth' g (Right (Dictionary u xs)) of
                                    Left err -> Left err
                                    Right ty -> Left (TypeMismatch ("Cannot match " <> (prettyTypes ty) <> " with " <> (prettyTypes (TDict keyTy valTy))))
                              Right false ->  case synth' g (Right (Dictionary u xs)) of
                                    Left err' -> Left err'
                                    Right ty -> Left (TypeMismatch ("Can't match " <> (prettyTypes ty) <> " with " <> (prettyTypes (TDict keyTy valTy))))  
                              Right true -> case ((ty1 == keyTy) && (ty1' == valTy)) of
                                    false -> Left (TypeMismatch ("Cannot match types, comparing " <> ((prettyTypes ty1) <> " with " <> (prettyTypes keyTy)) <> " and " <> ((prettyTypes ty1') <> " with " <> (prettyTypes valTy))))
                                    true -> Right true
      _ -> Right true

check' g (Right (Constr u ctr exprs)) (TCons ctrName) = 
      if ctr == ctrName then
            -- check the list of expressions
            case exprs of
                  Nil -> Right true
                  (x : Nil) -> case synth' g (Right x) of
                        Left err -> Left err
                        Right ty -> Right true
                  (x : xs) -> case synth' g (Right x) of
                        Left err -> Left err
                        Right ty -> Right( all(\y -> case synth g y of
                                          Just _ -> true
                                          _ -> false
                                    ) xs)
                  _ -> Right true
      else
            Left (TypeMismatch ("Cannot match " <> ctr <> " with " <> (prettyTypes (TCons ctrName))))

check' g (Right (ListEnum e1 e2)) (TList ty) = case synth' g (Right e1) of
      Left err -> Left err
      Right e1Synth -> case synth' g (Right e2) of
            Left err' -> Left err'
            Right e2Synth -> case (e1Synth == e2Synth && e1Synth == ty) of
                  true -> Right true
                  _ -> Left (TypeMismatch ("Cannot match " <> (prettyTypes (TList e2Synth)) <> " with " <> (prettyTypes (TList ty))))
check' g (Right (ListComp u expr qualifiers)) (TList ty) = case check' g (Right expr) (TList ty) of
      Left err -> Left err
      Right false -> case synth' g (Right expr) of
            Left err' -> Left err'
            Right ty' -> Left (TypeMismatch ("Cannot match " <> (prettyTypes ty') <> " with " <> (prettyTypes (TList ty))))
      Right true -> case qualifiers of
            (q : qs) -> case q of
                  ListCompGuard guardExpr -> case check' g (Right guardExpr) ty of
                        Left err' -> Left err'
                        Right false -> case synth' g (Right guardExpr) of
                              Left synthErr -> Left synthErr
                              Right ty' -> Left (TypeMismatch ("Cannot match " <> (prettyTypes ty') <> " with " <> (prettyTypes (TList ty))))
                        Right true -> check' g (Right (ListComp u expr qs)) (TList ty)
                  ListCompGen pattern genExpr -> case checkPattern' g pattern ty of
                        Nothing -> Left (TypeMismatch ("Cannot match pattern with " <> (prettyTypes ty)))
                        Just context -> case check' context (Right genExpr) ty of
                              Left err -> Left err
                              Right false -> case synth' context (Right genExpr) of
                                    Left err' -> Left err'
                                    Right synthTy -> Left (TypeMismatch ("Cannot match " <> (prettyTypes synthTy) <> " with " <> (prettyTypes ty)))
                              Right true -> check' context (Right (ListComp u expr qs)) (TList ty)
                  ListCompDecl varDef -> case varDef of
                        (VarDef pattern ty' val) -> case checkPattern' g pattern ty' of
                              Nothing -> Left (TypeMismatch ("Cannot match pattern with " <> (prettyTypes ty')))
                              Just context -> case check' context (Right val) ty' of
                                    Left err -> Left err
                                    Right false -> case synth' context (Right val) of
                                          Left err' -> Left err'
                                          Right synthTy -> Left (TypeMismatch ("Cannot match " <> (prettyTypes synthTy) <> " with " <> (prettyTypes ty')))
                                    Right true -> check' context (Right (ListComp u expr qs)) (TList ty)
                  _ -> Left (InvalidSyntax "Invalid syntax")
            Nil -> Right true
check' g (Right expr) ty = case synth' g (Right expr) of
      Left err -> Left err
      Right ty' -> 
            if ty == ty' then 
                  Right true
            else
                  Left (TypeMismatch ("Cannot match " <> (prettyTypes ty) <> " with " <> (prettyTypes ty')))


prettyTypes :: Types -> String
prettyTypes (TCons ty) = ty
prettyTypes (TList ty) = "[" <> prettyTypes ty <> "]"
prettyTypes (TDict ty1 ty2) = "{" <> prettyTypes ty1 <> ", " <> prettyTypes ty2 <> "}"
prettyTypes (FunTy ty1 ty2) = prettyTypes ty1 <> " -> " <> prettyTypes ty2 