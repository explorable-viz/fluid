module TypeChecking where

import Prelude

import Data.Array (fromFoldable, foldl, elem, findMap, head)
import Data.List.NonEmpty (NonEmptyList(..), cons)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Tuple (Tuple(..), snd, fst)
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, Types(..), VarDef, DictEntry(..) )
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
import Bind (Bind, Var, varAnon, (↦), keys)
import Data.List (List(..), length, sortBy, zip, zipWith, (:), (\\), nub, find, singleton, foldM, index, foldr, concat)
import Data.List.Lazy.NonEmpty (toList)
import Util (Endo, type (×), (×), type (+), error, onlyIf)
import Data.Foldable (all)
import Data.Traversable (traverse)
import Util.Pair (Pair(..))
import Data.Semigroup
import Data.Either (Either(..))
import TypeCheckError (TypeErr(..), prettyExpr, prettyPattern, prettyTypes)
import Parsing (ParseError)
import Debug

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

------------------------------
synthPattern :: Context -> Pattern -> Either TypeErr Context
synthPattern g (PVar var) = case lookup g var of
      Just ty -> Right (singleton (Tuple var ty))
      Nothing -> Left (LookupNil ("Unbound variable: " <> var))
synthPattern g (PConstr ctr patterns) = case lookup g ctr of
      Just ctrTy -> case liftTypes ctrTy of
            Tuple argTys retTy -> case processPatterns g argTys patterns of 
                  Left err -> Left err
                  Right (Tuple listTy ty) -> Right (combineContext (listTy))
            _ -> Left (TypeMismatch ("Return type mismatch in constructor " <> show ctr))
      _ -> Left (LookupNil ("Constructor not found: " <> show ctr))
synthPattern g (PListEmpty) = Right g
synthPattern g (PListNonEmpty head tail) = case synthPattern g head of
      Left err -> Left err
      Right g' -> synthPatternList g tail
synthPattern _ _ = Left (InvalidSyntax "Unsupported")  

synthPatternList :: Context -> ListRestPattern -> Either TypeErr Context
synthPatternList g (PListEnd) = Right g
synthPatternList g (PListNext next rest) = case synthPattern g next of
      Left err -> Left err
      Right g' -> synthPatternList g' rest
synthPatternList g (PListVar var) = case lookup g var of 
      Just _ -> Right g
      Nothing -> Left (LookupNil ("Unbound variable " <> var))

processPatterns :: Context -> List Types -> List Pattern -> Either TypeErr (Tuple (List (Tuple Var Types)) Types)
processPatterns _ Nil Nil = Right (Tuple Nil (TCons ""))
processPatterns g (argTy : argTys) (p : ps) = case argTy of
    FunTy ty1 ty2 -> case checkPattern' g p ty1 of
        Just updatedG -> do
            result <- processPatterns updatedG (ty2 : argTys) ps
            let Tuple restCtx restTy = result
            Right (Tuple ((Tuple (extractVar p) ty1) : restCtx) restTy)
        Nothing -> Left (TypeMismatch ("Cannot match pattern with type " <> (prettyTypes ty1)))
    _ -> case checkPattern' g p argTy of
        Just updatedG -> do
            result <- processPatterns updatedG argTys ps
            let Tuple restCtx restTy = result
            Right (Tuple ((Tuple (extractVar p) argTy) : restCtx) restTy)
        Nothing -> Left (TypeMismatch ("Cannot match pattern with type " <> (prettyTypes argTy)))
processPatterns _ _ _ = Left (TypeMismatch "Mismatch in number of arguments")

extractVar :: Pattern -> Var
extractVar (PVar var) = var
extractVar _ = ""

combineContext :: List (Tuple Var Types) -> Context
combineContext = foldr (\(Tuple var ty) acc -> acc <> singleton (Tuple var ty)) Nil

------------------------------
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
checkNonEmptyList (Next _ nextElem rest) ty = case check Nil (Right nextElem) ty of
      Right true -> checkNonEmptyList rest ty
      _ -> false

synthRest :: forall a. Context -> ListRest a -> Types -> Maybe Types 
synthRest g (End _) expectedType = Just (TList expectedType)
synthRest g (Next _ exp rest) expectedType = case synth g (Right exp) of
      Right nextType -> if nextType == expectedType then synthRest g rest expectedType else Nothing
      Left err -> Nothing



pushVarDef :: Context -> String -> Types -> Context
pushVarDef g varName varType = (Tuple varName varType) : g

synth :: forall a b. Context -> Either a (Expr b) -> Either TypeErr Types
synth _ (Left err) = Left (ParseErr "Parse error")
synth g (Right (Int _ _)) = Right (TCons "Int")
synth g (Right (Str _ _)) = Right (TCons "Str")
synth g (Right (Float _ _)) = Right (TCons "Float")
-- OPERATOR
synth g (Right (Op operator)) = case operatorTypes operator of
      Just typesArr -> case head typesArr of
            Just { argTy, opTy } -> Right (FunTy (FunTy (TCons "a") (TCons "a")) (opTy))    --Right opTy
            _ -> Left (InvalidSyntax ("Cannot synthesize operator: " <> operator))
      _ -> Left (InvalidSyntax ("Unknown operator: " <> operator))
synth g (Right (BinaryApp e1 op e2)) = case synth g (Right e1) of
      Left _ -> Left (InvalidSyntax ("Cannot synthetize expression '" <> (prettyExpr e1) <> "'"))
      Right t1 -> case synth g (Right e2) of
            Left _ -> Left (InvalidSyntax ("Cannot synthetize expression '" <> (prettyExpr e2) <> "'"))
            Right t2 -> case operatorTypes op of
                  Just operatorTypesArray -> 
                        let 
                              checkOperatorType :: OperatorType -> Maybe Types
                              checkOperatorType { opTy, argTy } = if t1 == t2 && allEqual t1 argTy then Just opTy else Nothing
                        in
                              case findMap checkOperatorType operatorTypesArray of
                                    Nothing -> Left (InvalidType ("Operator not supported for: " <> (prettyTypes t1)))
                                    Just opTy -> Right opTy
                  Nothing -> Left (InvalidSyntax ("'" <> op <> "' not supoprted"))
synth g (Right (Var varName)) = case lookup g varName of
      Nothing -> Left (LookupNil ("Unbound variable found: " <> varName))
      Just t -> Right t
-- Let 
synth g (Right (Let varDefs expr)) = case varDefs of
      NonEmptyList (NonEmpty (VarDef pattern ty' val) Nil) -> case check g (Right val) ty' of
            Right true -> case checkPattern' g pattern ty' of
                  Just updatedG -> case pattern of
                        PVar varName -> 
                              let updatedG' = pushVarDef updatedG varName ty'
                              in synth updatedG' (Right expr)
                        PListEmpty -> Right ty'
                        _ -> Right ty' 
                  Nothing -> case pattern of
                        PVar varName -> 
                              let updatedG' = pushVarDef g varName ty'
                              in synth updatedG' (Right expr)
                        _ -> Left (InvalidType ("Cannot match type of pattern: " <> (prettyPattern pattern)))
            _ -> Left (InvalidType ("Cannot match type of pattern: " <> (prettyPattern pattern)))
      _ -> Left (InvalidSyntax "Cannot match varDef definition")
-- Empty List
synth g (Right (ListEmpty _)) = Right (TList (TCons ""))
-- NonEmpty List
synth g (Right (ListNonEmpty _ exp rest)) = do
      headTy <- synth g (Right exp)
      restTy <- synthRest' g rest headTy
      Right (TList (headTy))
-- If Else
synth g (Right (IfElse e1 e2 e3)) =
      if ((synth g (Right e1)) == Right (TCons "Bool")) then
            do
                  e2' <- synth g (Right e2)
                  e3' <- synth g (Right e3)
                  if (e2' == e3') then 
                        Right e2'
                  else
                        Left (TypeMismatch ("Cannot match '" <> (prettyTypes e2') <> "' with '" <> (prettyTypes e3') <> "'"))
      else
            Left (InvalidType ("Type of conditional '" <> (prettyExpr e1) <> "' must be Bool"))
-- Constructor
synth g (Right (Constr u ctr exprs)) = case exprs of 
      (e : es) -> case synth g (Right e) of
            Left _ -> Left (InvalidSyntax ("Expression '" <> (prettyExpr e) <> "' cannot be synthesized"))
            Right ty -> synth g (Right (Constr u ctr es))
      Nil -> case lookup g ctr of
            Just ty -> Right ty
            _ -> Right (TCons ctr)
-- List Enum
synth g (Right (ListEnum e1 e2)) = case synth g (Right e1) of
      Left _ -> Left (InvalidSyntax ("Expression '" <> (prettyExpr e1) <> "' cannot be synthesized"))
      Right e1Synth -> case synth g (Right e2) of
            Left _ -> Left (InvalidSyntax ("Expression '" <> (prettyExpr e2) <> "' cannot be synthesized"))
            Right e2Synth -> if e1Synth == e2Synth then Right (TList e2Synth) else Left (TypeMismatch ("Cannot match '" <> (prettyTypes e1Synth) <> "' with '" <> (prettyTypes e2Synth) <> "'"))
-- List Comp
synth g (Right (ListComp u expr qualifiers)) = 
      case synth g (Right expr) of
            Left _ -> Left (InvalidSyntax ("Expression '" <> (prettyExpr expr) <> "' cannot be synthesized"))
            Right ty -> case qualifiers of
                  (q : qs) -> case q of
                        ListCompGuard guardExpr -> do
                              ty' <- synth g (Right guardExpr)
                              if ty == ty' then 
                                    synth g (Right (ListComp u expr qs))
                              else 
                                    Left (TypeMismatch ("Cannot match '" <> (prettyTypes ty) <> "' with '" <> (prettyTypes ty') <> "'"))
                        ListCompGen pattern genExpr ->
                              let newContext = checkPattern' g pattern ty in 
                              case newContext of
                                    Nothing -> Left (InvalidSyntax ("Pattern '" <> (prettyPattern pattern) <> "' is invalid"))
                                    Just context -> case check context (Right genExpr) ty of
                                          Right true -> synth context (Right (ListComp u expr qs))
                                          _ -> Left (TypeMismatch ("Cannot match expression '" <> (prettyExpr expr) <> "' with type " <> (prettyTypes ty)))
                        ListCompDecl varDef -> case varDef of
                              (VarDef pattern ty' val) ->
                                    let newContext = checkPattern' g pattern ty' in
                                    case newContext of
                                          Nothing -> Left (InvalidSyntax ("Pattern '" <> (prettyPattern pattern) <> "' is invalid"))
                                          Just context -> case check context (Right val) ty' of
                                                Right true -> synth context (Right (ListComp u expr qs))
                                                _ -> Left (TypeMismatch ("Cannot match expression '" <> (prettyExpr expr) <> "' with type " <> (prettyTypes ty')))
                  _ -> Right (TList ty)
-- Dictionary
synth g (Right (Dictionary u entries)) = case entries of 
      (x : xs) -> case x of 
            Tuple (ExprKey expr1) expr2 -> case synth g (Right expr1) of
                  Left _ -> Left (InvalidSyntax ("Cannot synthesize expression '" <> (prettyExpr expr1) <> "'"))
                  Right keyTy -> case synth g (Right expr2) of
                        Left _ -> Left (InvalidSyntax ("Cannot synthesize expression '" <> (prettyExpr expr2) <> "'"))
                        Right valTy -> case check g (Right (Dictionary u xs)) (TDict keyTy valTy) of
                              Right true -> Right (TDict keyTy valTy)
                              _ -> Left (TypeMismatch ("Cannot match expression '" <> (prettyExpr (Dictionary u xs)) <> "' with type " <> (prettyTypes (TDict keyTy valTy))))
            Tuple (VarKey _ var) expr -> case lookup g var of
                  Nothing -> Left (LookupNil ("Unbound variable found: " <> var))
                  Just keyTy -> case synth g (Right expr) of
                        Left _ -> Left (InvalidSyntax ("Cannot synthesize expression '" <> (prettyExpr expr) <> "'"))
                        Right valTy -> case check g (Right (Dictionary u xs)) (TDict keyTy valTy) of
                              Right true -> Right (TDict keyTy valTy)
                              _ -> Left (TypeMismatch ("Cannot match expression '" <> (prettyExpr (Dictionary u xs)) <> "' with type " <> (prettyTypes (TDict keyTy valTy))))
      Nil -> Left (InvalidSyntax "Null dictionary found")
-- Lambda
synth g (Right (Lambda clauses)) = case clauses of 
      (Clauses (NonEmptyList (NonEmpty (Clause (Tuple (NonEmptyList (NonEmpty pattern Nil)) expr)) Nil))) -> case synthPattern g pattern of
            Left err -> Left err
            Right updatedG -> case synth updatedG (Right expr) of
                  Left err -> Left err
                  Right ty' -> case index updatedG 0 of 
                        Just elem -> Right (FunTy (snd elem) ty')
                        _ -> Left (TypeMismatch ("Expression '" <> (prettyExpr expr) <> "' does not match type " <> (prettyTypes ty')))
      _ -> Left (InvalidSyntax "Clauses structure is invalid")

-- App
synth g (Right (App exp1 exp2)) = case synth g (Right exp1) of
      Left err -> Left err
      Right ty1 -> case synth g (Right exp2) of 
            Left err -> Left err
            Right ty2 -> Right (FunTy ty1 ty2)

-- MatchAs
synth g (Right (MatchAs expr patterns)) = case synth g (Right expr) of
      Left err -> Left err
      Right exprTy -> case patterns of 
            NonEmptyList list -> case matchAsList g list of 
                  Left err -> Left err
                  Right _ -> Right exprTy
            _ -> Left (InvalidSyntax ("Syntax invalid for patterns, must be NonEmptyList"))
-- DProject
synth g (Right (DProject exp1 exp2)) = case synth g (Right exp1) of
      Left err -> Left err
      Right ty -> case synth g (Right exp2) of
            Left err -> Left err
            Right ty' ->
                  if ty == ty' then 
                        Right ty
                  else
                        Left (TypeMismatch ("Cannot match '" <> (prettyTypes ty) <> "' with '" <> (prettyTypes ty') <> "'"))
-- Other
synth _ _ = Left (InvalidSyntax "Not supported yet")


synthRest' :: forall a. Context -> ListRest a -> Types -> Either TypeErr Types
synthRest' g (End _) expectedType = Right (TList expectedType)
synthRest' g (Next _ exp rest) expectedType = do
  nextType <- synth g (Right exp)
  if nextType == expectedType
    then synthRest' g rest expectedType
    else Left (TypeMismatch ("Cannot match '" <> (prettyTypes nextType) <> "' with '" <> (prettyTypes expectedType) <> "'"))


check :: forall a b. Context -> Either a (Expr b) -> Types -> Either TypeErr Boolean
check g (Left err) _ = Left (ParseErr "Parse error")
check g (Right (Int u n)) (TCons "Int") = Right true
check g (Right (Str u s)) (TCons "Str") = Right true
check g (Right (Float u n)) (TCons "Float") = Right true
-- OPERATOR
check g (Right (Op operator)) (FunTy (FunTy argTy1 argTy2) ty) = case operatorTypes operator of
      Just typesArr -> 
            let
                  checkOperatorTy :: OperatorType -> Maybe Types
                  checkOperatorTy { opTy, argTy } = if argTy1 == argTy2 && allEqual argTy1 argTy then Just opTy else Nothing
            in 
                  case findMap checkOperatorTy typesArr of
                        Just opTy -> Right true
                        _ -> Left (InvalidType ("Cannot match operator '" <> operator <> "' with type " <> (prettyTypes (FunTy (FunTy argTy1 argTy2) ty)))) 
      _ -> Left (InvalidType ("Cannot match operator '" <> operator <> "' with type " <> (prettyTypes (FunTy (FunTy argTy1 argTy2) ty)))) 

check g (Right (BinaryApp e1 op e2)) ty = case synth g (Right (BinaryApp e1 op e2)) of
      Left err -> Left err
      Right ty' -> 
            if ty' == ty then 
                  Right true
            else 
                  Left (TypeMismatch ("Cannot match '" <> (prettyTypes ty) <> "' with '" <> (prettyTypes ty') <> "'"))
check g (Right (Var varName)) ty = case lookup g varName of
      Just t -> 
            if isValidType t then 
                  if show t == show ty then Right true else Left (TypeMismatch ("Cannot match '" <> prettyTypes t <> "' with '" <> prettyTypes ty <> "'"))
            else 
                  Left (InvalidType ("'" <> prettyTypes t <> "' is not a valid type"))
      Nothing -> Left (LookupNil ("Unbound variable found: " <> varName))
check g (Right (Let varDefs expr)) ty = case varDefs of
      NonEmptyList (NonEmpty (VarDef pattern ty' val) Nil) -> case check g (Right val) ty' of
            Right true -> 
                  if isValidType ty then 
                        case checkPattern' g pattern ty' of
                              Just updatedG -> case pattern of
                                    PVar varName -> 
                                          let updatedG' = pushVarDef updatedG varName ty'
                                          in check updatedG' (Right expr) ty
                                    _ -> Right true
                              Nothing -> case pattern of
                                    PVar varName -> 
                                          let updatedG' = pushVarDef g varName ty'
                                          in check updatedG' (Right expr) ty
                                    _ -> Left (InvalidType ("Cannot match pattern '" <> prettyPattern pattern <> "' with type '" <> prettyTypes ty' <> "'"))
                  else 
                        Left (InvalidType (prettyTypes ty <> " is not a valid type"))
            Right _ -> Left (InvalidType ("Cannot match expression '" <> (prettyExpr val) <> "' with type " <> (prettyTypes ty')))
            Left err -> Left err
      _ -> Left (InvalidSyntax "Invalid syntax, varDef must be a NonEmptyList")
-- -- The empty list
check g (Right (ListEmpty u)) (TList (TCons ty)) = 
      if elem ty acceptedTypes then 
            Right true
      else
            Left (InvalidType ("Type '" <> (prettyTypes (TList (TCons ty))) <> "' is not accepted"))
-- -- NonEmpty List
check g (Right (ListNonEmpty u expr rest)) (TList ty) = case check g (Right expr) ty of
      Right true -> Right (checkNonEmptyList rest ty)
      _ -> case synth g (Right expr) of
            Left err -> Left err
            Right ty' -> Left (TypeMismatch ("Can't match '" <> (prettyTypes ty') <> "' with '" <> (prettyTypes (TList ty)) <> "'"))
-- -- If else
check g (Right (IfElse e1 e2 e3)) ty = case (check g (Right e1) (TCons "Bool")) of
      Right true -> do 
            e2Synth <- synth g (Right e2)
            e3Synth <- synth g (Right e3)
            if e2Synth == e3Synth then Right true else Left (TypeMismatch ("Cannot match '" <> (prettyTypes e2Synth) <> "' with '" <> (prettyTypes e3Synth) <> "'"))
      _ -> Left (InvalidType ("Condition '" <> (prettyExpr e1) <> "' must be of type Bool"))
-- Dictionary
check g (Right (Dictionary u entries)) (TDict keyTy valTy) = case entries of 
      (x : xs) -> case x of 
            Tuple (ExprKey expr1) expr2 -> case synth g (Right expr1) of
                  Left err -> Left err
                  Right ty1 -> case synth g (Right expr2) of
                        Left err' -> Left err'
                        Right ty2 -> case check g (Right (Dictionary u xs)) (TDict keyTy valTy) of
                              Right true -> case ((ty1 == keyTy) && (ty2 == valTy)) of
                                    false -> Left (TypeMismatch ("Cannot match types, comparing '" <> ((prettyTypes ty1) <> "' with '" <> (prettyTypes keyTy)) <> "' and '" <> ((prettyTypes ty1) <> "' with '" <> (prettyTypes valTy) <> "'")))
                                    true -> Right true
                              _ -> case synth g (Right (Dictionary u xs)) of
                                    Left err -> Left err
                                    Right ty -> Left (TypeMismatch ("Cannot match '" <> (prettyTypes ty) <> "' with '" <> (prettyTypes (TDict keyTy valTy)) <> "'"))
            Tuple (VarKey _ var) expr -> case lookup g var of
                  Nothing -> Left (LookupNil ("Unbound variable found: " <> var))
                  Just ty1 -> case synth g (Right expr) of
                        Left err -> Left err
                        Right ty1' -> case check g (Right (Dictionary u xs)) (TDict keyTy valTy) of
                              Right true -> case ((ty1 == keyTy) && (ty1' == valTy)) of
                                    false -> Left (TypeMismatch ("Cannot match types, comparing '" <> ((prettyTypes ty1) <> "' with '" <> (prettyTypes keyTy)) <> "' and '" <> ((prettyTypes ty1') <> "' with '" <> (prettyTypes valTy) <> "'")))
                                    true -> Right true
                              _ -> case synth g (Right (Dictionary u xs)) of
                                    Left err -> Left err
                                    Right ty -> Left (TypeMismatch ("Cannot match '" <> (prettyTypes ty) <> "' with '" <> (prettyTypes (TDict keyTy valTy)) <> "'"))  
      _ -> Right true
-- Constructor
check g (Right (Constr u ctr exprs)) (TCons ctrName) = 
      if ctr == ctrName then
            -- check the list of expressions
            case exprs of
                  Nil -> Right true
                  (x : Nil) -> case synth g (Right x) of
                        Left err -> Left err
                        Right ty -> Right true
                  (x : xs) -> case synth g (Right x) of
                        Left err -> Left err
                        Right ty -> Right( all(\y -> case synth g (Right y) of
                                          Right _ -> true
                                          _ -> false
                                    ) xs)
                  _ -> Right true
      else
            Left (TypeMismatch ("Cannot match '" <> ctr <> "' with '" <> (prettyTypes (TCons ctrName)) <> "'"))
-- List Enum
check g (Right (ListEnum e1 e2)) (TList ty) = case synth g (Right e1) of
      Left err -> Left err
      Right e1Synth -> case synth g (Right e2) of
            Left err' -> Left err'
            Right e2Synth -> case (e1Synth == e2Synth && e1Synth == ty) of
                  true -> Right true
                  _ -> Left (TypeMismatch ("Cannot match '" <> (prettyTypes (TList e2Synth)) <> "' with '" <> (prettyTypes (TList ty)) <> "'"))
-- List Comp
check g (Right (ListComp u expr qualifiers)) (TList ty) = case check g (Right expr) (TList ty) of
      Right true -> case qualifiers of
            (q : qs) -> case q of
                  ListCompGuard guardExpr -> case check g (Right guardExpr) ty of
                        Right true -> check g (Right (ListComp u expr qs)) (TList ty)
                        _-> case synth g (Right guardExpr) of
                              Left synthErr -> Left synthErr
                              Right ty' -> Left (TypeMismatch ("Cannot match '" <> (prettyTypes ty') <> "' with '" <> (prettyTypes (TList ty)) <> "'"))
                  ListCompGen pattern genExpr -> case checkPattern' g pattern ty of
                        Nothing -> Left (TypeMismatch ("Cannot match pattern '" <> (prettyPattern pattern) <> "' with '" <> (prettyTypes ty) <> "'"))
                        Just context -> case check context (Right genExpr) ty of
                              Right true -> check context (Right (ListComp u expr qs)) (TList ty)
                              _ -> case synth context (Right genExpr) of
                                    Left err' -> Left err'
                                    Right synthTy -> Left (TypeMismatch ("Cannot match '" <> (prettyTypes synthTy) <> "' with '" <> (prettyTypes ty) <> "'"))
                              
                  ListCompDecl varDef -> case varDef of
                        (VarDef pattern ty' val) -> case checkPattern' g pattern ty' of
                              Nothing -> Left (TypeMismatch ("Cannot match pattern '" <> (prettyPattern pattern) <> "' with '" <> (prettyTypes ty') <> "'"))
                              Just context -> case check context (Right val) ty' of
                                    Right true -> check context (Right (ListComp u expr qs)) (TList ty)
                                    _ -> case synth context (Right val) of
                                          Left err' -> Left err'
                                          Right synthTy -> Left (TypeMismatch ("Cannot match '" <> (prettyTypes synthTy) <> "' with '" <> (prettyTypes ty') <> "'"))
                                    Right true -> check context (Right (ListComp u expr qs)) (TList ty)
                  _ -> Left (InvalidSyntax "Invalid syntax")
            Nil -> Right true
      _ -> case synth g (Right expr) of
            Left err' -> Left err'
            Right ty' -> Left (TypeMismatch ("Cannot match '" <> (prettyTypes ty') <> "' with '" <> (prettyTypes (TList ty)) <> "'"))
-- Lambda
check g (Right (Lambda clauses)) (FunTy argTy retTy) = case clauses of
      (Clauses (NonEmptyList (NonEmpty (Clause (Tuple (NonEmptyList (NonEmpty pattern Nil)) expr)) Nil))) -> case checkPattern' g pattern argTy of
            Just context -> check context (Right expr) retTy
            Nothing -> Left (InvalidSyntax "Pattern structure is invalid")
      _ -> Left (InvalidSyntax "Clauses structure is invalid")
-- App
-- Right (App (Op "+") (Int unit 1))  ---> FunTy (TCons "Int") (TCons "Int")
check g (Right (App exp1 exp2)) (FunTy ty1 ty2) = case check g (Right exp1) ty2 of
      Right true -> case check g (Right exp2) ty1 of
            Right true -> Right true
            _ -> Left (InvalidType ("Cannot match expression '" <> (prettyExpr exp2) <> "' with '" <> (prettyTypes ty1) <> "'"))
      _ -> Left (InvalidType ("Cannot match expression '" <> (prettyExpr exp1) <> "' with '" <> (prettyTypes ty2) <> "'"))
-- MatchAs
check g (Right (MatchAs expr patterns)) ty = case synth g (Right expr) of
      Left err -> Left err
      Right exprTy -> 
            if ty == exprTy then 
                  case patterns of
                        NonEmptyList list -> case matchAsList g list of
                              Left err -> Left err
                              Right _ -> Right true
                        _ -> Left (InvalidSyntax "Syntax invalid for patterns, must be NonEmptyList")
            else
                  Left (TypeMismatch ("Cannot match '" <> (prettyTypes ty) <> "' with '" <> (prettyTypes exprTy) <> "'"))
-- Matrix
check g (Right (Matrix u expr1 varPair expr2)) (TList ty) = case check g (Right expr1) ty of
      Left err -> Left err 
      _ -> case check g (Right expr2) ty of 
            Left err -> Left err
            _ -> Right true
-- DProject
check g (Right (DProject exp1 exp2)) ty = case check g (Right exp1) ty of
      Left err -> Left err
      _ -> case check g (Right exp2) ty of
            Left err -> Left err
            _ -> Right true
-- Others
check g (Right expr) ty = case synth g (Right expr) of
      Left err -> Left err
      Right ty' -> 
            if ty == ty' then 
                  Right true
            else
                  Left (TypeMismatch ("Cannot match '" <> (prettyTypes ty) <> "' with '" <> (prettyTypes ty') <> "'"))

-- data Expr a
--    | Matrix a (Expr a) (Var × Var) (Expr a)                    
--    | Project (Expr a) Var                                      
--    | DProject (Expr a) (Expr a)
--    | MatchAs (Expr a) (NonEmptyList (Pattern × Expr a))        Done in check, Not sure if this is correct though
--    | LetRec (RecDefs a) (Expr a)

matchAsTuple :: forall a. Context -> Tuple Pattern (Expr a) -> Either TypeErr Boolean
matchAsTuple g (Tuple pattern expr) = case synthPattern g pattern of
      Left err -> Left err
      Right g' -> case synth g' (Right expr) of 
            Left err -> Left err
            Right ty' -> Right true
matchAsTuple _ _ = Left (InvalidSyntax "Unsupported")

matchAsTail :: forall a. Context -> List (Tuple Pattern (Expr a)) -> Either TypeErr Boolean
matchAsTail g Nil = Right true
matchAsTail g (x : Nil) = matchAsTuple g x
matchAsTail g (x : xs) = case matchAsTuple g x of
      Left err -> Left err
      Right _ -> matchAsTail g xs

matchAsList :: forall a. Context -> NonEmpty List (Tuple Pattern (Expr a)) -> Either TypeErr Boolean
matchAsList g (NonEmpty head tail) = case matchAsTuple g head of
      Left err -> Left err
      Right _ -> case matchAsTail g tail of
            Left err -> Left err 
            Right _ -> Right true


-- > runParser "x . [var]" expr_
-- (Right (DProject (Var "x") (Var "var")))