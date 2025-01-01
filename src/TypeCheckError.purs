module TypeCheckError where

import SExpr (Types(..), Expr(..), Pattern(..), Clauses(..), DictEntry(..), Clause(..), ListRestPattern(..), ListRest(..), Qualifier(..), VarDef(..), VarDefs(..))
import Bind (Bind, Var, varAnon, (↦), keys)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList(..), cons)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Tuple (Tuple(..), snd, fst)
import Data.Show.Generic (genericShow)
import Data.Generic.Rep (class Generic)
import Util (type (+), type (×), Endo, absurd, appendList, assert, defined, definitely, definitely', error, nonEmpty, shapeMismatch, singleton, throw, unimplemented, (×), (≜))
import Prelude

data TypeErr = 
    LookupNil String
    | TypeMismatch String
    | ParseErr String
    | InvalidSyntax String
    | InvalidType String

derive instance Eq TypeErr
derive instance Generic TypeErr _
instance Show TypeErr where
   show c = genericShow c

-- data Expr a
--    | Matrix a (Expr a) (Var × Var) (Expr a)
--    | Project (Expr a) Var
--    | DProject (Expr a) (Expr a)
--    | App (Expr a) (Expr a)
--    | ListComp a (Expr a) (List (Qualifier a))      MAYBE DONE?? 
--    | LetRec (RecDefs a) (Expr a)
prettyExpr :: forall a. Expr a -> String
prettyExpr (Var var) = var
prettyExpr (Op var) = var
prettyExpr (Int a num) = show num
prettyExpr (Float a floatNum) = show floatNum
prettyExpr (Str a string) = string
prettyExpr (Constr _ ctr expressions) = 
      if ctr == ":" || ctr == "Pair" then 
            prettyExprList expressions
      else
            "(" <> ctr <> ") = " <> prettyCtrExprs expressions
      where
      prettyExprList :: forall a. List (Expr a) -> String
      prettyExprList Nil = ""
      prettyExprList (x : Nil) = prettyExpr x
      prettyExprList (x : xs) = 
            if ctr == "Pair" then 
                  "(" <> prettyExpr x <> ", " <> prettyExprList xs <> ")"
            else 
                  "(" <> prettyExpr x <> " : " <> prettyExprList xs <> ")"

      prettyCtrExprs :: forall a. List (Expr a) -> String
      prettyCtrExprs Nil = ""
      prettyCtrExprs (x : Nil) = prettyExpr x
      prettyCtrExprs (x : xs) = prettyExpr x <> " | " <> prettyCtrExprs xs

prettyExpr (Dictionary _ dictEntries) = "{" <> (prettyDictEntries dictEntries) <> "}"
      where 
      prettyDictEntries :: forall a. List (DictEntry a × Expr a) -> String
      prettyDictEntries Nil = ""
      prettyDictEntries (x : Nil) = case x of 
            (Tuple key val) -> case key of
                  ExprKey exp -> prettyExpr exp <> ": " <> prettyExpr val
                  VarKey _ var -> var <> ": " <> prettyExpr val
      prettyDictEntries (x : xs) = case x of 
            (Tuple key val) -> case key of
                  ExprKey exp -> prettyExpr exp <> ": " <> prettyExpr val <> ", " <> (prettyDictEntries xs)
                  VarKey _ var -> var <> ": " <> prettyExpr val <> "," <> (prettyDictEntries xs)
prettyExpr (BinaryApp exp1 op exp2) = prettyExpr exp1 <> " " <> op <> " " <> prettyExpr exp2 
prettyExpr (IfElse exp1 exp2 exp3) = "if " <> prettyExpr exp1 <> " then " <> prettyExpr exp2 <> " else " <> prettyExpr exp3
prettyExpr (ListEmpty _) = "[]"
prettyExpr (ListNonEmpty _ exp expressions) = "[" <> prettyExpr exp <> prettyListRest expressions
      where
      prettyListRest :: forall a. ListRest a -> String
      prettyListRest (End _) = "]"
      prettyListRest (Next _ exp rest) = ", " <> prettyExpr exp <> prettyListRest rest
prettyExpr (ListEnum exp1 exp2) = "[" <> prettyExpr exp1 <> " .. " <> prettyExpr exp2 <> "]"
prettyExpr (ListComp _ expr qualifiers) = "[" <> prettyExpr expr <> " | " <> prettyQualifiers qualifiers <> "]"
      where 
      prettyQualifiers :: forall a. List (Qualifier a) -> String
      prettyQualifiers Nil = ""
      prettyQualifiers (q : Nil) = case q of 
            ListCompGuard expr -> prettyExpr expr
            ListCompGen pattern expr -> prettyPattern pattern <> " -> " <> prettyExpr expr
            ListCompDecl varDef -> prettyVarDef varDef
      prettyQualifiers (q : qs) = case q of 
            ListCompGuard expr -> prettyExpr expr <> ", " <> prettyQualifiers qs
            ListCompGen pattern expr -> prettyPattern pattern <> " -> " <> prettyExpr expr <> ", " <> prettyQualifiers qs
            ListCompDecl varDef -> prettyVarDef varDef
prettyExpr (Lambda clauses) = "fun " <> prettyClauses clauses 
prettyExpr (MatchAs expr patternExprList) = case patternExprList of
      NonEmptyList tuples -> "match " <> (prettyExpr expr) <> " as { " <> prettyNonEmptyList tuples <> "};"
prettyExpr (Let varDefs expr) = case varDefs of
      NonEmptyList varDef -> "let " <> prettyVarDefNonEmptyList varDef <> " in " <> prettyExpr expr <> ";"
prettyExpr _ = "No"

-------------------------------------------------------------------------------------------------------------------------
prettyTuple :: forall a. Tuple Pattern (Expr a) -> String
prettyTuple (Tuple pattern expr) = (prettyPattern pattern) <> " -> " <> (prettyExpr expr)

prettyNonEmptyList :: forall a. NonEmpty List (Tuple Pattern (Expr a)) -> String
prettyNonEmptyList (NonEmpty head tail) = prettyTuple head <> "; " <> prettyTail tail
      where
      prettyTail :: forall a. List (Tuple Pattern (Expr a)) -> String
      prettyTail Nil = ""
      prettyTail (x : Nil) = prettyTuple x
      prettyTail (x : xs) = prettyTuple x <> "; " <> prettyTail xs
-------------------------------------------------------------------------------------------------------------------------
-- data VarDef a = VarDef Pattern (Types) (Expr a)
-- type VarDefs a = NonEmptyList (VarDef a)
prettyVarDef :: forall a. VarDef a -> String
prettyVarDef (VarDef pattern ty expr) = prettyPattern pattern <> " :: " <> prettyTypes ty <> " = " <> prettyExpr expr

prettyVarDefNonEmptyList :: forall a. NonEmpty List (VarDef a) -> String
prettyVarDefNonEmptyList (NonEmpty head tail) = prettyVarDef head <> prettyVarDefList tail
      where 
      prettyVarDefList :: forall a. List (VarDef a) -> String
      prettyVarDefList Nil = ""
      prettyVarDefList (x : Nil) = " " <> prettyVarDef x
      prettyVarDefList (x : xs) = " " <> prettyVarDef x <> "; " <> prettyVarDefList xs
-------------------------------------------------------------------------------------------------------------------------


prettyPattern :: Pattern -> String
prettyPattern (PVar var) = var 
prettyPattern (PConstr ctr patterns) = 
      if ctr == ":" || ctr == "Pair" then 
            prettyPatternKnownCtr patterns 
      else
            "(" <> ctr <> ")" <> prettyPatternList patterns
      where
      prettyPatternKnownCtr :: List (Pattern) -> String
      prettyPatternKnownCtr Nil = ""
      prettyPatternKnownCtr (x : Nil) = prettyPattern x
      prettyPatternKnownCtr (x : xs) = 
            if ctr == "Pair" then 
                  "(" <> prettyPattern x <> ", " <> prettyPatternKnownCtr xs <> ")"
            else
                  "(" <> prettyPattern x <> " : " <> prettyPatternKnownCtr xs <> ")"

      prettyPatternList Nil = ""
      prettyPatternList (x : Nil) = prettyPattern x
      prettyPatternList (x : xs) = prettyPattern x <> "| " <> prettyPatternList xs

prettyPattern (PRecord patternBindings) = "{" <> prettyBindings patternBindings <> "}"
      where
      prettyBindings :: List (Bind Pattern) -> String
      prettyBindings Nil = ""
      prettyBindings ((x ↦ pattern) : Nil) = x <> " ↦ " <> prettyPattern pattern
      prettyBindings ((x ↦ pattern) : xs) = x <> " ↦ " <> prettyPattern pattern <> ", " <> prettyBindings xs
prettyPattern (PListEmpty) = "[]"
prettyPattern (PListNonEmpty pattern restPatterns) = "[" <> prettyPattern pattern <> prettyListRestPattern restPatterns <> "]"
      where
      prettyListRestPattern :: ListRestPattern -> String
      prettyListRestPattern (PListVar var) = var
      prettyListRestPattern (PListEnd) = ""
      prettyListRestPattern (PListNext pattern restPatterns) = case restPatterns of
            PListEnd -> ", " <> prettyPattern pattern
            _ -> ", " <> prettyPattern pattern <> ", " <> prettyListRestPattern restPatterns
prettyPattern _ = "No"


prettyClauses :: forall a. Clauses a -> String
prettyClauses clauses = case clauses of
      (Clauses (NonEmptyList (NonEmpty (Clause (Tuple (NonEmptyList (NonEmpty pattern Nil)) expr)) Nil))) ->
            prettyPattern pattern <> " -> " <> prettyExpr expr
      _ -> "NO"

prettyTypes :: Types -> String
prettyTypes (TCons ty) = ty
prettyTypes (TList ty) = "[" <> prettyTypes ty <> "]"
prettyTypes (TDict ty1 ty2) = "{" <> prettyTypes ty1 <> ", " <> prettyTypes ty2 <> "}"
prettyTypes (FunTy ty1 ty2) = prettyTypes ty1 <> " -> " <> prettyTypes ty2 