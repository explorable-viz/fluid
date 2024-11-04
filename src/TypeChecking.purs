module TypeChecking where

import Prelude
import Data.Array (fromFoldable, singleton)
import Data.Tuple (Tuple(..))
import SExpr (Branch, Clause(..), Clauses(..), Expr(..), ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, Types(..), VarDef )
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
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
type Identifier = String
type Context = Array (Tuple Identifier Types)

check :: forall a. Context -> Expr a -> Types -> Boolean
check g (Int u n) (TCons "Int") = case synth g (Int u n) (TCons "Int") of
      Nothing -> false
      Just _ -> true
check g (Str u s) (TCons "Str") = case synth g (Str u s) (TCons "Str") of
      Nothing -> false
      Just _ -> true
check g (Float u n) (TCons "Float") = case synth g (Float u n) (TCons "Float") of
      Nothing -> false
      Just _ -> true
check g (BinaryApp e1 op e2) ty = case synth g (BinaryApp e1 op e2) ty of
      Nothing -> false
      Just _ -> true
check _ _ _ = false

synth :: forall a. Context -> Expr a -> Types -> Maybe Types
synth g (Int _ n) (TCons "Int") = Just (TCons "Int")
synth g (Str _ s) (TCons "Str") = Just (TCons "Str")
synth g (Float _ n) (TCons "Float") = Just (TCons "Float")
synth g (BinaryApp e1 op e2) ty =
      if op == "+" || op == "-" || op == "*" || op == "/" then
            do
                  -- Left and right should be the same type
                  left <- synth g e1 (TCons "Int") <|> synth g e1 (TCons "Float")
                  right <- synth g e2 (TCons "Int") <|> synth g e2 (TCons "Float")
                  case left of 
                        ty -> case right of
                              ty' -> if ty == ty' then 
                                          Just right
                                    else
                                          Nothing
                        _ -> Nothing
      else if op == ">" || op == "<" || op == ">=" || op == "<=" || op == "==" then
            -- Left and right should be the same type
            -- Return type is Bool
            do
                  left <- synth g e1 (TCons "Int") <|> synth g e1 (TCons "Float") <|> synth g e1 (TCons "Str")
                  right <- synth g e2 (TCons "Int") <|> synth g e2 (TCons "Float") <|> synth g e1 (TCons "Str")
                  case left of 
                        ty -> case right of
                              ty' -> if ty == ty' then 
                                          Just (TCons "Bool")
                                    else
                                          Nothing
                        _ -> Nothing
      else
            Nothing

synth g _ _ = Nothing



------------------------------ TESTING ---------------------------------------------------------------------
exampleCheckInt = check (singleton (Tuple "x" (TCons "Int"))) (Int unit 42) (TCons "Int")
exampleCheckString = check (singleton (Tuple "x" (TCons "Str"))) (Str unit "hello") (TCons "Str")
exampleCheckFloat = check (singleton (Tuple "x" (TCons "Float"))) (Float unit 5.0) (TCons "Float")
exampleCheckInvalid = check (singleton (Tuple "x" (TCons "Str"))) (Str unit "hello") (TCons "Int")
exampleBinaryApp = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Int unit 2) "+" (Int unit 5)) (TCons "Int")
exampleBinaryAppBool = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Int unit 2) ">" (Int unit 5)) (TCons "Bool")
exampleSynthBool = synth (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Int unit 2) ">" (Int unit 5)) (TCons "Bool")
exampleBinaryAppInvalid = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (Str unit "2") ">" (Int unit 5)) (TCons "Bool")
recursionCheck = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (BinaryApp (Int unit 2) "+" (Int unit 1)) "+" (Int unit 3)) (TCons "Int")
recursionCompCheck = check (singleton (Tuple "x" (TCons "Int"))) (BinaryApp (BinaryApp (Int unit 2) "+" (Int unit 1)) "==" (Int unit 4)) (TCons "Int")