module Operator where

import Prelude hiding (join)
import Prim hiding (Type)

import Bind (Name)
import Data.Array (findIndex)
import Data.Foldable (all, and, elem, find)
import Data.List (List(..), mapMaybe, nubEq, zipWith, (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import Classes (ClassTable, comparable, fieldsAt, join, joins, subtype)
import Expr (Binop(..), Unop(..))
import Literal as L
import Parsing.Expr (Assoc(..))
import Types (Type(..), baseType)
import Types.Defs (bool, float, int, str)
import Util (type (×), definitely', whenever, (×))

data Operator = Binary Binop | Unary Unop | AndOp | OrOp | InfixOp

derive instance Eq Operator

-- Precedence levels, loosest first; infix application of a named function is Fluid-only
levels :: Array (Array Operator)
levels =
   [ [ OrOp ]
   , [ AndOp ]
   , [ Unary Not ]
   , [ Binary Eq, Binary Ne, Binary Lt, Binary Le, Binary Gt, Binary Ge, Binary In, Binary NotIn ]
   , [ InfixOp ]
   , [ Binary Add, Binary Sub ]
   , [ Binary Mul, Binary Div, Binary FloorDiv, Binary Mod ]
   , [ Unary Pos, Unary Neg ]
   , [ Binary Pow ]
   ]

assoc :: Operator -> Assoc
assoc (Binary Pow) = AssocRight
assoc _ = AssocLeft

-- Higher binds tighter
prec :: Operator -> Int
prec op = definitely' (findIndex (elem op) levels)

binopSymbol :: Binop -> String
binopSymbol Eq = "=="
binopSymbol Ne = "!="
binopSymbol Lt = "<"
binopSymbol Le = "<="
binopSymbol Gt = ">"
binopSymbol Ge = ">="
binopSymbol In = "in"
binopSymbol NotIn = "not in"
binopSymbol Add = "+"
binopSymbol Sub = "-"
binopSymbol Mul = "*"
binopSymbol Div = "/"
binopSymbol FloorDiv = "//"
binopSymbol Mod = "%"
binopSymbol Pow = "**"

unopSymbol :: Unop -> String
unopSymbol Not = "not"
unopSymbol Pos = "+"
unopSymbol Neg = "-"

-- Parameter types and result type
type Overload = List Type × Type

resolveBinop :: ClassTable -> Binop -> Type -> Type -> Maybe Type
resolveBinop classes op σ τ =
   least classes (nubEq (resolved σ τ <> resolved (baseType σ) (baseType τ)))
   where
   resolved σ' τ' = mapMaybe (\overload -> overload σ' τ') (binopOverloads classes op)

resolveUnop :: ClassTable -> Unop -> Type -> Maybe Type
resolveUnop classes op σ =
   least classes (nubEq (resolved σ <> resolved (baseType σ)))
   where
   resolved σ' = mapMaybe (\overload -> overload σ') (unopOverloads classes op)

least :: ClassTable -> List Overload -> Maybe Type
least classes os = snd <$> find (\(σs × _) -> all (\(τs × _) -> and (zipWith (subtype classes) σs τs)) os) os

binopOverloads :: ClassTable -> Binop -> List (Type -> Type -> Maybe Overload)
binopOverloads classes op = case op of
   Eq -> equality : Nil
   Ne -> equality : Nil
   In -> membership
   NotIn -> membership
   Lt -> ordering
   Le -> ordering
   Gt -> ordering
   Ge -> ordering
   Add -> arithmetic int : arithmetic float : both str str : concatList : concatTuple : Nil
   Sub -> arithmetic int : arithmetic float : Nil
   Mul -> arithmetic int : arithmetic float : repeatStr : repeatStrLeft : repeatList : repeatListLeft : Nil
   Div -> arithmetic float : Nil
   FloorDiv -> arithmetic int : arithmetic float : Nil
   Mod -> arithmetic int : arithmetic float : Nil
   Pow -> powerInt : arithmetic float : Nil
   where
   sub σ τ = subtype classes σ τ

   both bound result σ τ = whenever (sub σ bound && sub τ bound) ((bound : bound : Nil) × result)
   arithmetic τ = both τ τ
   ordering = both float bool : both str bool : Nil

   equality σ τ =
      whenever (comparable classes σ τ && equalityType classes σ && equalityType classes τ) ((σ : τ : Nil) × bool)

   membership = membershipList : membershipTuple : both str bool : membershipDict : Nil
   membershipList σ τ@(ListTy τ') = whenever (comparable classes σ τ') ((σ : τ : Nil) × bool)
   membershipList _ _ = Nothing
   membershipTuple σ τ@(TupleTy τs) = whenever (comparable classes σ (joins classes τs)) ((σ : τ : Nil) × bool)
   membershipTuple _ _ = Nothing
   membershipDict σ τ@(DictTy _) = whenever (sub σ str) ((str : τ : Nil) × bool)
   membershipDict _ _ = Nothing

   concatList σ@(ListTy σ') τ@(ListTy τ') = Just ((σ : τ : Nil) × ListTy (join classes σ' τ'))
   concatList _ _ = Nothing
   concatTuple σ@(TupleTy σs) τ@(TupleTy τs) = Just ((σ : τ : Nil) × TupleTy (σs <> τs))
   concatTuple _ _ = Nothing

   repeatStr σ τ = whenever (sub σ str && sub τ int) ((str : int : Nil) × str)
   repeatStrLeft σ τ = whenever (sub σ int && sub τ str) ((int : str : Nil) × str)
   repeatList σ@(ListTy _) τ = whenever (sub τ int) ((σ : int : Nil) × σ)
   repeatList _ _ = Nothing
   repeatListLeft σ τ@(ListTy _) = whenever (sub σ int) ((int : τ : Nil) × τ)
   repeatListLeft _ _ = Nothing

   powerInt σ τ@(LitTy (L.Int n)) = whenever (sub σ int) ((int : τ : Nil) × if n < 0 then float else int)
   powerInt _ _ = Nothing

unopOverloads :: ClassTable -> Unop -> List (Type -> Maybe Overload)
unopOverloads classes op = case op of
   Not -> sign bool : Nil
   Pos -> sign int : sign float : Nil
   Neg -> sign int : sign float : Nil
   where
   sign τ σ = whenever (subtype classes σ τ) ((τ : Nil) × τ)

equalityType :: ClassTable -> Type -> Boolean
equalityType classes = go Nil
   where
   go :: List Name -> Type -> Boolean
   go _ (CallableTy _ _) = false
   go cs (ListTy σ) = go cs σ
   go cs (DictTy σ) = go cs σ
   go cs (TupleTy τs) = all (go cs) τs
   go cs (UnionTy σ τ) = go cs σ && go cs τ
   go cs (ClassTy c τs)
      | c `elem` cs = true
      | otherwise = all (go (c : cs) <<< snd) (fieldsAt classes c τs)
   go _ _ = true
