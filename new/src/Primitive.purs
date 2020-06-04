module Primitive where

import Prelude hiding (apply)
import Bindings (Var, ε, (:+:), (↦))
import Selected (Selected, (∧))
import Val (Binary(..), BinaryOp(..), Env, RawVal(..), Unary(..), UnaryOp(..), Val(..), val)
import Data.Foldable (foldl)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Util (error)

-- name in user land and precedence 0 to 9, similar to Haskell 98
data OpName = OpName Var Int

opPrec :: OpName -> Int
opPrec (OpName _ prec) = prec

opName :: String -> Int -> Tuple String OpName
opName op = OpName op >>> Tuple op

-- Syntactic information only. No guarantee that any of these will be defined.
opNames :: Map String OpName
opNames = fromFoldable [
   opName "*" 7,
   opName "+" 6,
   opName "-" 6,
   opName "==" 4,
   opName "/=" 4,
   opName "<" 4,
   opName ">" 4,
   opName "<=" 4,
   opName ">=" 4
]

-- Enforce argument type requirements.
class To a where
   to :: Val -> a

class From a where
   from :: a -> Val

instance toInt :: To Int where
   to (Val _ (Int n)) = n
   to _ = error "Integer expected"

instance fromInt :: From Int where
   from = Int >>> val

instance fromBoolean :: From Boolean where
   from b = val $ if b then True else False

instance fromString :: From String where
   from = Str >>> val

applyBinary :: BinaryOp -> Val -> Val -> Val
applyBinary (BinaryOp _ (IntIntInt f)) v v' = from $ f (to v) (to v')
applyBinary (BinaryOp _ (IntIntBool f)) v v' = from $ f (to v) (to v')

applyBinary_fwd :: BinaryOp -> Selected -> Val -> Val -> Val
applyBinary_fwd op α v@(Val α1 _) v'@(Val α2 _) =
   Val (α ∧ α1 ∧ α2) u where Val _ u = applyBinary op v v'

applyUnary :: UnaryOp -> Val -> Val
applyUnary (UnaryOp _ (IntStr f)) = to >>> f >>> from
applyUnary (PartialApp φ v) = applyBinary φ v

applyUnary_fwd :: UnaryOp -> Selected -> Val -> Val
applyUnary_fwd op α v@(Val α' _) =
   Val (α ∧ α') u where Val _ u = applyUnary op v

intStr :: String -> (Int -> String) -> Val
intStr name = IntStr >>> UnaryOp name >>> Unary >>> val

intIntBool :: String -> (Int -> Int -> Boolean) -> Val
intIntBool name = IntIntBool >>> BinaryOp name >>> Binary >>> val

intIntInt :: String -> (Int -> Int -> Int) -> Val
intIntInt name = IntIntInt >>> BinaryOp name >>> Binary >>> val

primitives :: Env
primitives = foldl (:+:) ε [
   "+" ↦ intIntInt "prim-plus" (+),
   "-" ↦ intIntInt "prim-minus" (-),
   "*" ↦ intIntInt "prim-times" (*),
   "div" ↦ intIntInt "prim-div" div,
   "==" ↦ intIntBool "prim-eq" (==),
   "/=" ↦ intIntBool "prim-eq" (/=),
   "<" ↦ intIntBool "prim-lt" (<),
   ">" ↦ intIntBool "prim-gt" (>),
   "<=" ↦ intIntBool "prim-leq" (<=),
   ">=" ↦ intIntBool "prim-geq" (>=),
   "intToStr" ↦ intStr "prim-intToStr" (show)
]
