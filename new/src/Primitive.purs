module Primitive where

import Prelude hiding (apply, append, map)
import Prelude (map) as P
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map (Map, fromFoldable)
import Text.Parsing.Parser.Expr (Assoc(..))
import Bindings (Bindings(..), Var, (:+:), (↦))
import DataType (cTrue, cFalse, Ctr(..))
import Lattice (Selected, (∧))
import Util (type (×), (×), error)
import Expr as E
import Expr (Expr(..), Elim, expr)
import Val (Env, Primitive(..), Val(..), val)
import Val (RawVal(..)) as V

-- name in user land, precedence 0 to 9 (similar to Haskell 98), associativity
type OpDef = {
   op    :: Var,
   prec  :: Int,
   assoc :: Assoc
}

opDef :: Var -> Int -> Assoc -> Var × OpDef
opDef op prec assoc = op × { op, prec, assoc }

-- Syntactic information only. No guarantee that any of these will be defined.
opDefs :: Map String OpDef
opDefs = fromFoldable [
   opDef "*"   7 AssocLeft,
   opDef "+"   6 AssocLeft,
   opDef "-"   6 AssocLeft,
   opDef ":"   6 AssocRight,
   opDef "=="  4 AssocNone,
   opDef "/="  4 AssocNone,
   opDef "<"   4 AssocLeft,
   opDef ">"   4 AssocLeft,
   opDef "<="  4 AssocLeft,
   opDef ">="  4 AssocLeft
]

class ToList a where
   toList :: a -> List a

class FromList a where
   fromList :: List a -> a

instance exprToList :: ToList Expr where
   toList (Expr a (E.Constr (Ctr "Cons") (e:es:Nil))) = (e:toList es)
   toList (Expr a (E.Constr (Ctr "Nil") Nil)) = Nil
   toList _ = error "expected list expression"

instance exprFromList :: FromList Expr where
   fromList (x:xs) = expr $ (E.Constr (Ctr "Cons") (x:fromList xs:Nil))
   fromList Nil    = expr $ E.Constr (Ctr "Nil") Nil

-- Enforce primitive argument types.
class To a where
   to :: Val -> a

class From a where
   from :: a -> Val

instance toInt :: To Int where
   to (Val _ (V.Int n)) = n
   to _ = error "Integer expected"

instance fromInt :: From Int where
   from = V.Int >>> val

true_ :: Val
true_ = val $ V.Constr cTrue Nil

false_ :: Val
false_ = val $ V.Constr cFalse Nil

instance fromBoolean :: From Boolean where
   from b = if b then true_ else false_

instance fromString :: From String where
   from = V.Str >>> val

instance fromIntOp :: From a => From (Int -> a) where
   from op = val $ V.Primitive $ IntOp $ op >>> from

apply :: Primitive -> Val -> Val
apply (IntOp op) = op <<< to

apply_fwd :: Primitive -> Selected -> Val -> Val
apply_fwd φ α v@(Val α' _) =
   Val (α ∧ α') u where Val _ u = apply φ v

primitives :: Env
primitives = foldl (:+:) Empty [
   -- need to instantiate the corresponding PureScript primitive at a concrete type
   "+"         ↦ from   ((+)  :: Int -> Int -> Int),
   "-"         ↦ from   ((-)  :: Int -> Int -> Int),
   "*"         ↦ from   ((*)  :: Int -> Int -> Int),
   "div"       ↦ from   (div  :: Int -> Int -> Int),
   "=="        ↦ from   ((==) :: Int -> Int -> Boolean),
   "/="        ↦ from   ((/=) :: Int -> Int -> Boolean),
   "<"         ↦ from   ((<)  :: Int -> Int -> Boolean),
   ">"         ↦ from   ((>)  :: Int -> Int -> Boolean),
   "<="        ↦ from   ((<=) :: Int -> Int -> Boolean),
   ">="        ↦ from   ((>=) :: Int -> Int -> Boolean),
   "intToStr"  ↦ from   (show :: Int -> String)
]

append :: Expr -> Expr -> Expr
append e1 e2 = fromList $ (toList e1) <> (toList e2)

concat :: Expr -> Expr
concat e1 = fromList $ L.concat $ P.map toList (toList e1)

map :: Elim -> Expr -> Expr
map σ e = fromList $ P.map (applyσ σ) (toList e)
   where applyσ :: Elim -> Expr -> Expr
         applyσ σ' e' = expr $ E.MatchAs e' σ'

concatMap :: Elim -> Expr -> Expr
concatMap = map
