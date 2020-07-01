module Primitive where

import Prelude hiding (apply, append, map)
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Map (Map, fromFoldable)
import Text.Parsing.Parser.Expr (Assoc(..))
import Bindings (Bindings(..), Var, (:+:), (↦))
import DataType (cTrue, cFalse)
import Lattice (Selected, (∧))
import Util (type (×), (×), error)
import Expr as E
import Expr (Expr(..), Elim)
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
apply (IntOp op) v = op $ to v

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
append (Expr α (E.Constr cNil Nil)) (Expr α' ys) = (Expr α' ys)
append (Expr α (E.Constr cCons (e:es:Nil))) (Expr α' ys) = let zs = es `append` (Expr α' ys)
                                                           in  Expr α (E.Constr cCons (e:zs:Nil))
append _ _ = error "List expression expected"

concat :: Expr -> Expr
concat (Expr α (E.Constr cCons (e:es:Nil))) = e `append` (concat es)
concat (Expr α (E.Constr cNil Nil)) = (Expr α (E.Constr cNil Nil))
concat _ = error "List expression expected"

map :: Elim -> Expr -> Expr
map σ (Expr α (E.Constr cCons (e:es:Nil)))
   = (Expr α (E.MatchAs e σ)) `append` (map σ es)
map _ (Expr α (E.Constr cNil Nil))
   = (Expr α (E.Constr cNil Nil))
map _ _ = error "List expression expected"

concatMap :: Elim -> Expr -> Expr
concatMap σ (Expr α (E.Constr cCons (e:es:Nil)))
   = (Expr α (E.MatchAs e σ)) `append` (concatMap σ es)
concatMap _ (Expr α (E.Constr cNil Nil))
   = (Expr α (E.Constr cNil Nil))
concatMap _ _ = error "List expression expected"
