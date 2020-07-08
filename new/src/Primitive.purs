module Primitive where

import Prelude hiding (apply, append, map)
import Prelude (map) as P
import Data.Foldable (foldl)
import Data.List (List(..), (:), snoc)
import Data.List as L
import Data.Map (Map, fromFoldable, toUnfoldable, singleton)
import Debug.Trace (trace) as T
import Text.Parsing.Parser.Expr (Assoc(..))
import Bindings (Bindings(..), Var, (:+:), (↦))
import DataType (cTrue, cNil, cCons, cFalse, cMany, Ctr(..))
import Pretty (pretty)
import Lattice (Selected, (∧))
import Util (type (×), (×), error, fromJust)
import Expr as E
import Expr (Expr(..), Elim(..), Cont(..), expr)
import Val (Env, Primitive(..), Val(..), val)
import Val (RawVal(..)) as V


trace s a = T.trace (pretty s) $ \_-> a
trace' s a = T.trace  s $ \_-> a

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
   toList (Expr a (E.Constr (Ctr ":") (e:es:Nil))) = (e:toList es)
   toList (Expr a (E.Constr (Ctr "Nil") Nil)) = Nil
   toList _ = error "expected list expression"

instance exprFromList :: FromList Expr where
   fromList (x:xs) = expr $ (E.Constr (Ctr ":") (x:fromList xs:Nil))
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

-- concat :: Expr -> Expr
-- concat e1 = fromList $ L.concat $ P.map toList (toList e1)

map :: Elim -> Expr -> Expr
map σ e = fromList $ P.map (applyσ σ) (toList e)
   where applyσ :: Elim -> Expr -> Expr
         applyσ σ' e' = expr $ E.MatchAs e' σ'

nilExpr :: Expr
nilExpr = Expr false (E.Constr cNil Nil)

concat' :: Expr -> Expr
concat' es = fromList (L.concat (P.map toList (toList $ fromList $ (P.map concat (toList es)))))

concat :: Expr -> Expr
concat (Expr _ (E.MatchAs e (ElimVar x (Body match_es))))
   = fromList $ P.map mergeMatch (toList match_es)
   where mergeMatch :: Expr -> Expr
         mergeMatch (Expr _ (E.MatchAs e' (ElimVar y (Body e_body))))
            = let merged_e = Expr false (E.Constr cMany (e':e:Nil))
                  merged_σ = ElimConstr (singleton cMany (Arg (ElimVar y (Arg (ElimVar x (Body (e_body)))))))
              in  Expr false (E.MatchAs merged_e merged_σ)
         mergeMatch (Expr _ (E.MatchAs (Expr _ (E.Constr cMany es)) (ElimConstr m)))
            = let ctr × k = fromJust "No head element found" (L.head $ toUnfoldable m)
                  merged_e = Expr false (E.Constr cMany (snoc es e))
                  merged_σ = ElimConstr (singleton cMany (insertArg k x))
              in  Expr false (E.MatchAs merged_e merged_σ)
         mergeMatch e = let k0 = trace e 0 in error "mergeMatch error 1"

concat (Expr _ (E.MatchAs (Expr _ (E.Constr cMany es)) (ElimConstr m)))
   = fromList $ P.map mergeMatch (toList match_es)
   where ctr × k = fromJust "No head element found" (L.head $ toUnfoldable m)
         match_es = getEMatches k
         mergeMatch :: Expr -> Expr
         mergeMatch (Expr _ (E.MatchAs e' (ElimVar y (Body e_body))))
            = let merged_e = Expr false (E.Constr cMany (snoc es e'))
                  merged_σ = ElimConstr (singleton cMany (insertArgEbody k y e_body))
              in  Expr false (E.MatchAs merged_e merged_σ)
         mergeMatch e = let k0 = trace e 0 in error "mergeMatch error 2"

concat _ = error "concat error"

getEMatches :: Cont -> Expr
getEMatches (Arg (ElimVar x k)) = getEMatches k
getEMatches (Body es) = es
getEMatches _ = error "getEMatches error"

insertArg :: Cont -> Var -> Cont
insertArg (Arg (ElimVar x k)) y = Arg (ElimVar x (insertArg k y))
insertArg (Body e_body) y = Arg (ElimVar y (Body e_body))
insertArg _ _ = error "insertArg error"

insertArgEbody :: Cont -> Var -> Expr -> Cont
insertArgEbody (Arg (ElimVar x k)) y e_body = Arg (ElimVar x (insertArgEbody k y e_body))
insertArgEbody (Body e_matchas) y e_body = Arg (ElimVar y (Body e_body))
insertArgEbody _ _ _ = error "insertArgEbody error"