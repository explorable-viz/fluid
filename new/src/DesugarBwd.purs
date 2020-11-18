module DesugarBwd where

import Prelude hiding (absurd)
import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Function (on)
import Data.List (List(..), (:), (\\), length, zip)
import Data.List (head) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, reverse, toList)
import Data.Map (Map, fromFoldable, insert, lookup, singleton, toUnfoldable, update)
import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Data.Tuple (Tuple, fst, snd, uncurry)
import Bindings (Binding, Bindings, (↦), fromList)
import DataType (Ctr(..), DataType'(..), checkArity, checkDataType, ctrToDataType, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..), Var)
import Expr (Expr(..), Module(..), RawExpr(..), VarDef(..), expr) as E
import SExprX (
   Clause, Expr(..), ListPatternRest(..), ListRest(..), RawListRest(..), Module(..), Pattern(..), VarDefs(..), VarDef(..), RecDefs(..), RawQualifier(..), Qualifier(..), RawExpr(..), expr
)
import Lattice (𝔹, (∧), bot)
import Util (MayFail, type (×), (×), (≞), (≜), absurd, fromJust, mustLookup, report, error)

class DesugarBwd a b where
   desugarBwd :: a -> b -> MayFail b

instance desugarBwdListRest :: DesugarBwd (E.Expr Boolean) (ListRest Boolean) where
   desugarBwd (E.Expr α (E.Constr (Ctr "Nil") Nil)) (ListRest _ End) =
      pure $ ListRest α End
   desugarBwd (E.Expr α (E.Constr (Ctr ":") (e : e' : Nil))) (ListRest _ (Next s l)) =
      ListRest α <$> (Next <$> desugarBwd e s <*> desugarBwd e' l)
   desugarBwd _ _ = error absurd

instance desugarBwdRecDefs :: DesugarBwd (Bindings Elim Boolean)
                                         (NonEmptyList (String × ((NonEmptyList Pattern) × (Expr Boolean)))) where
   desugarBwd _ _ = error ""

-- | traverse :: (a -> m b) -> t a -> m (t b)
instance desugarBwdExpr :: DesugarBwd (E.Expr Boolean) (Expr Boolean) where
   desugarBwd (E.Expr α (E.Var x))   (Expr _ (Var x'))      = pure $ Expr α (Var (x ≜ x'))
   desugarBwd (E.Expr α (E.Op op))   (Expr _ (Op op'))      = pure $ Expr α (Op (op ≜ op'))
   desugarBwd (E.Expr α (E.Int n))   (Expr _ (Int n'))      = pure $ Expr α (Int (n ≜ n'))
   desugarBwd (E.Expr α (E.Float n)) (Expr _ (Float n'))    = pure $ Expr α (Float (n ≜ n'))
   desugarBwd (E.Expr α (E.Str s))   (Expr _ (Str s'))      = pure $ Expr α (Str (s ≜ s'))
   -- | This covers Cons
   desugarBwd (E.Expr α (E.Constr ctr args)) (Expr _ (Constr ctr' args')) =
      Expr α <$> (Constr ctr <$> traverse (uncurry desugarBwd) (zip args args'))
   -- | Application
   desugarBwd (E.Expr α (E.App e1 e2)) (Expr _ (App s1 s2)) =
      Expr α <$> (App <$> desugarBwd e1 s1 <*> desugarBwd e2 s2)
   desugarBwd (E.Expr α (E.BinaryApp e1 x e2)) (Expr _ (BinaryApp s1 x' s2)) =
      Expr α <$> (BinaryApp <$> desugarBwd e1 s1 <@> x ≜ x' <*> desugarBwd e2 s2)
   -- | Empty-list
   desugarBwd (E.Expr α (E.Constr (Ctr "Nil") Nil)) (Expr _ ListEmpty) = pure $ Expr α ListEmpty
   -- | Non-empty list
   desugarBwd (E.Expr α (E.Constr (Ctr ":") (e : e' : Nil))) (Expr _ (ListNonEmpty s l)) =
      Expr α <$> (ListNonEmpty <$> desugarBwd e s <*> desugarBwd e' l)
   -- | Recursive-function
   -- type E.RecDefs = Bindings Elim
   -- type RecDefs   = NonEmptyList (Var × Branch a)
   desugarBwd (E.Expr α (E.LetRec fπs e)) (Expr _ (LetRec fπs' s)) =
      Expr α <$> (LetRec <$> desugarBwd fπs fπs' <*> desugarBwd e s)
   -- desugarBwd (E.Expr α (E.Let de e)) = desugarBwd $ α × (de × e)
   -- desugarBwd (E.LetRec fπs e)
   -- desugarBwd (E.Expr α (E.Lambda σ)) (Expr _ (Lambda σ))   =
   --    Expr α <$> (Lambda <$> desugarBwd σ)
   desugarBwd _ _ = error ""

-- data RawExpr a =
--    Var Var |
--    Op Var |
--    Int Int |
--    Float Number |
--    Str String |
--    Constr Ctr (List (Expr a)) |
--    Lambda (NonEmptyList (Branch a)) |
--    App (Expr a) (Expr a) |
--    BinaryApp (Expr a) Var (Expr a) |
--    MatchAs (Expr a) (NonEmptyList (Branch a)) |
--    IfElse (Expr a) (Expr a) (Expr a) |
--    ListEmpty |  -- in the formalism we unify with Nil, but cleaner here to keep separate
--    ListNonEmpty (Expr a) (ListRest a) |
--    ListRange (Expr a) (Expr a) |
--    ListComp (Expr a) (NonEmptyList (Qualifier a)) |
--    Let (VarDefs a) (Expr a) |
--    LetRec (RecDefs a) (Expr a)
