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
import DataType (Ctr(..), DataType'(..), checkArity, checkDataType, ctrToDataType, cPair, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..), Var)
import Expr (Expr(..), Module(..), RawExpr(..), VarDef(..), expr) as E
import SExprX (
   Branch, Clause, Expr(..), ListPatternRest(..), ListRest(..), RawListRest(..), Module(..), Pattern(..), VarDefs(..), VarDef(..), RecDefs(..), RawQualifier(..), Qualifier(..), RawExpr(..), expr
)
import Lattice (𝔹, (∧), bot)
import Util (MayFail, type (×), (×), (≞), (≜), absurd, fromJust, mustLookup, report, error, onlyIf, maybeToEither)

qualTrue :: 𝔹 -> Qualifier 𝔹
qualTrue α = Qualifier α (Guard (Expr α (Constr cTrue Nil)))

snil :: 𝔹 -> Expr 𝔹
snil α = Expr α $ Constr cNil Nil


class DesugarBwd a b where
   desugarBwd :: a -> b -> MayFail b

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
   desugarBwd (E.Expr α (E.Constr (Ctr ":") (e : e' : Nil)))
              (Expr _ (ListNonEmpty s l)) =
      Expr α <$> (ListNonEmpty <$> desugarBwd e s <*> desugarBwd e' l)
   -- | Recursive-function
   desugarBwd (E.Expr α (E.LetRec fπs e))
              (Expr _ (LetRec fπs' s)) =
      Expr α <$> (LetRec <$> desugarBwd fπs fπs' <*> desugarBwd e s)
   -- | If-then-else
   desugarBwd (E.Expr α2 (E.App (E.Expr α1 (E.Lambda (ElimConstr m))) e1))
              (Expr _ (IfElse s1 s2 s3)) = do
      κ2 <- maybeToEither $ lookup (Ctr "True") m
      κ3 <- maybeToEither $ lookup (Ctr "False") m
      case κ2, κ3 of
         Body e2, Body e3 -> Expr (α1 ∧ α2) <$> (IfElse <$> desugarBwd e1 s1 <*> desugarBwd e2 s2 <*> desugarBwd e3 s3)
         _, _             -> error "failed to match IfElse"
   -- | Match-as
   desugarBwd (E.Expr α2 (E.App (E.Expr α1 (E.Lambda σ)) e))
              (Expr _ (MatchAs s bs)) =
      Expr (α1 ∧ α2) <$> (MatchAs <$> desugarBwd e s <*> desugarBwd σ bs)
   -- | List-range
   desugarBwd (E.Expr α2 (E.App (E.Expr α1 (E.App (E.Expr _ (E.Var "range")) e1)) e2))
              (Expr α (ListRange s1 s2)) =
      Expr (α1 ∧ α2) <$> (ListRange <$> desugarBwd e1 s1 <*> desugarBwd e2 s2)
   -- | List-comp-done
   desugarBwd (E.Expr α2 (E.Constr (Ctr ":") (e : (E.Expr α1 (E.Constr (Ctr "Nil") Nil)) : Nil)))
              (Expr _ (ListComp s_body (NonEmptyList (Qualifier _ (Guard (Expr _ (Constr (Ctr "True") Nil))) :| Nil)))) =
      Expr (α1 ∧ α2) <$> (ListComp <$> desugarBwd e s_body <*> (pure $ NonEmptyList (Qualifier (α1 ∧ α2) (Guard (Expr (α1 ∧ α2) (Constr cTrue Nil))) :| Nil)))
   -- | List-comp-qual
   desugarBwd e
              (Expr α (ListComp s_body (NonEmptyList (q :| Nil)))) = do
      sListComp <- desugarBwd e (Expr α (ListComp s_body (NonEmptyList (q :| (qualTrue true) : Nil))))
      case sListComp of
         Expr α2 (ListComp s_body'
                           (NonEmptyList (q' :| (Qualifier α1 (Guard (Expr _ (Constr (Ctr "True") Nil)))) : Nil))
                  )
            -> pure $ Expr (α1 ∧ α2) (ListComp s_body' (NonEmptyList (q' :| Nil)))
         _  -> error ""
   -- | List-comp-guard
   desugarBwd (E.Expr α2 (E.App (E.Expr α1 (E.Lambda (ElimConstr m))) e1))
              (Expr _ (ListComp s1 (NonEmptyList ((Qualifier _ (Guard s2)) :| q : qs)))) = do
      κ2 <- maybeToEither $ lookup cTrue m
      κ3 <- maybeToEither $ lookup cFalse m
      case κ2, κ3 of
         Body e2, Body e3 -> do
            s2'         <- desugarBwd e1 s2
            sListComp   <- desugarBwd e2 (Expr true (ListComp s1 (NonEmptyList (q :| qs))))
            sNil        <- desugarBwd e3 (snil true)
            case sListComp, sNil of
               Expr α3 (ListComp s1' (NonEmptyList (q' :| qs'))), Expr α4 (Constr (Ctr "Nil") Nil)
                     -> pure $ Expr (α1 ∧ α2 ∧ α3 ∧ α4)
                                    (ListComp s1' (NonEmptyList ((Qualifier (α1 ∧ α2 ∧ α3 ∧ α4) (Guard s2')) :| q' : qs')))
               _, _  -> error ""
         _, _ -> error ""
   -- s  <- desugarBwd e (Expr α (IfThen s2 (Expr α (ListComp s1 (NonEmptyList (q :| qs)) snil ))))
   -- | List-comp-decl
   desugarBwd (E.Expr α1 (E.App (E.Expr α2 (E.Lambda σ)) e))
              (Expr _ (ListComp s2 (NonEmptyList ((Qualifier _ (Declaration (p × s1))) :| q : qs)))) = do
      (p' × s1') <- desugarBwd σ (NonEmptyList (p :| Nil) × s1)
      s          <- desugarBwd e (Expr true (ListComp s2 (NonEmptyList (q :| qs))))
      case s of
         Expr α3 (ListComp s2' (NonEmptyList (q' :| qs')))
            -> pure $ Expr (α1 ∧ α2 ∧ α3) (ListComp s2' (NonEmptyList ((Qualifier (α1 ∧ α2 ∧ α3) (Declaration (p × s1'))) :| q' : qs')))
         _  -> error ""
   desugarBwd _ _ = error ""

{- e, l ↘ l -}
instance desugarBwdListRest :: DesugarBwd (E.Expr Boolean) (ListRest Boolean) where
   desugarBwd (E.Expr α (E.Constr (Ctr "Nil") Nil)) (ListRest _ End) =
      pure $ ListRest α End
   desugarBwd (E.Expr α (E.Constr (Ctr ":") (e : e' : Nil))) (ListRest _ (Next s l)) =
      ListRest α <$> (Next <$> desugarBwd e s <*> desugarBwd e' l)
   desugarBwd _ _ = error absurd

class DesugarPatternBwd a where
   desugarPatternBwd :: Elim Boolean -> a -> MayFail (Cont Boolean)

{-    →     -}
{- σ, p ↘ κ -}
instance desugarPatternBwdPatterns :: DesugarPatternBwd (NonEmptyList Pattern) where
   desugarPatternBwd σ (NonEmptyList (π :| Nil)) = desugarPatternBwd σ π
   desugarPatternBwd σ (NonEmptyList (π :| π' : πs)) = do
      κ <- desugarPatternBwd σ π
      case κ of
           Arg σ' -> desugarPatternBwd σ' (NonEmptyList (π' :| πs))
           _      -> error "NonEmptyList Pattern: σ' not found"

{- σ, p ↘ κ -}
instance desugarPatternBwdPattern :: DesugarPatternBwd Pattern where
   -- | Var
   desugarPatternBwd (ElimVar x κ)  (PVar x') = (x ≞ x') *> pure κ
   -- | Nil
   desugarPatternBwd (ElimConstr m) (PConstr (Ctr "Nil") Nil) = maybeToEither $ lookup cNil m
   -- | Cons, Pair
   desugarPatternBwd (ElimConstr m) (PConstr ctr (π:π':_))
      | ctr == cNil || ctr == cPair = do
          κ <- maybeToEither $ lookup ctr m
          case κ of
               Arg σ -> do κ' <- desugarPatternBwd σ π
                           case κ' of
                                Arg σ' -> desugarPatternBwd σ' π'
                                _      -> error "PConstr cCons: σ' not found"
               _     -> error "PConstr cCons: σ not found"
   -- | Empty-list
   desugarPatternBwd (ElimConstr m) (PListEmpty) = maybeToEither $ lookup cNil m
   -- | Non-empty-list
   desugarPatternBwd σ (PListNonEmpty π o)  = do
      κ <- desugarPatternBwd σ π
      case κ of Arg σ' -> desugarPatternBwd σ' o
                _      -> error "PListNonEmpty: σ' not found"
   desugarPatternBwd _ _ = error ""

{- σ, o ↘ κ -}
instance desugarPatternBwdListPatternRest :: DesugarPatternBwd ListPatternRest where
   desugarPatternBwd (ElimConstr m) PEnd        = maybeToEither $ lookup cCons m
   desugarPatternBwd (ElimConstr m) (PNext π o) = do
      κ <- maybeToEither $ lookup cCons m
      case κ of Arg σ -> do κ' <- desugarPatternBwd σ π
                            case κ' of Arg σ' -> desugarPatternBwd σ' o
                                       _      -> error "PNext: σ' not found"
                _     -> error "PNext: σ not found"
   desugarPatternBwd _ _ = error ""

{- σ, c ↘ c -}
instance desugarBwdBranch :: DesugarBwd (Elim Boolean) (NonEmptyList Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) = do
      κ  <- desugarPatternBwd σ πs
      case κ of Body e -> do s' <- desugarBwd e s
                             pure $ πs × s'
                _      -> error "Branch: e not found"

{-    →   → -}
{- σ, c ↘ c -}
instance desugarBwdBranches :: DesugarBwd (Elim Boolean) (NonEmptyList (NonEmptyList Pattern × Expr Boolean)) where
   desugarBwd σ (NonEmptyList (b1 :| b2 : bs)) = do
      b'  <- desugarBwd σ b1
      bs' <- desugarBwd σ (NonEmptyList (b2 :| bs))
      pure $ NonEmptyList (b' :| (toList bs'))
   desugarBwd σ (NonEmptyList (b :| Nil)) = do
      b' <- desugarBwd σ b
      pure $ NonEmptyList (b' :| Nil)