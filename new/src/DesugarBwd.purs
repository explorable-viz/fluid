module DesugarBwd where

import Prelude hiding (absurd)
import Data.List (List(..), (:), zip)
import Data.List.NonEmpty (NonEmptyList(..), toList)
import Data.Map (fromFoldable)
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Bindings (Bindings)
import DataType (Ctr(..), cPair, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..))
import Expr (Expr(..), RawExpr(..)) as E
import SExprX (
   Expr(..), ListPatternRest(..), ListRest(..), RawListRest(..), Pattern(..), RawQualifier(..), Qualifier(..), RawExpr(..)
)
import Lattice (𝔹, (∧))
import Util (MayFail, type (×), (×), (≞), (≜), absurd, mustLookup, lookupE, error)

qualTrue :: 𝔹 -> Qualifier 𝔹
qualTrue α = Qualifier α (Guard (Expr α (Constr cTrue Nil)))

snil :: 𝔹 -> Expr 𝔹
snil α = Expr α $ Constr cNil Nil

class DesugarBwd a b where
   desugarBwd :: a -> b -> MayFail b

instance desugarBwdRecDefs :: DesugarBwd (Bindings Elim Boolean)
                                         (NonEmptyList (String × ((NonEmptyList Pattern) × (Expr Boolean)))) where
   desugarBwd _ _ = error ""

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
   desugarBwd (E.Expr α (E.Constr (Ctr "Nil") Nil)) (Expr _ ListEmpty) =
      pure $ Expr α ListEmpty
   -- | Non-empty-list
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
      e2 <- liftM1 asExpr $ lookupE cTrue m
      e3 <- liftM1 asExpr $ lookupE cFalse m
      Expr (α1 ∧ α2) <$> (IfElse <$> desugarBwd e1 s1 <*> desugarBwd e2 s2 <*> desugarBwd e3 s3)
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
      e2          <- liftM1 asExpr $ lookupE cTrue  m
      e3          <- liftM1 asExpr $ lookupE cFalse m
      s2'         <- desugarBwd e1 s2
      sListComp   <- desugarBwd e2 (Expr true (ListComp s1 (NonEmptyList (q :| qs))))
      sNil        <- desugarBwd e3 (snil true)
      case sListComp, sNil of
         Expr α3 (ListComp s1' (NonEmptyList (q' :| qs'))), Expr α4 (Constr (Ctr "Nil") Nil)
               -> pure $ Expr (α1 ∧ α2 ∧ α3 ∧ α4)
                              (ListComp s1' (NonEmptyList ((Qualifier (α1 ∧ α2 ∧ α3 ∧ α4) (Guard s2')) :| q' : qs')))
         _, _  -> error ""
   -- | List-comp-decl
   desugarBwd (E.Expr α1 (E.App (E.Expr α2 (E.Lambda σ)) e))
              (Expr _ (ListComp s2 (NonEmptyList ((Qualifier _ (Declaration (p × s1))) :| q : qs)))) = do
      (p' × s1') <- desugarBwd σ (NonEmptyList (p :| Nil) × s1)
      sListComp  <- desugarBwd e (Expr true (ListComp s2 (NonEmptyList (q :| qs))))
      case sListComp of
         Expr α3 (ListComp s2' (NonEmptyList (q' :| qs')))
            -> pure $ Expr (α1 ∧ α2 ∧ α3)
                           (ListComp s2' (NonEmptyList ((Qualifier (α1 ∧ α2 ∧ α3) (Declaration (p × s1'))) :| q' : qs')))
         _  -> error ""
   -- | List-comp-gen
   desugarBwd (E.Expr α3 (E.App (E.Expr α2 (E.App (E.Expr _  (E.Var "concatMap"))
                                                  (E.Expr α1 (E.Lambda σ))))
                                 e1))
              (Expr _ (ListComp s2 (NonEmptyList ((Qualifier _ (Generator p s1)) :| q : qs)))) = do
      s1'        <- desugarBwd e1 s1
      σ'         <- pure $ asElim $ untotalisePatt (Arg σ) p
      e2         <- liftM1 asExpr (desugarPatternBwd σ' p)
      sListComp  <- desugarBwd e2 (Expr true (ListComp s2 (NonEmptyList (q :| qs))))
      case sListComp of
         Expr α4 (ListComp s2' (NonEmptyList (q' :| qs'))) ->
            pure $ Expr (α1 ∧ α2 ∧ α3 ∧ α4)
                        (ListComp s2' (NonEmptyList ((Qualifier (α1 ∧ α2 ∧ α3) (Generator p s1)) :| q' : qs')))
         _ -> error ""

   desugarBwd _ _ = error ""

asElim :: Cont 𝔹 -> Elim 𝔹
asElim (Arg σ) =  σ
asElim _ = error "Couldn't infer Elim from Cont"

asExpr :: Cont 𝔹 -> E.Expr 𝔹
asExpr (Body e) =  e
asExpr _ = error "Couldn't infer Expr from Cont"

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
      σ' <- liftM1 asElim $ desugarPatternBwd σ π
      desugarPatternBwd σ' (NonEmptyList (π' :| πs))

{- σ, p ↘ κ -}
instance desugarPatternBwdPattern :: DesugarPatternBwd Pattern where
   -- | Var
   desugarPatternBwd (ElimVar x κ)  (PVar x') = (x ≞ x') *> pure κ
   -- | Nil
   desugarPatternBwd (ElimConstr m) (PConstr (Ctr "Nil") Nil) = lookupE cNil m
   -- | Cons, Pair
   desugarPatternBwd (ElimConstr m) (PConstr ctr (π:π':_))
      | ctr == cNil || ctr == cPair = do
          σ  <- liftM1 asElim $ lookupE ctr m
          σ' <- liftM1 asElim $ desugarPatternBwd σ π
          desugarPatternBwd σ' π'
   -- | Empty-list
   desugarPatternBwd (ElimConstr m) (PListEmpty) = lookupE cNil m
   -- | Non-empty-list
   desugarPatternBwd σ (PListNonEmpty π o)  = do
      σ' <- liftM1 asElim $ desugarPatternBwd σ π
      desugarPatternBwd σ' o
   desugarPatternBwd _ _ = error ""

{- σ, o ↘ κ -}
instance desugarPatternBwdListPatternRest :: DesugarPatternBwd ListPatternRest where
   desugarPatternBwd (ElimConstr m) PEnd        = lookupE cCons m
   desugarPatternBwd (ElimConstr m) (PNext π o) = do
      σ  <- liftM1 asElim $ lookupE cCons m
      σ' <- liftM1 asElim $ desugarPatternBwd σ π
      desugarPatternBwd σ' o
   desugarPatternBwd _ _ = error ""

{- σ, c ↘ c -}
instance desugarBwdBranch :: DesugarBwd (Elim Boolean) (NonEmptyList Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) = do
      e  <- liftM1 asExpr $ desugarPatternBwd σ πs
      s' <- desugarBwd e s
      pure $ πs × s'

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

{- untotalise κ p ↗ κ' -}
untotalisePatt :: Cont 𝔹 -> Pattern -> Cont 𝔹
untotalisePatt (Arg σ) p =
   case σ, p of
      -- | var
      ElimVar x κ, PVar x'            ->
         if x == x' then Arg (ElimVar x κ) else error absurd
      -- | true, false, pair, nil, cons
      ElimConstr m, PConstr ctr ps    ->
         let κ = mustLookup ctr m
         in  Arg $ ElimConstr (fromFoldable [ctr × untotaliseListPatt κ ps])
      -- | patt-list-empty
      ElimConstr m, PListEmpty        ->
         let κ = mustLookup cNil m
         in  Arg $ ElimConstr (fromFoldable [cNil × κ])
      -- | patt-list-non-empty
      ElimConstr m, PListNonEmpty p' o ->
         let κ = mustLookup cCons m
         in  Arg $ ElimConstr (fromFoldable [cCons × untotaliseListPattRest (untotalisePatt κ p') o])
      _, _ -> error ""
untotalisePatt _ _ = error ""

untotaliseListPatt :: Cont 𝔹 -> List Pattern -> Cont 𝔹
untotaliseListPatt κ Nil = κ
untotaliseListPatt κ (p:ps) =
   untotaliseListPatt (untotalisePatt κ p) ps

{- untotalise κ o ↗ κ' -}
untotaliseListPattRest :: Cont 𝔹 -> ListPatternRest -> Cont 𝔹
untotaliseListPattRest (Arg (ElimConstr m)) PEnd =
   let κ = mustLookup cNil m
   in  Arg $ ElimConstr (fromFoldable [cNil × κ])
untotaliseListPattRest (Arg (ElimConstr m)) (PNext p o) =
   let κ = mustLookup cCons m
   in  Arg $ ElimConstr (fromFoldable [cCons × untotaliseListPattRest (untotalisePatt κ p) o])
untotaliseListPattRest _ _ = error ""
