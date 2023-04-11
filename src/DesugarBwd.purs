module DesugarBwd where

import Prelude hiding (absurd, top)

import Bindings (Bind, (↦), keys)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (applyN, on)
import Data.List (List(..), singleton, sortBy, zip, zipWith, (:), (\\))
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList)
import Data.List.NonEmpty (singleton) as NE
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Profunctor.Strong (first, (***))
import Data.Set (toUnfoldable) as S
import Data.Tuple (uncurry, fst, snd)
import DataType (Ctr, arity, cCons, cNil, cTrue, cFalse, ctrs, dataTypeFor)
import Dict (Dict, get)
import Dict (fromFoldable) as D
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), RecDefs, VarDef(..)) as E
import Lattice (class BoundedJoinSemilattice, (∨), bot, Raw)
import Partial.Unsafe (unsafePartial)
import SExpr (Clause(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, RecDef(..))
import Util (Endo, type (+), type (×), absurd, error, successful, (×))
import Util.Pair (Pair(..))

desugarBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw Expr -> Expr a
desugarBwd = exprBwd

varDefsBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw VarDefs × Raw Expr -> VarDefs a × Expr a
varDefsBwd (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π s1 :| Nil) × s2) =
   NonEmptyList (VarDef π (exprBwd e1 s1) :| Nil) × exprBwd e2 s2
varDefsBwd (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π s1 :| d : ds) × s2) =
   let
      NonEmptyList (d' :| ds') × s2' = varDefsBwd e2 (NonEmptyList (d :| ds) × s2)
   in
      NonEmptyList (VarDef π (exprBwd e1 s1) :| d' : ds') × s2'
varDefsBwd _ (NonEmptyList (_ :| _) × _) = error absurd

recDefsBwd :: forall a. BoundedJoinSemilattice a => E.RecDefs a -> Raw RecDefs -> RecDefs a
recDefsBwd ρ xcs = join (recDefsBwd' ρ (groupBy (eq `on` fst) xcs))

recDefsBwd' :: forall a. BoundedJoinSemilattice a => E.RecDefs a -> NonEmptyList (Raw RecDefs) -> NonEmptyList (RecDefs a)
recDefsBwd' ρ (NonEmptyList (xcs :| xcss)) =
   let
      x = fst (head xcs)
      xcss' = case xcss of
         Nil -> Nil
         xcs2 : xcss'' -> toList (recDefsBwd' ρ (NonEmptyList (xcs2 :| xcss'')))
   in
      NonEmptyList (unwrap (recDefBwd (x ↦ get x ρ) (RecDef xcs)) :| xcss')

recDefBwd :: forall a. BoundedJoinSemilattice a => Bind (Elim a) -> Raw RecDef -> RecDef a
recDefBwd (x ↦ σ) (RecDef rds) = RecDef (map (x × _) (clausesBwd σ (map snd rds)))

-- e, s desugar_bwd s'
exprBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw Expr -> Expr a
exprBwd (E.Var _) (Var x) = Var x
exprBwd (E.Op _) (Op op) = Op op
exprBwd (E.Int α _) (Int _ n) = Int α n
exprBwd (E.Float α _) (Float _ n) = Float α n
exprBwd (E.Str α _) (Str _ str) = Str α str
exprBwd (E.Constr α _ es) (Constr _ c ss) = Constr α c (uncurry exprBwd <$> zip es ss)
exprBwd (E.Record α xes) (Record _ xss) =
   Record α $ xss <#> \(x ↦ s) -> x ↦ exprBwd (get x xes) s
exprBwd (E.Dictionary α ees) (Dictionary _ sss) =
   Dictionary α (zipWith (\(Pair e e') (Pair s s') -> Pair (exprBwd e s) (exprBwd e' s')) ees sss)
exprBwd (E.Matrix α e1 _ e2) (Matrix _ s (x × y) s') =
   Matrix α (exprBwd e1 s) (x × y) (exprBwd e2 s')
exprBwd (E.Lambda σ) (Lambda bs) = Lambda (clausesBwd σ bs)
exprBwd (E.Project e _) (Project s x) = Project (exprBwd e s) x
exprBwd (E.App e1 e2) (App s1 s2) = App (exprBwd e1 s1) (exprBwd e2 s2)
exprBwd (E.App (E.Lambda σ) e) (MatchAs s bs) =
   let
      bwded = clausesBwd σ (map Clause (map (first $ NE.singleton) bs)) :: NonEmptyList (Clause _)
      unwrapped = first head <$> map unwrap bwded :: NonEmptyList (Pattern × Expr _)
   in
      MatchAs (exprBwd e s) (unwrapped)
exprBwd (E.App (E.Lambda (ElimConstr m)) e1) (IfElse s1 s2 s3) =
   IfElse (exprBwd e1 s1)
      (exprBwd (asExpr (get cTrue m)) s2)
      (exprBwd (asExpr (get cFalse m)) s3)
exprBwd (E.App (E.App (E.Op _) e1) e2) (BinaryApp s1 op s2) =
   BinaryApp (exprBwd e1 s1) op (exprBwd e2 s2)
exprBwd (E.Let d e) (Let ds s) = uncurry Let (varDefsBwd (E.Let d e) (ds × s))
exprBwd (E.LetRec xσs e') (LetRec xcs s) = LetRec (recDefsBwd xσs xcs) (exprBwd e' s)
exprBwd (E.Constr α _ Nil) (ListEmpty _) = ListEmpty α
exprBwd (E.Constr α _ (e1 : e2 : Nil)) (ListNonEmpty _ s l) =
   ListNonEmpty α (exprBwd e1 s) (listRestBwd e2 l)
exprBwd (E.App (E.App (E.Var "enumFromTo") e1) e2) (ListEnum s1 s2) =
   ListEnum (exprBwd e1 s1) (exprBwd e2 s2)

-- -- list-comp-done
-- exprBwd (E.Constr α2 _ (e' : E.Constr α1 _ Nil : Nil)) (ListComp _ s_body (Guard (Constr _ c Nil) : Nil)) | c == cTrue =
--    ListComp (α1 ∨ α2) (exprBwd e' s_body) (Guard (Constr (α1 ∨ α2) cTrue Nil) : Nil)

-- | list-comp-done
exprBwd (E.Constr α2 c (e : E.Constr α1 c' Nil : Nil)) (ListComp _ s Nil) | c == cCons && c' == cNil =
   ListComp (α1 ∨ α2) (exprBwd e s) Nil

-- list-comp-guard
exprBwd (E.App (E.Lambda (ElimConstr m)) e2) (ListComp _ s1 (Guard s2 : qs)) =
   case
      exprBwd (asExpr (get cTrue m)) (ListComp unit s1 qs) ×
         exprBwd (asExpr (get cFalse m)) (Constr bot cNil Nil)
      of
      ListComp β s1' qs' × Constr α c Nil | c == cNil ->
         ListComp (α ∨ β) s1' (Guard (exprBwd e2 s2) : qs')
      _ × _ -> error absurd
-- list-comp-decl
exprBwd (E.App (E.Lambda σ) e1) (ListComp _ s2 (Declaration (VarDef π s1) : qs)) =
   case exprBwd (asExpr (pattContBwd π σ)) (ListComp unit s2 qs) of
      ListComp β s2' qs' ->
         ListComp β s2' (Declaration (VarDef π (exprBwd e1 s1)) : qs')
      _ -> error absurd
-- list-comp-gen
exprBwd
   (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e1)
   (ListComp _ s2 (Generator p s1 : qs)) =
   let
      σ' × β = orElseBwd (ContElim σ) (Left p : Nil)
   in
      case exprBwd (asExpr (pattContBwd p (asElim σ'))) (ListComp unit s2 qs) of
         ListComp β' s2' qs' ->
            ListComp (β ∨ β') s2' (Generator p (exprBwd e1 s1) : qs')
         _ -> error absurd
exprBwd _ _ = error absurd

-- e, l desugar_bwd l'
listRestBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw ListRest -> ListRest a
listRestBwd (E.Constr α _ _) (End _) = End α
listRestBwd (E.Constr α _ (e1 : e2 : Nil)) (Next _ s l) =
   Next α (exprBwd e1 s) (listRestBwd e2 l)
listRestBwd _ _ = error absurd

pattsExprBwd :: forall a. NonEmptyList Pattern -> Elim a -> E.Expr a
pattsExprBwd (NonEmptyList (p :| Nil)) σ = asExpr (pattContBwd p σ)
pattsExprBwd (NonEmptyList (p :| p' : ps)) σ = next (asExpr (pattContBwd p σ))
   where
   next (E.Lambda τ) = pattsExprBwd (NonEmptyList (p' :| ps)) τ
   next _ = error absurd

pattContBwd :: forall a. Pattern -> Elim a -> Cont a
pattContBwd (PVar _) (ElimVar _ κ) = κ
pattContBwd (PConstr c ps) (ElimConstr m) = pattArgsBwd (Left <$> ps) (get c m)
pattContBwd (PListEmpty) (ElimConstr m) = get cNil m
pattContBwd (PListNonEmpty p o) (ElimConstr m) = pattArgsBwd (Left p : Right o : Nil) (get cCons m)
pattContBwd (PRecord xps) (ElimRecord _ κ) = pattArgsBwd ((snd >>> Left) <$> sortBy (compare `on` fst) xps) κ
pattContBwd _ _ = error absurd

pattCont_ListRest_Bwd :: forall a. Elim a -> ListRestPattern -> Cont a
pattCont_ListRest_Bwd (ElimVar _ _) _ = error absurd
pattCont_ListRest_Bwd (ElimRecord _ _) _ = error absurd
pattCont_ListRest_Bwd (ElimConstr m) PEnd = get cNil m
pattCont_ListRest_Bwd (ElimConstr m) (PNext p o) = pattArgsBwd (Left p : Right o : Nil) (get cCons m)
pattCont_ListRest_Bwd (ElimSug _ κ) p = pattCont_ListRest_Bwd κ p

pattArgsBwd :: forall a. List (Pattern + ListRestPattern) -> Endo (Cont a)
pattArgsBwd Nil κ = κ
pattArgsBwd (Left p : πs) σ = pattArgsBwd πs (pattContBwd p (asElim σ))
pattArgsBwd (Right o : πs) σ = pattArgsBwd πs (pattCont_ListRest_Bwd (asElim σ) o)

clausesBwd :: forall a. BoundedJoinSemilattice a => Elim a -> NonEmptyList (Raw Clause) -> NonEmptyList (Clause a)
clausesBwd σ bs = clauseBwd <$> bs
   where
   clauseBwd :: Raw Clause -> Clause a
   clauseBwd (Clause (πs × s)) = Clause (πs × exprBwd (pattsExprBwd πs σ) s)

orElseBwd :: forall a. BoundedJoinSemilattice a => Cont a -> List (Pattern + ListRestPattern) -> Cont a × a
orElseBwd κ Nil = κ × bot
orElseBwd ContNone _ = error absurd
orElseBwd (ContElim (ElimVar _ κ')) (Left (PVar x) : πs) =
   orElseBwd κ' πs # first (\κ'' -> ContElim (ElimVar x κ''))
orElseBwd (ContElim (ElimRecord _ κ')) (Left (PRecord xps) : πs) =
   orElseBwd κ' ((xps <#> (snd >>> Left)) <> πs) # first (\κ'' -> ContElim (ElimRecord (keys xps) κ''))
orElseBwd (ContElim (ElimConstr m)) (π : πs) =
   let
      c × πs' = case π of
         -- TODO: refactor so these two cases aren't necessary
         Left (PVar _) -> error absurd
         Left (PRecord _) -> error absurd
         Left (PConstr c ps) -> c × (Left <$> ps)
         Left PListEmpty -> cNil × Nil
         Left (PListNonEmpty p o) -> cCons × (Left p : Right o : Nil)
         Right PEnd -> cNil × Nil
         Right (PNext p o) -> cCons × (Left p : Right o : Nil)
      κ' × α = unlessBwd m c
   in
      orElseBwd κ' (πs' <> πs) #
         (\κ'' -> ContElim (ElimConstr (D.fromFoldable (singleton (c × κ''))))) *** (α ∨ _)
orElseBwd _ _ = error absurd

-- Discard all synthesised branches, returning the original singleton branch for c, plus join of annotations
-- on the empty lists used for bodies of synthesised branches.
unlessBwd :: forall a. BoundedJoinSemilattice a => Dict (Cont a) -> Ctr -> Cont a × a
unlessBwd m c = unsafePartial $
   let
      cs = (ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ singleton c
   in
      get c m × foldl (∨) bot ((bodyAnn <<< body) <$> cs)
   where
   body :: Partial => Ctr -> Cont a
   body c' = applyN (\(ContElim (ElimVar _ κ)) -> κ) (successful (arity c')) (get c' m)

   bodyAnn :: Partial => Cont a -> a
   bodyAnn (ContExpr (E.Constr α c' Nil)) | c' == cNil = α
