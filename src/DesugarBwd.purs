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
import Data.Profunctor.Strong (first)
import Data.Set (toUnfoldable) as S
import Data.Tuple (uncurry, fst, snd)
import DataType (Ctr, arity, cCons, cNil, cTrue, cFalse, ctrs, dataTypeFor)
import Dict (Dict, get)
import Dict (fromFoldable) as D
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), RecDefs, VarDef(..)) as E
import Lattice (class BoundedJoinSemilattice, (∨), bot)
import Partial.Unsafe (unsafePartial)
import SExpr (Clause(..), Expr(..), ListRest(..), ListRestPattern(..), Pattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs, RecDef(..))
import Util (type (+), type (×), Endo, absurd, error, successful, (×))
import Util.Pair (Pair(..))

desugarBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Expr a -> Expr a
desugarBwd = exprBwd

varDefsBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> VarDefs a × Expr a -> VarDefs a × Expr a
varDefsBwd (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π s1 :| Nil) × s2) =
   NonEmptyList (VarDef π (exprBwd e1 s1) :| Nil) × exprBwd e2 s2
varDefsBwd (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π s1 :| d : ds) × s2) =
   let
      NonEmptyList (d' :| ds') × s2' = varDefsBwd e2 (NonEmptyList (d :| ds) × s2)
   in
      NonEmptyList (VarDef π (exprBwd e1 s1) :| d' : ds') × s2'
varDefsBwd _ (NonEmptyList (_ :| _) × _) = error absurd

recDefsBwd :: forall a. BoundedJoinSemilattice a => E.RecDefs a -> RecDefs a -> RecDefs a
recDefsBwd ρ xcs = join (recDefsBwd' ρ (groupBy (eq `on` fst) xcs))

recDefsBwd' :: forall a. BoundedJoinSemilattice a => E.RecDefs a -> NonEmptyList (RecDefs a) -> NonEmptyList (RecDefs a)
recDefsBwd' ρ (NonEmptyList (xcs :| xcss)) =
   let
      x = fst (head xcs)
      xcss' = case xcss of
         Nil -> Nil
         xcs2 : xcss'' -> toList (recDefsBwd' ρ (NonEmptyList (xcs2 :| xcss'')))
   in
      NonEmptyList (unwrap (recDefBwd (x ↦ get x ρ) (RecDef xcs)) :| xcss')

recDefBwd :: forall a. BoundedJoinSemilattice a => Bind (Elim a) -> RecDef a -> RecDef a
recDefBwd (x ↦ σ) (RecDef rds) = RecDef (map (x × _) (clausesBwd_curried σ (map snd rds)))

exprBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Expr a -> Expr a
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
exprBwd (E.Lambda σ) (Lambda bs) = Lambda (clausesBwd_curried σ bs)
exprBwd (E.Project e _) (Project s x) = Project (exprBwd e s) x
exprBwd (E.App e1 e2) (App s1 s2) = App (exprBwd e1 s1) (exprBwd e2 s2)
-- | <<-match checked, assuming bwded+unwrapped implementation are correct
exprBwd (E.App (E.Lambda σ) e) (MatchAs s bs) =
   let
      bwded = clausesBwd_curried σ (map Clause (map (first $ NE.singleton) bs)) :: NonEmptyList (Clause _)
      unwrapped = map (\(x × y) -> head x × y) (map unwrap bwded) :: NonEmptyList (Pattern × Expr _)
   in
      MatchAs (exprBwd e s) (unwrapped)
-- | <<-if checked
exprBwd (E.App (E.Lambda (ElimConstr m)) e1) (IfElse s1 s2 s3) =
   IfElse (exprBwd e1 s1)
      (exprBwd (asExpr (get cTrue m)) s2)
      (exprBwd (asExpr (get cFalse m)) s3)
-- | <<-binary-apply checked
exprBwd (E.App (E.App (E.Op _) e1) e2) (BinaryApp s1 op s2) =
   BinaryApp (exprBwd e1 s1) op (exprBwd e2 s2)
exprBwd (E.Let d e) (Let ds s) = uncurry Let (varDefsBwd (E.Let d e) (ds × s))
-- | <<-let-rec checked
exprBwd (E.LetRec xσs e') (LetRec xcs s) = LetRec (recDefsBwd xσs xcs) (exprBwd e' s)
exprBwd (E.Constr α _ Nil) (ListEmpty _) = ListEmpty α
exprBwd (E.Constr α _ (e1 : e2 : Nil)) (ListNonEmpty _ s l) =
   ListNonEmpty α (exprBwd e1 s) (listRestBwd e2 l)
-- | <<-list-enum checked
exprBwd (E.App (E.App (E.Var "enumFromTo") e1) e2) (ListEnum s1 s2) =
   ListEnum (exprBwd e1 s1) (exprBwd e2 s2)
-- list-comp-done
exprBwd (E.Constr α2 _ (e' : E.Constr α1 _ Nil : Nil)) (ListComp _ s_body ((Guard (Constr _ c Nil) : Nil))) | c == cTrue =
   ListComp (α1 ∨ α2) (exprBwd e' s_body)
      ((Guard (Constr (α1 ∨ α2) cTrue Nil) : Nil))
-- | <<-list-comp-last
exprBwd e (ListComp α s ((q : Nil))) =
   case exprBwd e (ListComp α s ((q : Guard (Constr bot cTrue Nil) : Nil))) of
      ListComp β s' ((q' : (Guard (Constr _ c Nil)) : Nil)) | c == cTrue ->
         (ListComp β s' ((q' : Nil)))
      _ -> error absurd
-- | <<-list-comp-guard checked
exprBwd (E.App (E.Lambda (ElimConstr m)) e2) (ListComp α0 s1 ((Guard s2 : q : qs))) =
   case -- first element of pair is e†
      exprBwd (asExpr (get cTrue m)) (ListComp α0 s1 ((q : qs))) ×
         exprBwd (asExpr (get cFalse m)) (Constr bot cNil Nil)
      of
      ListComp β s1' ((q' : qs')) × Constr α c Nil | c == cNil ->
         ListComp (α ∨ β) s1' ((Guard (exprBwd e2 s2) : q' : qs'))
      _ × _ -> error absurd

-- | <<-list-comp-decl
exprBwd (E.App (E.Lambda σ) e1) (ListComp α0 s2 ((Declaration (VarDef π s1) : q : qs))) =
   case exprBwd (asExpr (pattContBwd σ π)) (ListComp α0 s2 ((q : qs))) of
      ListComp β s2' ((q' : qs')) ->
         ListComp β s2' (((Declaration (VarDef π (exprBwd e1 s1))) : q' : qs'))
      _ -> error absurd

-- list-comp-gen
exprBwd
   (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e1)
   (ListComp α s2 ((Generator p s1 : q : qs))) =
   let
      σ' × β = orElseBwd (ContElim σ) (Left p : Nil)
   in
      case exprBwd (asExpr (pattContBwd (asElim σ') p)) (ListComp α s2 ((q : qs))) of
         ListComp β' s2' ((q' : qs')) ->
            ListComp (β ∨ β') s2' ((Generator p (exprBwd e1 s1) : q' : qs'))
         _ -> error absurd
exprBwd _ _ = error absurd

-- e, l desugar_bwd l
listRestBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Endo (ListRest a)
listRestBwd (E.Constr α _ _) (End _) = End α
listRestBwd (E.Constr α _ (e1 : e2 : Nil)) (Next _ s l) =
   Next α (exprBwd e1 s) (listRestBwd e2 l)
listRestBwd _ _ = error absurd

-- σ, ps desugar_bwd e
pattsExprBwd :: forall a. Elim a -> NonEmptyList Pattern -> E.Expr a
pattsExprBwd σ (NonEmptyList (p :| Nil)) = asExpr (pattContBwd σ p)
pattsExprBwd σ (NonEmptyList (p :| p' : ps)) = pattsExprBwd_rest (asExpr (pattContBwd σ p))
   where
   pattsExprBwd_rest (E.Lambda τ) = pattsExprBwd τ (NonEmptyList (p' :| ps))
   pattsExprBwd_rest _ = error absurd

-- σ, p desugar_bwd κ
pattContBwd :: forall a. Elim a -> Pattern -> Cont a
pattContBwd (ElimVar _ κ) (PVar _) = κ
pattContBwd (ElimConstr m) (PConstr c ps) = pattCont_args_Bwd (get c m) (Left <$> ps)
pattContBwd (ElimConstr m) (PListEmpty) = get cNil m
pattContBwd (ElimConstr m) (PListNonEmpty p o) = pattCont_args_Bwd (get cCons m) (Left p : Right o : Nil)
pattContBwd (ElimRecord _ κ) (PRecord xps) = pattCont_record_Bwd κ (sortBy (flip compare `on` fst) xps)
pattContBwd _ _ = error absurd

-- σ, o desugar_bwd κ
pattCont_listRest_Bwd :: forall a. Elim a -> ListRestPattern -> Cont a
pattCont_listRest_Bwd (ElimVar _ _) _ = error absurd
pattCont_listRest_Bwd (ElimRecord _ _) _ = error absurd
pattCont_listRest_Bwd (ElimConstr m) PEnd = get cNil m
pattCont_listRest_Bwd (ElimConstr m) (PNext p o) = pattCont_args_Bwd (get cCons m) (Left p : Right o : Nil)

pattCont_args_Bwd :: forall a. Cont a -> List (Pattern + ListRestPattern) -> Cont a
pattCont_args_Bwd κ Nil = κ
pattCont_args_Bwd κ (Left p : πs) = pattCont_args_Bwd (pattContBwd (asElim κ) p) πs
pattCont_args_Bwd κ (Right o : πs) = pattCont_args_Bwd (pattCont_listRest_Bwd (asElim κ) o) πs

pattCont_record_Bwd :: forall a. Cont a -> List (Bind Pattern) -> Cont a
pattCont_record_Bwd κ Nil = κ
pattCont_record_Bwd σ (_ ↦ p : xps) = pattCont_record_Bwd σ xps # (asElim >>> flip pattContBwd p)

-- σ, c desugar_bwd c'
clauseBwd_curried :: forall a. BoundedJoinSemilattice a => Elim a -> Endo (Clause a)
clauseBwd_curried σ (Clause (πs × s)) = Clause $ πs × exprBwd (pattsExprBwd σ πs) s

-- σ, c desugar_bwd c'
clauseBwd_uncurried :: forall a. BoundedJoinSemilattice a => Elim a -> Endo (Pattern × Expr a)
clauseBwd_uncurried σ (p × s) = p × exprBwd (asExpr (pattContBwd σ p)) s

-- σ, cs desugar_bwd cs'
clausesBwd_curried :: forall a. BoundedJoinSemilattice a => Elim a -> Endo (NonEmptyList (Clause a))
clausesBwd_curried σ (NonEmptyList (b1 :| b2 : bs)) =
   NonEmptyList (clauseBwd_curried σ b1 :| toList (clausesBwd_curried σ (NonEmptyList (b2 :| bs))))
clausesBwd_curried σ (NonEmptyList (b :| Nil)) =
   NonEmptyList (clauseBwd_curried σ b :| Nil)

-- κ, πs totalise_bwd κ', α
orElseBwd :: forall a. BoundedJoinSemilattice a => Cont a -> List (Pattern + ListRestPattern) -> Cont a × a
orElseBwd κ Nil = κ × bot
orElseBwd ContNone _ = error absurd
orElseBwd (ContElim (ElimVar _ κ')) (Left (PVar x) : πs) =
   let
      κ'' × α = orElseBwd κ' πs
   in
      ContElim (ElimVar x κ'') × α
orElseBwd (ContElim (ElimRecord _ κ')) (Left (PRecord xps) : πs) =
   let
      ps = xps <#> (snd >>> Left)
      κ'' × α = orElseBwd κ' (ps <> πs)
   in
      ContElim (ElimRecord (keys xps) κ'') × α
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
      κ' × α = orElseConstrBwd m c
      κ'' × β = orElseBwd κ' (πs' <> πs)
   in
      ContElim (ElimConstr (D.fromFoldable (singleton (c × κ'')))) × (α ∨ β)
orElseBwd _ _ = error absurd

-- Discard all synthesised branches, returning the original singleton branch for c, plus join of annotations
-- on the empty lists used for bodies of synthesised branches.
orElseConstrBwd :: forall a. BoundedJoinSemilattice a => Dict (Cont a) -> Ctr -> Cont a × a
orElseConstrBwd m c = unsafePartial $
   let
      cs = (ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ singleton c
   in
      get c m × foldl (∨) bot ((bodyAnn <<< body) <$> cs)
   where
   body :: Partial => Ctr -> Cont a
   body c' = applyN unargument (successful (arity c')) (get c' m)

   unargument :: Partial => Cont a -> Cont a
   unargument (ContElim (ElimVar _ κ)) = κ

   bodyAnn :: Partial => Cont a -> a
   bodyAnn (ContExpr (E.Constr α c' Nil)) | c' == cNil = α
