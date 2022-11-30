module DesugarBwd where

import Prelude hiding (absurd)

import Bindings (Bind, (↦), keys)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (applyN, on)
import Data.List (List(..), (:), (\\), singleton, sortBy, zip)
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList)
import Data.NonEmpty ((:|))
import Data.Set (toUnfoldable) as S
import Data.Tuple (uncurry, fst, snd)
import DataType (Ctr, arity, cCons, cNil, cTrue, cFalse, ctrs, dataTypeFor)
import Dict (Dict, get)
import Dict (fromFoldable) as D
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), RecDefs, VarDef(..)) as E
import Lattice (𝔹, (∨))
import Partial.Unsafe (unsafePartial)
import SExpr (Branch, Clause, Expr(..), ListRest(..), Pattern(..), ListRestPattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs)
import Util (type (+), type (×), Endo, Pair(..), absurd, error, successful, (×))

desugarBwd :: E.Expr 𝔹 -> Expr 𝔹 -> Expr 𝔹
desugarBwd = exprBwd

varDefsBwd :: E.Expr 𝔹 -> VarDefs 𝔹 × Expr 𝔹 -> VarDefs 𝔹 × Expr 𝔹
varDefsBwd (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π s1 :| Nil) × s2) =
   NonEmptyList (VarDef π (exprBwd e1 s1) :| Nil) × exprBwd e2 s2
varDefsBwd (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π s1 :| d : ds) × s2) =
   let
      NonEmptyList (d' :| ds') × s2' = varDefsBwd e2 (NonEmptyList (d :| ds) × s2)
   in
      NonEmptyList (VarDef π (exprBwd e1 s1) :| d' : ds') × s2'
varDefsBwd _ (NonEmptyList (_ :| _) × _) = error absurd

recDefsBwd :: E.RecDefs 𝔹 -> RecDefs 𝔹 -> RecDefs 𝔹
recDefsBwd ρ xcs = join (recDefsBwd' ρ (groupBy (eq `on` fst) xcs))

recDefsBwd' :: E.RecDefs 𝔹 -> NonEmptyList (RecDefs 𝔹) -> NonEmptyList (RecDefs 𝔹)
recDefsBwd' ρ (NonEmptyList (xcs :| xcss)) =
   let
      x = fst (head xcs)
      xcss' = case xcss of
         Nil -> Nil
         xcs2 : xcss'' -> toList (recDefsBwd' ρ (NonEmptyList (xcs2 :| xcss'')))
   in
      NonEmptyList (recDefBwd (x ↦ get x ρ) xcs :| xcss')

recDefBwd :: Bind (Elim 𝔹) -> NonEmptyList (Clause 𝔹) -> NonEmptyList (Clause 𝔹)
recDefBwd (x ↦ σ) = map (x × _) <<< branchesBwd_curried σ <<< map snd

exprBwd :: E.Expr 𝔹 -> Expr 𝔹 -> Expr 𝔹
exprBwd (E.Var _) (Var x) = Var x
exprBwd (E.Op _) (Op op) = Op op
exprBwd (E.Int α _) (Int _ n) = Int α n
exprBwd (E.Float α _) (Float _ n) = Float α n
exprBwd (E.Str α _) (Str _ str) = Str α str
exprBwd (E.Constr α _ es) (Constr _ c ss) = Constr α c (uncurry exprBwd <$> zip es ss)
exprBwd (E.Record α xes) (Record _ xss) =
   Record α $ xss <#> \(x ↦ s) -> x ↦ exprBwd (get x xes) s
exprBwd (E.Dictionary α ees) (Dictionary _ sss) =
   Dictionary α $ (zip ees sss) <#> (\(ee × ss) -> exprBwd <$> ee <*> ss)
exprBwd (E.Matrix α e1 _ e2) (Matrix _ s (x × y) s') =
   Matrix α (exprBwd e1 s) (x × y) (exprBwd e2 s')
exprBwd (E.Lambda σ) (Lambda bs) = Lambda (branchesBwd_curried σ bs)
exprBwd (E.Project e _) (Project s x) = Project (exprBwd e s) x
exprBwd (E.App e1 e2) (App s1 s2) = App (exprBwd e1 s1) (exprBwd e2 s2)
exprBwd (E.App (E.Lambda σ) e) (MatchAs s bs) =
   MatchAs (exprBwd e s) (branchesBwd_uncurried σ bs)
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
-- list-comp-done
exprBwd (E.Constr α2 _ (e' : E.Constr α1 _ Nil : Nil)) (ListComp _ s_body (NonEmptyList (Guard (Constr _ c Nil) :| Nil))) | c == cTrue =
   ListComp (α1 ∨ α2) (exprBwd e' s_body)
      (NonEmptyList (Guard (Constr (α1 ∨ α2) cTrue Nil) :| Nil))
-- list-comp-last
exprBwd e (ListComp α s (NonEmptyList (q :| Nil))) =
   case exprBwd e (ListComp α s (NonEmptyList (q :| Guard (Constr true cTrue Nil) : Nil))) of
      ListComp β s' (NonEmptyList (q' :| (Guard (Constr _ c Nil)) : Nil)) | c == cTrue ->
         (ListComp β s' (NonEmptyList (q' :| Nil)))
      _ -> error absurd
-- list-comp-guard
exprBwd (E.App (E.Lambda (ElimConstr m)) e2) (ListComp α0 s1 (NonEmptyList (Guard s2 :| q : qs))) =
   case
      exprBwd (asExpr (get cTrue m)) (ListComp α0 s1 (NonEmptyList (q :| qs))) ×
         exprBwd (asExpr (get cFalse m)) (Constr true cNil Nil)
      of
      ListComp β s1' (NonEmptyList (q' :| qs')) × Constr α c Nil | c == cNil ->
         ListComp (α ∨ β) s1' (NonEmptyList (Guard (exprBwd e2 s2) :| q' : qs'))
      _ × _ -> error absurd
-- list-comp-decl
exprBwd (E.App (E.Lambda σ) e1) (ListComp α0 s2 (NonEmptyList (Declaration (VarDef π s1) :| q : qs))) =
   case branchBwd_curried σ (NonEmptyList (π :| Nil) × (ListComp α0 s2 (NonEmptyList (q :| qs)))) of
      _ × ListComp β s2' (NonEmptyList (q' :| qs')) ->
         ListComp β s2' (NonEmptyList ((Declaration (VarDef π (exprBwd e1 s1))) :| q' : qs'))
      _ × _ -> error absurd
-- list-comp-gen
exprBwd
   (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e1)
   (ListComp α s2 (NonEmptyList (Generator p s1 :| q : qs))) =
   let
      σ' × β = totaliseBwd (ContElim σ) (Left p : Nil)
   in
      case exprBwd (asExpr (patternBwd (asElim σ') p)) (ListComp α s2 (NonEmptyList (q :| qs))) of
         ListComp β' s2' (NonEmptyList (q' :| qs')) ->
            ListComp (β ∨ β') s2' (NonEmptyList (Generator p (exprBwd e1 s1) :| q' : qs'))
         _ -> error absurd
exprBwd _ _ = error absurd

-- e, l desugar_bwd l
listRestBwd :: E.Expr 𝔹 -> Endo (ListRest 𝔹)
listRestBwd (E.Constr α _ _) (End _) = End α
listRestBwd (E.Constr α _ (e1 : e2 : Nil)) (Next _ s l) =
   Next α (exprBwd e1 s) (listRestBwd e2 l)
listRestBwd _ _ = error absurd

-- σ, ps desugar_bwd e
patternsBwd :: Elim 𝔹 -> NonEmptyList Pattern -> E.Expr 𝔹
patternsBwd σ (NonEmptyList (p :| Nil)) = asExpr (patternBwd σ p)
patternsBwd σ (NonEmptyList (p :| p' : ps)) = patternsBwd_rest (asExpr (patternBwd σ p))
   where
   patternsBwd_rest (E.Lambda τ) = patternsBwd τ (NonEmptyList (p' :| ps))
   patternsBwd_rest _ = error absurd

-- σ, p desugar_bwd κ
patternBwd :: Elim 𝔹 -> Pattern -> Cont 𝔹
patternBwd (ElimVar _ κ) (PVar _) = κ
patternBwd (ElimConstr m) (PConstr c ps) = argsBwd (get c m) (Left <$> ps)
patternBwd (ElimConstr m) (PListEmpty) = get cNil m
patternBwd (ElimConstr m) (PListNonEmpty p o) = argsBwd (get cCons m) (Left p : Right o : Nil)
patternBwd (ElimRecord _ κ) (PRecord xps) = recordBwd κ (sortBy (flip compare `on` fst) xps)
patternBwd _ _ = error absurd

-- σ, o desugar_bwd κ
listRestPatternBwd :: Elim 𝔹 -> ListRestPattern -> Cont 𝔹
listRestPatternBwd (ElimVar _ _) _ = error absurd
listRestPatternBwd (ElimRecord _ _) _ = error absurd
listRestPatternBwd (ElimConstr m) PEnd = get cNil m
listRestPatternBwd (ElimConstr m) (PNext p o) = argsBwd (get cCons m) (Left p : Right o : Nil)

argsBwd :: Cont 𝔹 -> List (Pattern + ListRestPattern) -> Cont 𝔹
argsBwd κ Nil = κ
argsBwd κ (Left p : πs) = argsBwd (patternBwd (asElim κ) p) πs
argsBwd κ (Right o : πs) = argsBwd (listRestPatternBwd (asElim κ) o) πs

recordBwd :: Cont 𝔹 -> List (Bind Pattern) -> Cont 𝔹
recordBwd κ Nil = κ
recordBwd σ (_ ↦ p : xps) = recordBwd σ xps # (asElim >>> flip patternBwd p)

-- σ, c desugar_bwd c'
branchBwd_curried :: Elim 𝔹 -> Endo (Branch 𝔹)
branchBwd_curried σ (πs × s) = πs × exprBwd (patternsBwd σ πs) s

-- σ, c desugar_bwd c'
branchBwd_uncurried :: Elim 𝔹 -> Endo (Pattern × Expr 𝔹)
branchBwd_uncurried σ (p × s) = p × exprBwd (asExpr (patternBwd σ p)) s

-- σ, cs desugar_bwd cs'
branchesBwd_curried :: Elim 𝔹 -> Endo (NonEmptyList (Branch 𝔹))
branchesBwd_curried σ (NonEmptyList (b1 :| b2 : bs)) =
   NonEmptyList (branchBwd_curried σ b1 :| toList (branchesBwd_curried σ (NonEmptyList (b2 :| bs))))
branchesBwd_curried σ (NonEmptyList (b :| Nil)) =
   NonEmptyList (branchBwd_curried σ b :| Nil)

-- σ, cs desugar_bwd cs'
branchesBwd_uncurried :: Elim 𝔹 -> Endo (NonEmptyList (Pattern × Expr 𝔹))
branchesBwd_uncurried σ (NonEmptyList (b1 :| b2 : bs)) =
   NonEmptyList (branchBwd_uncurried σ b1 :| toList (branchesBwd_uncurried σ (NonEmptyList (b2 :| bs))))
branchesBwd_uncurried σ (NonEmptyList (b :| Nil)) =
   NonEmptyList (branchBwd_uncurried σ b :| Nil)

-- κ, πs totalise_bwd κ', α
totaliseBwd :: Cont 𝔹 -> List (Pattern + ListRestPattern) -> Cont 𝔹 × 𝔹
totaliseBwd κ Nil = κ × false
totaliseBwd ContNone _ = error absurd
totaliseBwd (ContElim (ElimVar _ κ')) (Left (PVar x) : πs) =
   let
      κ'' × α = totaliseBwd κ' πs
   in
      ContElim (ElimVar x κ'') × α
totaliseBwd (ContElim (ElimRecord _ κ')) (Left (PRecord xps) : πs) =
   let
      ps = xps <#> (snd >>> Left)
      κ'' × α = totaliseBwd κ' (ps <> πs)
   in
      ContElim (ElimRecord (keys xps) κ'') × α
totaliseBwd (ContElim (ElimConstr m)) (π : πs) =
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
      κ' × α = totaliseConstrBwd m c
      κ'' × β = totaliseBwd κ' (πs' <> πs)
   in
      ContElim (ElimConstr (D.fromFoldable (singleton (c × κ'')))) × (α ∨ β)
totaliseBwd _ _ = error absurd

-- Discard all synthesised branches, returning the original singleton branch for c, plus join of annotations
-- on the empty lists used for bodies of synthesised branches.
totaliseConstrBwd :: Dict (Cont 𝔹) -> Ctr -> Cont 𝔹 × 𝔹
totaliseConstrBwd m c = unsafePartial $
   let
      cs = (ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ singleton c
   in
      get c m × foldl (∨) false (map (bodyAnn <<< body) cs)
   where
   body :: Partial => Ctr -> Cont 𝔹
   body c' = applyN unargument (successful (arity c')) (get c' m)

   unargument :: Partial => Cont 𝔹 -> Cont 𝔹
   unargument (ContElim (ElimVar _ κ)) = κ

   bodyAnn :: Partial => Cont 𝔹 -> 𝔹
   bodyAnn (ContExpr (E.Constr α c' Nil)) | c' == cNil = α
