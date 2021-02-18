module DesugarBwd where

import Prelude hiding (absurd)
import Data.Function (on)
import Data.Either (Either(..))
import Data.List (List(..), (:), zip)
import Data.List.NonEmpty (NonEmptyList(..), groupBy, toList, reverse)
import Data.Map (fromFoldable)
import Data.NonEmpty ((:|))
import Data.Tuple (uncurry, fst, snd)
import Data.Profunctor.Strong (first)
import Bindings (Binding, Bindings(..), (↦), (:+:), varAnon)
import DataType (cCons, cNil, cTrue, cFalse)
import DesugarFwd (elimBool)
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), RecDefs, VarDef(..)) as E
import SExpr (
   Branch, Clause, Expr(..), ListRest(..), Pattern(..), ListRestPattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs
)
import Lattice (𝔹, (∨), expand)
import Util (Endo, type (+), type (×), (×), absurd, mustLookup, error)

desugarBwd :: E.Expr 𝔹 -> Expr 𝔹 -> Expr 𝔹
desugarBwd = exprBwd

varDefsBwd :: E.Expr 𝔹 -> VarDefs 𝔹 × Expr 𝔹 -> VarDefs 𝔹 × Expr 𝔹
varDefsBwd (E.Let (E.VarDef σ e1) e2) (NonEmptyList (VarDef π s1 :| Nil) × s2) =
   NonEmptyList (VarDef π (exprBwd e1 s1) :| Nil) × exprBwd e2 s2
varDefsBwd (E.Let (E.VarDef σ e1) e2) (NonEmptyList (VarDef π s1 :| d : ds) × s2) =
   let NonEmptyList (d' :| ds') × s2' = varDefsBwd e2 (NonEmptyList (d :| ds) × s2) in
   NonEmptyList (VarDef π (exprBwd e1 s1) :| d' : ds') × s2'
varDefsBwd _ (NonEmptyList (_ :| _) × _) = error absurd

recDefsBwd :: E.RecDefs 𝔹 -> RecDefs 𝔹 -> RecDefs 𝔹
recDefsBwd xσs xcs = join (zipRecDefs xσs (reverse (groupBy (eq `on` fst) xcs)))

zipRecDefs :: E.RecDefs 𝔹 -> Endo (NonEmptyList (NonEmptyList (Clause 𝔹)))
zipRecDefs Empty _                                             = error absurd
zipRecDefs (Empty :+: x ↦ σ) (NonEmptyList (xcs :| Nil))       = NonEmptyList (recDefBwd (x ↦ σ) xcs :| Nil)
zipRecDefs (_ :+: _ :+: _) (NonEmptyList (_ :| Nil))           = error absurd
zipRecDefs (ρ :+: x ↦ σ) (NonEmptyList (xcs1 :| xcs2 : xcss))  =
   NonEmptyList (recDefBwd (x ↦ σ) xcs1 :| toList (zipRecDefs ρ (NonEmptyList (xcs2 :| xcss))))

recDefBwd :: Binding Elim 𝔹 -> NonEmptyList (Clause 𝔹) -> NonEmptyList (Clause 𝔹)
recDefBwd (x ↦ σ) = map (x × _) <<< branchesBwd_curried σ <<< map snd

exprBwd :: E.Expr 𝔹 -> Expr 𝔹 -> Expr 𝔹
exprBwd e (Var x) =
   case expand e (E.Var x) of
      E.Var _ -> Var x
      _ -> error absurd
exprBwd e (Op op) =
   case expand e (E.Op op) of
      E.Op _ -> Op op
      _ -> error absurd
exprBwd e (Int _ n) =
   case expand e (E.Int false n) of
      E.Int α _ -> Int α n
      _ -> error absurd
exprBwd e (Float _ n) =
   case expand e (E.Float false n) of
      E.Float α _ -> Float α n
      _ -> error absurd
exprBwd e (Str _ str) =
   case expand e (E.Str false str) of
      E.Str α _ -> Str α str
      _ -> error absurd
exprBwd e (Constr _ c es) =
   case expand e (E.Constr false c (const E.Hole <$> es)) of
      E.Constr α _ es' -> Constr α c (uncurry exprBwd <$> zip es' es)
      _ -> error absurd
exprBwd e (Matrix _ s _ s') =
   case expand e (E.Matrix false E.Hole (varAnon × varAnon) E.Hole) of
      E.Matrix α e1 (x × y) e2 -> Matrix α (exprBwd e1 s) (x × y) (exprBwd e2 s')
      _ -> error absurd
exprBwd e (Lambda bs) =
   case expand e (E.Lambda ElimHole) of
      E.Lambda σ -> Lambda (branchesBwd_curried σ bs)
      _ -> error absurd
exprBwd e (App s1 s2) =
   case expand e (E.App E.Hole E.Hole) of
      E.App e1 e2 -> App (exprBwd e1 s1) (exprBwd e2 s2)
      _ -> error absurd
exprBwd e (MatchAs s bs) =
   case expand e (E.App (E.Lambda ElimHole) E.Hole) of
      E.App (E.Lambda σ) e' -> MatchAs (exprBwd e' s) (branchesBwd_uncurried σ bs)
      _ -> error absurd
exprBwd e (IfElse s1 s2 s3) =
   case expand e (E.App (E.Lambda (elimBool ContHole ContHole)) E.Hole) of
      E.App (E.Lambda (ElimConstr m)) e1 ->
         IfElse (exprBwd e1 s1)
                  (exprBwd (asExpr (mustLookup cTrue m)) s2)
                  (exprBwd (asExpr (mustLookup cFalse m)) s3)
      _ -> error absurd
exprBwd e (BinaryApp s1 _ s2) =
   case expand e (E.BinaryApp E.Hole varAnon E.Hole) of
      E.BinaryApp e1 op e2 -> BinaryApp (exprBwd e1 s1) op (exprBwd e2 s2)
      _ -> error absurd
exprBwd (E.Let d e) (Let ds s)                        = uncurry Let (varDefsBwd (E.Let d e) (ds × s))
-- THIS CASE NEEDS WORK
exprBwd (E.LetRec xσs e) (LetRec xcs s)               = LetRec (recDefsBwd xσs xcs) (exprBwd e s)
exprBwd e (ListEmpty _) =
   case expand e (E.Constr false cNil Nil) of
      E.Constr α _ Nil -> ListEmpty α
      _ -> error absurd
exprBwd e (ListNonEmpty _ s l) =
   case expand e (E.Constr false cCons (E.Hole : E.Hole : Nil)) of
      E.Constr α _ (e1 : e2 : Nil) ->
         ListNonEmpty α (exprBwd e1 s) (listRestBwd e2 l)
      _ -> error absurd
exprBwd e (ListEnum s1 s2) =
   case expand e (E.App (E.App (E.Var "enumFromTo") E.Hole) E.Hole) of
      E.App (E.App (E.Var "enumFromTo") e1) e2 ->
         ListEnum (exprBwd e1 s1) (exprBwd e2 s2)
      _ -> error absurd
-- list-comp-done
exprBwd e (ListComp _ s_body (NonEmptyList (Guard (Constr _ cTrue' Nil) :| Nil))) | cTrue' == cTrue =
   case expand e (E.Constr false cCons (E.Hole : E.Constr false cNil Nil : Nil)) of
      E.Constr α2 cCons' (e' : E.Constr α1 cNil' Nil : Nil) ->
         ListComp (α1 ∨ α2) (exprBwd e' s_body)
                           (NonEmptyList (Guard (Constr (α1 ∨ α2) cTrue Nil) :| Nil))
      _ -> error absurd
-- list-comp-last
exprBwd e (ListComp α s (NonEmptyList (q :| Nil))) =
   case exprBwd e (ListComp α s (NonEmptyList (q :| Guard (Constr true cTrue Nil) : Nil))) of
      ListComp β s' (NonEmptyList (q' :| (Guard (Constr _ c Nil)) : Nil)) | c == cTrue ->
         (ListComp β s' (NonEmptyList (q' :| Nil)))
      _ -> error absurd
-- list-comp-guard
exprBwd e (ListComp α0 s1 (NonEmptyList (Guard s2 :| q : qs))) =
   case expand e (E.App (E.Lambda (elimBool ContHole ContHole)) E.Hole) of
      E.App (E.Lambda (ElimConstr m)) e2 ->
         case exprBwd (asExpr (mustLookup cTrue m)) (ListComp α0 s1 (NonEmptyList (q :| qs))) ×
            exprBwd (asExpr (mustLookup cFalse m)) (Constr true cNil Nil) of
            ListComp β s1' (NonEmptyList (q' :| qs')) × Constr α c Nil | c == cNil ->
               ListComp (α ∨ β) s1' (NonEmptyList (Guard (exprBwd e2 s2) :| q' : qs'))
            _ × _ -> error absurd
      _ -> error absurd
-- list-comp-decl
exprBwd e (ListComp α0 s2 (NonEmptyList (Declaration (VarDef π s1) :| q : qs))) =
   case expand e (E.App (E.Lambda ElimHole) E.Hole) of
      E.App (E.Lambda σ) e1 ->
         case branchBwd_curried σ (NonEmptyList (π :| Nil) × (ListComp α0 s2 (NonEmptyList (q :| qs)))) of
            _ × ListComp β s2' (NonEmptyList (q' :| qs')) ->
               ListComp β s2' (NonEmptyList ((Declaration (VarDef π (exprBwd e1 s1))) :| q' : qs'))
            _ × _ -> error absurd
      _ -> error absurd
-- list-comp-gen
exprBwd e (ListComp α s2 (NonEmptyList (Generator p s1 :| q : qs))) =
   case expand e (E.App (E.App (E.Var "concatMap") (E.Lambda ElimHole)) E.Hole) of
      E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e1 ->
         let σ' × β = totaliseBwd (ContElim σ) (Left p : Nil) in
         case exprBwd (asExpr (patternBwd (asElim σ') p)) (ListComp α s2 (NonEmptyList (q :| qs))) of
            ListComp β' s2' (NonEmptyList (q' :| qs')) ->
               ListComp (β ∨ β') s2' (NonEmptyList (Generator p (exprBwd e1 s1) :| q' : qs'))
            _ -> error absurd
      _ -> error absurd
exprBwd (E.Hole) s = error "todo"
exprBwd _ _ = error absurd

-- e, l desugar_bwd l
listRestBwd :: E.Expr 𝔹 -> Endo (ListRest 𝔹)
listRestBwd e l@(End _) =
   case e of
      E.Constr α c Nil | c == cNil  -> End α
      E.Hole                        -> listRestBwd (E.Constr false cNil Nil) l
      _                             -> error absurd
listRestBwd e l@(Next _ s l') =
   case e of
      E.Constr α c (e1 : e2 : Nil) | c == cCons -> Next α (exprBwd e1 s) (listRestBwd e2 l')
      E.Hole                                    -> listRestBwd (E.Constr false cCons (E.Hole : E.Hole : Nil)) l
      _                                         -> error absurd

-- σ, ps desugar_bwd e
patternsBwd :: Elim 𝔹 -> NonEmptyList Pattern -> E.Expr 𝔹
patternsBwd σ (NonEmptyList (p :| Nil))      = asExpr (patternBwd σ p)
patternsBwd σ (NonEmptyList (p :| p' : ps))  = patternsBwd_rest (asExpr (patternBwd σ p))
   where
      patternsBwd_rest E.Hole        = patternsBwd_rest (E.Lambda ElimHole)
      patternsBwd_rest (E.Lambda σ') = patternsBwd σ' (NonEmptyList (p' :| ps))
      patternsBwd_rest _             = error absurd

-- σ, p desugar_bwd κ
patternBwd :: Elim 𝔹 -> Pattern -> Cont 𝔹
patternBwd ElimHole _                          = error "todo"
patternBwd (ElimVar x κ) (PVar _)              = κ
patternBwd (ElimConstr m) (PConstr c ps)       = argsBwd (mustLookup c m) (Left <$> ps)
patternBwd (ElimConstr m) (PListEmpty)         = mustLookup cNil m
patternBwd (ElimConstr m) (PListNonEmpty p o)  = argsBwd (mustLookup cCons m) (Left p : Right o : Nil)
patternBwd _ _                                 = error absurd

-- σ, o desugar_bwd κ
listRestPatternBwd :: Elim 𝔹 -> ListRestPattern -> Cont 𝔹
listRestPatternBwd ElimHole _                 = error "todo"
listRestPatternBwd (ElimVar _ _) _            = error absurd
listRestPatternBwd (ElimConstr m) PEnd        = mustLookup cNil m
listRestPatternBwd (ElimConstr m) (PNext p o) = argsBwd (mustLookup cCons m) (Left p : Right o : Nil)

argsBwd :: Cont 𝔹 -> List (Pattern + ListRestPattern) -> Cont 𝔹
argsBwd κ Nil = κ
argsBwd κ (Left p : πs) = argsBwd (patternBwd (asElim κ) p) πs
argsBwd κ (Right o : πs) = argsBwd (listRestPatternBwd (asElim κ) o) πs

-- σ, c desugar_bwd c
branchBwd_curried :: Elim 𝔹 -> Endo (Branch 𝔹)
branchBwd_curried σ (πs × s) = πs × exprBwd (patternsBwd σ πs) s

-- σ, c desugar_bwd c
branchBwd_uncurried :: Elim 𝔹 -> Endo (Pattern × Expr 𝔹)
branchBwd_uncurried σ (p × s) = p × exprBwd (asExpr (patternBwd σ p)) s

-- σ, cs desugar_bwd cs
branchesBwd_curried :: Elim 𝔹 -> Endo (NonEmptyList (Branch 𝔹))
branchesBwd_curried σ (NonEmptyList (b1 :| b2 : bs)) =
   NonEmptyList (branchBwd_curried σ b1 :| toList (branchesBwd_curried σ (NonEmptyList (b2 :| bs))))
branchesBwd_curried σ (NonEmptyList (b :| Nil)) =
   NonEmptyList (branchBwd_curried σ b :| Nil)

-- σ, cs desugar_bwd cs
branchesBwd_uncurried :: Elim 𝔹 -> Endo (NonEmptyList (Pattern × Expr 𝔹))
branchesBwd_uncurried σ (NonEmptyList (b1 :| b2 : bs)) =
   NonEmptyList (branchBwd_uncurried σ b1 :| toList (branchesBwd_uncurried σ (NonEmptyList (b2 :| bs))))
branchesBwd_uncurried σ (NonEmptyList (b :| Nil)) =
   NonEmptyList (branchBwd_uncurried σ b :| Nil)

-- κ, πs totalise_bwd κ', α
totaliseBwd :: Cont 𝔹 -> List (Pattern + ListRestPattern) -> Cont 𝔹 × 𝔹
totaliseBwd κ Nil                              = κ × false
totaliseBwd (ContExpr _) (_ : _)               = error absurd
totaliseBwd ContHole (_ : _)                   = error "todo"
totaliseBwd (ContElim ElimHole) _              = error "todo"
totaliseBwd (ContElim (ElimVar x κ)) (π : πs)  =
   case π of
      Left (PVar _)  -> first (\κ' -> ContElim (ElimVar x κ')) (totaliseBwd κ πs)
      Left _         -> error absurd
      Right _        -> error absurd
totaliseBwd (ContElim (ElimConstr m)) (π : πs) =
   case π of
      Left (PVar _) -> error absurd
      Left (PConstr c ps) ->
         first (\κ -> ContElim (ElimConstr (fromFoldable [c × κ])))
               (totaliseBwd (mustLookup c m) ((Left <$> ps) <> πs))
      Left PListEmpty ->
         first (\κ -> ContElim (ElimConstr (fromFoldable [cNil × κ])))
               (totaliseBwd (mustLookup cNil m) πs)
      Left (PListNonEmpty p o) ->
         first (\κ -> ContElim (ElimConstr (fromFoldable [cCons × κ])))
               (totaliseBwd (mustLookup cCons m) (Left p : Right o : πs))
      Right PEnd ->
         first (\κ -> ContElim (ElimConstr (fromFoldable [cNil × κ])))
               (totaliseBwd (mustLookup cNil m) πs)
      Right (PNext p o) ->
         first (\κ -> ContElim (ElimConstr (fromFoldable [cCons × κ])))
               (totaliseBwd (mustLookup cCons m) (Left p : Right o : πs))
