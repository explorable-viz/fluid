module DesugarBwd where

import Prelude hiding (absurd)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (applyN, on)
import Data.List (List(..), (:), (\\), singleton, zip)
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList)
import Data.Map (Map, fromFoldable)
import Data.NonEmpty ((:|))
import Data.Tuple (uncurry, fst, snd)
import Bindings (Binding, Bindings(..), (↦), (:+:), fromList)
import DataType (Ctr, arity, cCons, cNil, cTrue, cFalse, ctrs, dataTypeFor)
import DesugarFwd (elimBool, totaliseConstrFwd)
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), RecDefs, VarDef(..)) as E
import SExpr (
   Branch, Clause, Expr(..), ListRest(..), Pattern(..), ListRestPattern(..), Qualifier(..), RecDefs, VarDef(..), VarDefs
)
import Lattice (𝔹, (∨), expand)
import Util (Endo, type (+), type (×), (×), absurd, error, mustLookup, successful)

desugarBwd :: E.Expr 𝔹 -> Expr 𝔹 -> Expr 𝔹
desugarBwd = exprBwd

-- TODO: can probably lose the outer 'let' here.
varDefsBwd :: E.Expr 𝔹 -> VarDefs 𝔹 × Expr 𝔹 -> VarDefs 𝔹 × Expr 𝔹
varDefsBwd (E.Let (E.VarDef σ e1) e2) (NonEmptyList (VarDef π s1 :| Nil) × s2) =
   NonEmptyList (VarDef π (exprBwd e1 s1) :| Nil) × exprBwd e2 s2
varDefsBwd (E.Let (E.VarDef σ e1) e2) (NonEmptyList (VarDef π s1 :| d : ds) × s2) =
   let NonEmptyList (d' :| ds') × s2' = varDefsBwd e2 (NonEmptyList (d :| ds) × s2) in
   NonEmptyList (VarDef π (exprBwd e1 s1) :| d' : ds') × s2'
varDefsBwd _ (NonEmptyList (_ :| _) × _) = error absurd

recDefsBwd :: E.RecDefs 𝔹 -> RecDefs 𝔹 -> RecDefs 𝔹
recDefsBwd xσs xcs = join (recDefsBwd' xσs (groupBy (eq `on` fst) xcs))

recDefsBwd' :: E.RecDefs 𝔹 -> NonEmptyList (RecDefs 𝔹) -> NonEmptyList (RecDefs 𝔹)
recDefsBwd' Empty _                                             = error absurd
recDefsBwd' (Empty :+: x ↦ σ) (NonEmptyList (xcs :| Nil))       = NonEmptyList (recDefBwd (x ↦ σ) xcs :| Nil)
recDefsBwd' (_ :+: _ :+: _) (NonEmptyList (_ :| Nil))           = error absurd
recDefsBwd' (ρ :+: x ↦ σ) (NonEmptyList (xcs1 :| xcs2 : xcss))  =
   NonEmptyList (recDefBwd (x ↦ σ) xcs1 :| toList (recDefsBwd' ρ (NonEmptyList (xcs2 :| xcss))))

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
exprBwd e (Matrix _ s (x × y) s') =
   case expand e (E.Matrix false E.Hole (x × y) E.Hole) of
      E.Matrix α e1 _ e2 -> Matrix α (exprBwd e1 s) (x × y) (exprBwd e2 s')
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
exprBwd e (BinaryApp s1 op s2) =
   case expand e (E.App (E.App (E.Op op) E.Hole) E.Hole) of
      E.App (E.App (E.Op _) e1) e2 -> BinaryApp (exprBwd e1 s1) op (exprBwd e2 s2)
      _ -> error absurd
exprBwd e (Let ds s) =
   case expand e (E.Let (E.VarDef ElimHole E.Hole) E.Hole) of
      E.Let d e' -> uncurry Let (varDefsBwd (E.Let d e') (ds × s))
      _ -> error absurd
exprBwd e (LetRec xcs s) =
   case expand e (E.LetRec (fromList (toList (recDefHole <$> xcss))) E.Hole) of
      E.LetRec xσs e' -> LetRec (recDefsBwd xσs xcs) (exprBwd e' s)
      _ -> error absurd
      where
      -- repeat enough desugaring logic to determine shape of bindings
      recDefHole :: NonEmptyList (Clause 𝔹) -> Binding Elim 𝔹
      recDefHole xcs' = fst (head xcs') ↦ ElimHole
      xcss = groupBy (eq `on` fst) xcs :: NonEmptyList (NonEmptyList (Clause 𝔹))
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
exprBwd e (ListComp _ s_body (NonEmptyList (Guard (Constr _ c Nil) :| Nil))) | c == cTrue =
   case expand e (E.Constr false cCons (E.Hole : E.Constr false cNil Nil : Nil)) of
      E.Constr α2 cCons' (e' : E.Constr α1 _ Nil : Nil) ->
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
patternBwd (ElimVar x κ) (PVar _)               = κ
patternBwd ElimHole (PVar _)                    = ContHole
patternBwd ElimHole (PConstr c ps)              = argsBwd ContHole (Left <$> ps)
patternBwd (ElimConstr m) (PConstr c ps)        = argsBwd (mustLookup c m) (Left <$> ps)
patternBwd ElimHole (PListEmpty)                = ContHole
patternBwd (ElimConstr m) (PListEmpty)          = mustLookup cNil m
patternBwd ElimHole (PListNonEmpty p o)         = argsBwd ContHole (Left p : Right o : Nil)
patternBwd (ElimConstr m) (PListNonEmpty p o)   = argsBwd (mustLookup cCons m) (Left p : Right o : Nil)
patternBwd _ _                                  = error absurd

-- σ, o desugar_bwd κ
listRestPatternBwd :: Elim 𝔹 -> ListRestPattern -> Cont 𝔹
listRestPatternBwd (ElimVar _ _) _              = error absurd
listRestPatternBwd ElimHole PEnd                = ContHole
listRestPatternBwd (ElimConstr m) PEnd          = mustLookup cNil m
listRestPatternBwd ElimHole (PNext p o)         = argsBwd ContHole (Left p : Right o : Nil)
listRestPatternBwd (ElimConstr m) (PNext p o)   = argsBwd (mustLookup cCons m) (Left p : Right o : Nil)

argsBwd :: Cont 𝔹 -> List (Pattern + ListRestPattern) -> Cont 𝔹
argsBwd κ Nil              = κ
argsBwd κ (Left p : πs)    = argsBwd (patternBwd (asElim κ) p) πs
argsBwd κ (Right o : πs)   = argsBwd (listRestPatternBwd (asElim κ) o) πs

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
totaliseBwd κ Nil = κ × false
totaliseBwd κ (Left (PVar x) : πs) =
   case expand κ (ContElim (ElimVar x ContHole)) of
      ContElim (ElimVar _ κ') ->
         let κ'' × α = totaliseBwd κ' πs in
         ContElim (ElimVar x κ'') × α
      _ -> error absurd
totaliseBwd κ (π : πs) =
   let c × πs' = case π of
         Left (PVar _)              -> error absurd
         Left (PConstr c ps)        -> c × (Left <$> ps)
         Left PListEmpty            -> cNil × Nil
         Left (PListNonEmpty p o)   -> cCons × (Left p : Right o : Nil)
         Right PEnd                 -> cNil × Nil
         Right (PNext p o)          -> cCons × (Left p : Right o : Nil)
   -- use totaliseConstrFwd to construct "eliminator pattern" to match against
   in case expand κ (ContElim (ElimConstr (totaliseConstrFwd (c × ContHole) false))) of
      ContElim (ElimConstr m) ->
         let κ' × α = totaliseConstrBwd m c
             κ'' × β = totaliseBwd κ' (πs' <> πs) in
         ContElim (ElimConstr (fromFoldable (singleton (c × κ'')))) × (α ∨ β)
      _ -> error absurd

-- Discard all synthesised branches, returning the original singleton branch for c, plus join of annotations
-- on the empty lists used for bodies of synthesised branches.
totaliseConstrBwd :: Map Ctr (Cont 𝔹) -> Ctr -> Cont 𝔹 × 𝔹
totaliseConstrBwd m c =
   let cs = ctrs (successful (dataTypeFor c)) \\ singleton c in
   mustLookup c m × foldl (∨) false (map (bodyAnn <<< body) cs)
   where
      body :: Ctr -> Cont 𝔹
      body c' = applyN unargument (successful (arity c')) (mustLookup c' m)

      unargument :: Cont 𝔹 -> Cont 𝔹
      unargument (ContElim (ElimVar _ κ)) = κ
      unargument _                        = error absurd

      bodyAnn :: Cont 𝔹 -> 𝔹
      bodyAnn (ContExpr (E.Constr α c' Nil)) | c' == cNil = α
      bodyAnn _                                           = error absurd
