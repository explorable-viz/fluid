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
import Bindings (Binding, Bindings(..), (↦), (:+:))
import DataType (cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), VarDef(..)) as E
import SExpr (Branch, Clause, Expr(..), ListRest(..), Pattern(..), ListRestPattern(..), Qualifier(..), VarDef(..))
import Lattice (𝔹, (∨), expand)
import Util (Endo, type(+), type (×), (×), absurd, mustLookup, error)

qualTrue :: 𝔹 -> Qualifier 𝔹
qualTrue α = Guard (Constr α cTrue Nil)

snil :: 𝔹 -> Expr 𝔹
snil α = Constr α cNil Nil

class DesugarBwd a b where
   desugarBwd :: a -> Endo b

instance varDef  :: DesugarBwd (E.VarDef Boolean) (VarDef Boolean) where
   desugarBwd (E.VarDef σ e) (VarDef π s) = VarDef π (desugarBwd e s)

instance varDefs :: DesugarBwd (E.Expr Boolean) (NonEmptyList (VarDef Boolean) × Expr Boolean) where
   desugarBwd (E.Let (E.VarDef σ e1) e2) (NonEmptyList (VarDef π s1 :| Nil) × s2) =
      (NonEmptyList (VarDef π (desugarBwd e1 s1) :| Nil) × desugarBwd e2 s2)
   desugarBwd (E.Let (E.VarDef σ e1) e2) (NonEmptyList (VarDef π s1 :| d : ds) × s2) =
      let NonEmptyList (d' :| ds') × s2' = desugarBwd e2 (NonEmptyList (d :| ds) × s2) in
      NonEmptyList (VarDef π (desugarBwd e1 s1) :| d' : ds') × s2'
   desugarBwd _ (NonEmptyList (_ :| _) × _) = error absurd

instance recDefs :: DesugarBwd (Bindings Elim Boolean) (NonEmptyList (String × (NonEmptyList Pattern × Expr Boolean))) where
   desugarBwd xσs xcs = join (zipRecDefs xσs (reverse (groupBy (eq `on` fst) xcs)))

zipRecDefs :: Bindings Elim 𝔹 -> Endo (NonEmptyList (NonEmptyList (Clause 𝔹)))
zipRecDefs Empty _ = error absurd
zipRecDefs (Empty :+: x ↦ σ) (NonEmptyList (xcs :| Nil)) =
   NonEmptyList (fromRecDef (x ↦ σ) xcs :| Nil)
zipRecDefs (_ :+: _ :+: _) (NonEmptyList (_ :| Nil)) = error absurd
zipRecDefs (ρ :+: x ↦ σ) (NonEmptyList (xcs1 :| xcs2 : xcss)) =
   NonEmptyList (fromRecDef (x ↦ σ) xcs1 :| toList (zipRecDefs ρ (NonEmptyList (xcs2 :| xcss))))

fromRecDef :: Binding Elim 𝔹 -> Endo (NonEmptyList (Clause 𝔹))
fromRecDef (x ↦ σ) = map (x × _) <<< branchesBwd_curried σ <<< map snd

instance expr :: DesugarBwd (E.Expr Boolean) (Expr Boolean) where
   desugarBwd (E.Var x) (Var _)                             = Var x
   desugarBwd (E.Op op) (Op _)                              = Op op
   desugarBwd (E.Int α n) (Int _ _)                         = Int α n
   desugarBwd (E.Float α n) (Float _ _)                     = Float α n
   desugarBwd (E.Str α s) (Str _ _)                         = Str α s
   desugarBwd (E.Constr α c es) (Constr _ _ es')            = Constr α c (uncurry desugarBwd <$> zip es es')
   desugarBwd (E.Matrix α e (x × y) e') (Matrix _ s _ s')   = Matrix α (desugarBwd e s) (x × y) (desugarBwd e' s')
   desugarBwd (E.Lambda σ) (Lambda bs)                      = Lambda (branchesBwd_curried σ bs)
   desugarBwd (E.App e1 e2) (App s1 s2)                     = App (desugarBwd e1 s1) (desugarBwd e2 s2)
   desugarBwd (E.App (E.Lambda σ) e) (MatchAs s bs)         = MatchAs (desugarBwd e s) (branchesBwd_uncurried σ bs)
   desugarBwd (E.App (E.Lambda (ElimConstr m)) e1) (IfElse s1 s2 s3) = do
      IfElse (desugarBwd e1 s1)
             (desugarBwd (asExpr (mustLookup cTrue m)) s2)
             (desugarBwd (asExpr (mustLookup cFalse m)) s3)
   desugarBwd (E.BinaryApp e1 x e2) (BinaryApp s1 _ s2)     = BinaryApp (desugarBwd e1 s1) x (desugarBwd e2 s2)
   desugarBwd (E.Let d e) (Let ds s)                        = uncurry Let (desugarBwd (E.Let d e) (ds × s))
   desugarBwd (E.LetRec xσs e) (LetRec xcs s)               = LetRec (desugarBwd xσs xcs) (desugarBwd e s)
   desugarBwd (E.Constr α c Nil) (ListEmpty _) | c == cNil  =
      ListEmpty α
   desugarBwd (E.Constr α c (e : e' : Nil)) (ListNonEmpty _ s l) | c == cCons =
      ListNonEmpty α (desugarBwd e s) (listRestBwd e' l)
   desugarBwd (E.App (E.App (E.Var "enumFromTo") e1) e2) (ListEnum s1 s2) =
      ListEnum (desugarBwd e1 s1) (desugarBwd e2 s2)
   -- list-comp-done
   desugarBwd (E.Constr α2 cCons' (e : E.Constr α1 cNil' Nil : Nil))
              (ListComp _ s_body (NonEmptyList (Guard (Constr _ cTrue' Nil) :| Nil)))
      | cCons' == cCons , cNil' == cNil, cTrue' == cTrue =
      ListComp (α1 ∨ α2) (desugarBwd e s_body)
                         (NonEmptyList (Guard (Constr (α1 ∨ α2) cTrue Nil) :| Nil))
   -- list-comp-last
   desugarBwd e (ListComp α s (NonEmptyList (q :| Nil))) =
      case desugarBwd e (ListComp α s (NonEmptyList (q :| qualTrue true : Nil))) of
         ListComp β s' (NonEmptyList (q' :| (Guard (Constr _ c Nil)) : Nil)) | c == cTrue ->
            (ListComp β s' (NonEmptyList (q' :| Nil)))
         _ -> error absurd
   -- list-comp-guard
   desugarBwd (E.App (E.Lambda (ElimConstr m)) e2)
              (ListComp α0 s1 (NonEmptyList (Guard s2 :| q : qs))) =
      case desugarBwd (asExpr (mustLookup cTrue m)) (ListComp α0 s1 (NonEmptyList (q :| qs))) ×
           desugarBwd (asExpr (mustLookup cFalse m)) (snil true) of
         ListComp β s1' (NonEmptyList (q' :| qs')) × Constr α c Nil | c == cNil ->
            ListComp (α ∨ β) s1' (NonEmptyList (Guard (desugarBwd e2 s2) :| q' : qs'))
         _ × _ -> error absurd
   -- list-comp-decl
   desugarBwd (E.App (E.Lambda σ) e)
              (ListComp α0 s2 (NonEmptyList (Declaration (VarDef π s1) :| q : qs))) =
      case branchBwd_curried σ (NonEmptyList (π :| Nil) × (ListComp α0 s2 (NonEmptyList (q :| qs)))) of
         _ × ListComp β s2' (NonEmptyList (q' :| qs')) ->
            ListComp β s2' (NonEmptyList ((Declaration (VarDef π (desugarBwd e s1))) :| q' : qs'))
         _ × _ -> error absurd
   -- list-comp-gen
   desugarBwd e s@(ListComp α s2 (NonEmptyList (Generator p s1 :| q : qs))) =
      case expand e (E.App (E.App (E.Var "concatMap") (E.Lambda ElimHole)) E.Hole) of
         E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e1 ->
            let σ' × β = totaliseBwd (ContElim σ) (Left p : Nil) in
            case desugarBwd (asExpr (patternBwd (asElim σ') p)) (ListComp α s2 (NonEmptyList (q :| qs))) of
               ListComp β' s2' (NonEmptyList (q' :| qs')) ->
                  ListComp (β ∨ β') s2' (NonEmptyList (Generator p (desugarBwd e1 s1) :| q' : qs'))
               _ -> error absurd
         _ -> error absurd
   desugarBwd (E.Hole) s = error "todo"
   desugarBwd _ _ = error absurd

-- e, l desugar_bwd l
listRestBwd :: E.Expr 𝔹 -> Endo (ListRest 𝔹)
listRestBwd e l@(End _) =
   case e of
      E.Constr α c Nil | c == cNil  -> End α
      E.Hole                        -> listRestBwd (E.Constr false cNil Nil) l
      _                             -> error absurd
listRestBwd e l@(Next _ s l') =
   case e of
      E.Constr α c (e1 : e2 : Nil) | c == cCons -> Next α (desugarBwd e1 s) (listRestBwd e2 l')
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
branchBwd_curried σ (πs × s) = πs × desugarBwd (patternsBwd σ πs) s

-- σ, c desugar_bwd c
branchBwd_uncurried :: Elim 𝔹 -> Endo (Pattern × Expr 𝔹)
branchBwd_uncurried σ (p × s) = p × desugarBwd (asExpr (patternBwd σ p)) s

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
