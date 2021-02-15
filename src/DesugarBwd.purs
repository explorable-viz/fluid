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
import SExpr (Clause, Expr(..), ListRest(..), Pattern(..), ListPatternRest(..), Qualifier(..), VarDef(..))
import Lattice (𝔹, (∨))
import Util (Endo, type(+), type (×), (×), absurd, assert, mustLookup, error)

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
fromRecDef (x ↦ σ) = map (x × _) <<< desugarBwd σ <<< map snd

instance expr :: DesugarBwd (E.Expr Boolean) (Expr Boolean) where
   desugarBwd (E.Var x) (Var _)                             = Var x
   desugarBwd (E.Op op) (Op _)                              = Op op
   desugarBwd (E.Int α n) (Int _ _)                         = Int α n
   desugarBwd (E.Float α n) (Float _ _)                     = Float α n
   desugarBwd (E.Str α s) (Str _ _)                         = Str α s
   desugarBwd (E.Constr α c es) (Constr _ _ es')            = Constr α c (uncurry desugarBwd <$> zip es es')
   desugarBwd (E.Matrix α e (x × y) e') (Matrix _ s _ s')   = Matrix α (desugarBwd e s) (x × y) (desugarBwd e' s')
   desugarBwd (E.Lambda σ) (Lambda bs)                      = Lambda (desugarBwd σ bs)
   desugarBwd (E.App e1 e2) (App s1 s2)                     = App (desugarBwd e1 s1) (desugarBwd e2 s2)
   desugarBwd (E.App (E.Lambda σ) e) (MatchAs s bs)         = MatchAs (desugarBwd e s) (desugarBwd σ bs)
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
      ListNonEmpty α (desugarBwd e s) (desugarBwd e' l)
   desugarBwd (E.App (E.App (E.Var "enumFromTo") e1) e2) (ListEnum s1 s2) =
      ListEnum (desugarBwd e1 s1) (desugarBwd e2 s2)
   -- list-comp-done
   desugarBwd (E.Constr α2 cCons' (e : (E.Constr α1 cNil' Nil) : Nil))
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
              (ListComp α0 s2 (NonEmptyList ((Declaration (VarDef π s1)) :| q : qs))) =
      case desugarBwd σ (NonEmptyList (π :| Nil) × (ListComp α0 s2 (NonEmptyList (q :| qs)))) of
         _ × ListComp β s2' (NonEmptyList (q' :| qs')) ->
            ListComp β s2' (NonEmptyList ((Declaration (VarDef π (desugarBwd e s1))) :| q' : qs'))
         _ × _ -> error absurd
   -- list-comp-gen
   desugarBwd (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e1)
              (ListComp α s2 (NonEmptyList (Generator p s1 :| q : qs))) =
      let σ' × β = totalise_bwd (ContElim σ) (Left p : Nil) in
      case desugarBwd (asExpr (desugarPatternBwd (asElim σ') p)) (ListComp α s2 (NonEmptyList (q :| qs))) of
         ListComp β' s2' (NonEmptyList (q' :| qs')) ->
            ListComp (β ∨ β') s2' (NonEmptyList (Generator p (desugarBwd e1 s1) :| q' : qs'))
         _ -> error absurd
   desugarBwd (E.Hole) s = error "todo"
   desugarBwd _ _ = error absurd

instance listRest :: DesugarBwd (E.Expr Boolean) (ListRest Boolean) where
   desugarBwd e l@(End _) = case e of
      E.Constr α c Nil ->
         assert (c == cNil) $
         End α
      E.Constr _ _ _ -> error absurd
      E.Hole -> desugarBwd (E.Constr false cNil Nil) l
      _ -> error absurd
   desugarBwd e l@(Next _ s l') = case e of
      E.Constr α c (e1 : e2 : Nil) ->
         assert (c == cCons) $
         Next α (desugarBwd e1 s) (desugarBwd e2 l')
      E.Constr _ _ _ -> error absurd
      E.Hole -> desugarBwd (E.Constr false cCons (E.Hole : E.Hole : Nil)) l
      _ -> error absurd

class DesugarPatternBwd a b | a -> b where
   desugarPatternBwd :: Elim 𝔹 -> a -> b

-- σ, ps desugar_bwd e
instance patterns :: DesugarPatternBwd (NonEmptyList Pattern) (E.Expr Boolean) where
   desugarPatternBwd σ (NonEmptyList (π :| Nil)) = asExpr (desugarPatternBwd σ π)
   desugarPatternBwd σ (NonEmptyList (π :| π' : πs)) =
      case asExpr (desugarPatternBwd σ π) of
         E.Lambda σ' -> desugarPatternBwd σ' (NonEmptyList (π' :| πs))
         _ -> error absurd

-- σ, p desugar_bwd κ
instance pattern :: DesugarPatternBwd Pattern (Cont Boolean) where
   desugarPatternBwd ElimHole _                          = error "todo"
   desugarPatternBwd (ElimVar x κ) (PVar _)              = κ
   desugarPatternBwd (ElimConstr m) (PConstr c ps)       = desugarArgsBwd (mustLookup c m) (Left <$> ps)
   desugarPatternBwd (ElimConstr m) (PListEmpty)         = mustLookup cNil m
   desugarPatternBwd (ElimConstr m) (PListNonEmpty p o)  = desugarArgsBwd (mustLookup cCons m) (Left p : Right o : Nil)
   desugarPatternBwd _ _                                 = error absurd

desugarArgsBwd :: Cont 𝔹 -> List (Pattern + ListPatternRest) -> Cont 𝔹
desugarArgsBwd κ Nil = κ
desugarArgsBwd κ (Left p : πs) = desugarArgsBwd (desugarPatternBwd (asElim κ) p) πs
desugarArgsBwd κ (Right o : πs) = desugarArgsBwd (desugarPatternBwd (asElim κ) o) πs

-- σ, o desugar_bwd κ
instance patternRest :: DesugarPatternBwd ListPatternRest (Cont Boolean) where
   desugarPatternBwd ElimHole _                 = error "todo"
   desugarPatternBwd (ElimVar _ _) _            = error absurd
   desugarPatternBwd (ElimConstr m) PEnd        = mustLookup cNil m
   desugarPatternBwd (ElimConstr m) (PNext p o) = desugarArgsBwd (mustLookup cCons m) (Left p : Right o : Nil)

-- σ, c desugar_bwd c
instance branch :: DesugarBwd (Elim Boolean) (NonEmptyList Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) =
      πs × desugarBwd (desugarPatternBwd σ πs) s

-- σ, c desugar_bwd c
instance branchUncurried :: DesugarBwd (Elim Boolean) (Pattern × Expr Boolean) where
   desugarBwd σ (π × s) =
      π × desugarBwd (asExpr (desugarPatternBwd σ π)) s

-- σ, cs desugar_bwd cs
instance branches :: DesugarBwd (Elim Boolean) (NonEmptyList (NonEmptyList Pattern × Expr Boolean)) where
   desugarBwd σ (NonEmptyList (b1 :| b2 : bs)) =
      NonEmptyList (desugarBwd σ b1 :| toList (desugarBwd σ (NonEmptyList (b2 :| bs))))
   desugarBwd σ (NonEmptyList (b :| Nil)) =
      NonEmptyList (desugarBwd σ b :| Nil)

-- σ, cs desugar_bwd cs
instance branchesUncurried :: DesugarBwd (Elim Boolean) (NonEmptyList (Pattern × Expr Boolean)) where
   desugarBwd σ (NonEmptyList (b1 :| b2 : bs)) =
      NonEmptyList (desugarBwd σ b1 :| toList (desugarBwd σ (NonEmptyList (b2 :| bs))))
   desugarBwd σ (NonEmptyList (b :| Nil)) =
      NonEmptyList (desugarBwd σ b :| Nil)

-- κ, πs totalise_bwd κ', α
totalise_bwd :: Cont 𝔹 -> List (Pattern + ListPatternRest) -> Cont 𝔹 × 𝔹
totalise_bwd κ Nil                              = κ × false
totalise_bwd (ContExpr _) (_ : _)               = error absurd
totalise_bwd ContHole (_ : _)                   = error "todo"
totalise_bwd (ContElim ElimHole) _              = error "todo"
totalise_bwd (ContElim (ElimVar x κ)) (π : πs)  =
   case π of
      Left (PVar _) ->
         first (\κ' -> ContElim (ElimVar x κ')) (totalise_bwd κ πs)
      Left _ -> error absurd
      Right _ -> error absurd
totalise_bwd (ContElim (ElimConstr m)) (π : πs) =
   case π of
      Left (PVar _) -> error absurd
      Left (PConstr c ps) ->
         first (\κ -> ContElim (ElimConstr (fromFoldable [c × κ])))
               (totalise_bwd (mustLookup c m) ((Left <$> ps) <> πs))
      Left PListEmpty ->
         first (\κ -> ContElim (ElimConstr (fromFoldable [cNil × κ])))
               (totalise_bwd (mustLookup cNil m) πs)
      Left (PListNonEmpty p o) ->
         first (\κ -> ContElim (ElimConstr (fromFoldable [cCons × κ])))
               (totalise_bwd (mustLookup cCons m) (Left p : Right o : πs))
      Right PEnd ->
         first (\κ -> ContElim (ElimConstr (fromFoldable [cNil × κ])))
               (totalise_bwd (mustLookup cNil m) πs)
      Right (PNext p o) ->
         first (\κ -> ContElim (ElimConstr (fromFoldable [cCons × κ])))
               (totalise_bwd (mustLookup cCons m) (Left p : Right o : πs))
