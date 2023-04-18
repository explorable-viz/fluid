module SExpr where

import Prelude hiding (absurd, top)

import Bindings (Bind, Var, varAnon, (↦), keys)
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl)
import Data.Function (applyN, on)
import Data.List (List(..), (:), (\\), length, sortBy, zipWith)
import Data.List (singleton) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList, singleton)
import Data.List.NonEmpty (singleton) as NE
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Profunctor.Strong (first, (***))
import Data.Set (toUnfoldable) as S
import Data.Traversable (traverse)
import Data.Tuple (uncurry, fst, snd)
import DataType (Ctr, arity, checkArity, ctrs, cCons, cFalse, cNil, cTrue, dataTypeFor)
import Desugarable (class Desugarable, desugBwd', desugFwd')
import Dict (Dict, asSingletonMap, get)
import Dict (fromFoldable, singleton) as D
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), Module(..), RecDefs, VarDef(..)) as E
import Lattice (class JoinSemilattice, (∨), bot, definedJoin, neg, maybeJoin, class BoundedJoinSemilattice, Raw)
import Partial.Unsafe (unsafePartial)
import Util (type (+), type (×), Endo, MayFail, absurd, error, successful, unimplemented, (×))
import Util.Pair (Pair(..))

-- Surface language expressions.
data Expr a
   = Var Var
   | Op Var
   | Int a Int
   | Float a Number
   | Str a String
   | Constr a Ctr (List (Expr a))
   | Record a (List (Bind (Expr a)))
   | Dictionary a (List (Pair (Expr a)))
   | Matrix a (Expr a) (Var × Var) (Expr a)
   | Lambda (Clauses a)
   | Project (Expr a) Var
   | App (Expr a) (Expr a)
   | BinaryApp (Expr a) Var (Expr a)
   | MatchAs (Expr a) (NonEmptyList (Pattern × Expr a))
   | IfElse (Expr a) (Expr a) (Expr a)
   | ListEmpty a -- called [] in the paper
   | ListNonEmpty a (Expr a) (ListRest a)
   | ListEnum (Expr a) (Expr a)
   | ListComp a (Expr a) (List (Qualifier a))
   | Let (VarDefs a) (Expr a)
   | LetRec (RecDefs a) (Expr a)

data ListRest a
   = End a
   | Next a (Expr a) (ListRest a)

data Pattern
   = PVar Var
   | PConstr Ctr (List Pattern)
   | PRecord (List (Bind Pattern))
   | PListEmpty
   | PListNonEmpty Pattern ListRestPattern

data ListRestPattern
   = PEnd
   | PNext Pattern ListRestPattern

newtype Clause a = Clause (NonEmptyList Pattern × Expr a)
type Branch a = Var × Clause a
newtype Clauses a = Clauses (NonEmptyList (Clause a))
newtype RecDef a = RecDef (NonEmptyList (Branch a))
type RecDefs a = NonEmptyList (Branch a)

-- The pattern/expr relationship is different to the one in branch (the expr is the "argument", not the "body").
-- Using a data type makes for easier overloading.
data VarDef a = VarDef Pattern (Expr a)
type VarDefs a = NonEmptyList (VarDef a)

data Qualifier a
   = Guard (Expr a)
   | Generator Pattern (Expr a)
   | Declaration (VarDef a) -- could allow VarDefs instead

data Module a = Module (List (VarDefs a + RecDefs a))

instance Desugarable Expr E.Expr where
   desugFwd = exprFwd
   desugBwd = exprBwd

instance Desugarable ListRest E.Expr where
   desugFwd = listRestFwd
   desugBwd = listRestBwd

instance Desugarable Clauses Elim where
   desugFwd = clausesFwd
   desugBwd = clausesBwd

desugarModuleFwd :: forall a. JoinSemilattice a => Module a -> MayFail (E.Module a)
desugarModuleFwd = moduleFwd

-- helpers
enil :: forall a. a -> E.Expr a
enil α = E.Constr α cNil Nil

econs :: forall a. a -> E.Expr a -> E.Expr a -> E.Expr a
econs α e e' = E.Constr α cCons (e : e' : Nil)

elimBool :: forall a. Cont a -> Cont a -> Elim a
elimBool κ κ' = ElimConstr (D.fromFoldable [ cTrue × κ, cFalse × κ' ])

-- Module. Surface language supports "blocks" of variable declarations; core does not. Currently no backward.
moduleFwd :: forall a. JoinSemilattice a => Module a -> MayFail (E.Module a)
moduleFwd (Module ds) = E.Module <$> traverse varDefOrRecDefsFwd (join (flatten <$> ds))
   where
   varDefOrRecDefsFwd :: VarDef a + RecDefs a -> MayFail (E.VarDef a + E.RecDefs a)
   varDefOrRecDefsFwd (Left d) = Left <$> varDefFwd d
   varDefOrRecDefsFwd (Right xcs) = Right <$> recDefsFwd xcs

   flatten :: VarDefs a + RecDefs a -> List (VarDef a + RecDefs a)
   flatten (Left ds') = Left <$> toList ds'
   flatten (Right δ) = pure (Right δ)

varDefFwd :: forall a. JoinSemilattice a => VarDef a -> MayFail (E.VarDef a)
varDefFwd (VarDef π s) = E.VarDef <$> pattContFwd π (ContNone :: Cont a) <*> desugFwd' s

-- VarDefs
varDefsFwd :: forall a. JoinSemilattice a => VarDefs a × Expr a -> MayFail (E.Expr a)
varDefsFwd (NonEmptyList (d :| Nil) × s) =
   E.Let <$> varDefFwd d <*> desugFwd' s
varDefsFwd (NonEmptyList (d :| d' : ds) × s) =
   E.Let <$> varDefFwd d <*> varDefsFwd (NonEmptyList (d' :| ds) × s)

varDefsBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw VarDefs × Raw Expr -> VarDefs a × Expr a
varDefsBwd (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π _ :| Nil) × _) =
   NonEmptyList (VarDef π (desugBwd' e1) :| Nil) × desugBwd' e2
varDefsBwd (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π _ :| d : ds) × s2) =
   let
      NonEmptyList (d' :| ds') × s2' = varDefsBwd e2 (NonEmptyList (d :| ds) × s2)
   in
      NonEmptyList (VarDef π (desugBwd' e1) :| d' : ds') × s2'
varDefsBwd _ (NonEmptyList (_ :| _) × _) = error absurd

-- RecDefs
-- In the formalism, "group by name" is part of the syntax.
recDefsFwd :: forall a. JoinSemilattice a => RecDefs a -> MayFail (E.RecDefs a)
recDefsFwd xcs = D.fromFoldable <$> traverse recDefFwd xcss
   where
   xcss = map RecDef (groupBy (eq `on` fst) xcs) :: NonEmptyList (RecDef a)

recDefsBwd :: forall a. BoundedJoinSemilattice a => E.RecDefs a -> Raw RecDefs -> RecDefs a
recDefsBwd ρ xcs = join (go (groupBy (eq `on` fst) xcs))
   where
   go :: NonEmptyList (Raw RecDefs) -> NonEmptyList (RecDefs a)
   go (NonEmptyList (xcs1 :| xcss)) =
      let
         x = fst (head xcs1)
         xcss' = case xcss of
            Nil -> Nil
            xcs2 : xcss'' -> toList (go (NonEmptyList (xcs2 :| xcss'')))
      in
         NonEmptyList (unwrap (recDefBwd (x ↦ get x ρ) (RecDef xcs1)) :| xcss')

-- RecDef
recDefFwd :: forall a. JoinSemilattice a => RecDef a -> MayFail (Bind (Elim a))
recDefFwd xcs = (fst (head (unwrap xcs)) ↦ _) <$> clausesFwd (Clauses (snd <$> unwrap xcs))

recDefBwd :: forall a. BoundedJoinSemilattice a => Bind (Elim a) -> Raw RecDef -> RecDef a
recDefBwd (x ↦ σ) (RecDef bs) = RecDef ((x × _) <$> unwrap (clausesBwd σ (Clauses (snd <$> bs))))

-- Expr
exprFwd :: forall a. JoinSemilattice a => Expr a -> MayFail (E.Expr a)
exprFwd (Var x) = pure (E.Var x)
exprFwd (Op op) = pure (E.Op op)
exprFwd (Int α n) = pure (E.Int α n)
exprFwd (Float α n) = pure (E.Float α n)
exprFwd (Str α s) = pure (E.Str α s)
exprFwd (Constr α c ss) = E.Constr α c <$> traverse desugFwd' ss
exprFwd (Record α xss) = E.Record α <$> D.fromFoldable <$> traverse (traverse desugFwd') xss
exprFwd (Dictionary α sss) = E.Dictionary α <$> traverse (traverse desugFwd') sss
exprFwd (Matrix α s (x × y) s') = E.Matrix α <$> desugFwd' s <@> x × y <*> desugFwd' s'
exprFwd (Lambda bs) = E.Lambda <$> clausesFwd bs
exprFwd (Project s x) = E.Project <$> desugFwd' s <@> x
exprFwd (App s1 s2) = E.App <$> desugFwd' s1 <*> desugFwd' s2
exprFwd (BinaryApp s1 op s2) = E.App <$> (E.App (E.Op op) <$> desugFwd' s1) <*> desugFwd' s2
exprFwd (MatchAs s bs) =
   E.App <$> (E.Lambda <$> clausesFwd (Clauses (Clause <$> first singleton <$> bs))) <*> desugFwd' s
exprFwd (IfElse s1 s2 s3) =
   E.App <$> (E.Lambda <$> (elimBool <$> (ContExpr <$> desugFwd' s2) <*> (ContExpr <$> desugFwd' s3))) <*> desugFwd' s1
exprFwd (ListEmpty α) = pure (enil α)
exprFwd (ListNonEmpty α s l) = econs α <$> desugFwd' s <*> desugFwd' l
exprFwd (ListEnum s1 s2) = E.App <$> ((E.App (E.Var "enumFromTo")) <$> desugFwd' s1) <*> desugFwd' s2
exprFwd (ListComp α s qs) = listCompFwd' (α × s × qs)
exprFwd (Let ds s) = varDefsFwd (ds × s)
exprFwd (LetRec xcs s) = E.LetRec <$> recDefsFwd xcs <*> desugFwd' s

exprBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw Expr -> Expr a
exprBwd (E.Var _) (Var x) = Var x
exprBwd (E.Op _) (Op op) = Op op
exprBwd (E.Int α _) (Int _ n) = Int α n
exprBwd (E.Float α _) (Float _ n) = Float α n
exprBwd (E.Str α _) (Str _ str) = Str α str
exprBwd (E.Constr α _ es) (Constr _ c _) = Constr α c (desugBwd' <$> es)
exprBwd (E.Record α xes) (Record _ xss) =
   Record α $ xss <#> \(x ↦ _) -> x ↦ desugBwd' (get x xes)
exprBwd (E.Dictionary α ees) (Dictionary _ sss) =
   Dictionary α (zipWith (\(Pair e e') (Pair _ _) -> Pair (desugBwd' e) (desugBwd' e')) ees sss)
exprBwd (E.Matrix α e1 _ e2) (Matrix _ _ (x × y) _) =
   Matrix α (desugBwd' e1) (x × y) (desugBwd' e2)
exprBwd (E.Lambda σ) (Lambda bs) = Lambda (clausesBwd σ bs)
exprBwd (E.Project e _) (Project _ x) = Project (desugBwd' e) x
exprBwd (E.App e1 e2) (App _ _) = App (desugBwd' e1) (desugBwd' e2)
exprBwd (E.App (E.Lambda σ) e) (MatchAs _ bs) =
   MatchAs (desugBwd' e)
      (first head <$> unwrap <$> unwrap (clausesBwd σ (Clauses (Clause <$> first NE.singleton <$> bs))))
exprBwd (E.App (E.Lambda (ElimConstr m)) e1) (IfElse _ _ _) =
   IfElse (desugBwd' e1)
      (desugBwd' (asExpr (get cTrue m)))
      (desugBwd' (asExpr (get cFalse m)))
exprBwd (E.App (E.App (E.Op _) e1) e2) (BinaryApp _ op _) =
   BinaryApp (desugBwd' e1) op (desugBwd' e2)
exprBwd (E.Let d e) (Let ds s) = uncurry Let (varDefsBwd (E.Let d e) (ds × s))
exprBwd (E.LetRec xσs e) (LetRec xcs _) = LetRec (recDefsBwd xσs xcs) (desugBwd' e)
exprBwd (E.Constr α _ Nil) (ListEmpty _) = ListEmpty α
exprBwd (E.Constr α _ (e1 : e2 : Nil)) (ListNonEmpty _ _ _) =
   ListNonEmpty α (desugBwd' e1) (desugBwd' e2)
exprBwd (E.App (E.App (E.Var "enumFromTo") e1) e2) (ListEnum _ _) =
   ListEnum (desugBwd' e1) (desugBwd' e2)
exprBwd e (ListComp _ s qs) =
   let α × s' × qs' = listCompBwd' e (s × qs) in ListComp α s' qs'
exprBwd _ _ = error absurd

-- ListRest
listRestFwd :: forall a. JoinSemilattice a => ListRest a -> MayFail (E.Expr a)
listRestFwd (End α) = pure (enil α)
listRestFwd (Next α s l) = econs α <$> desugFwd' s <*> desugFwd' l

listRestBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw ListRest -> ListRest a
listRestBwd (E.Constr α _ _) (End _) = End α
listRestBwd (E.Constr α _ (e1 : e2 : Nil)) (Next _ _ _) =
   Next α (desugBwd' e1) (desugBwd' e2)
listRestBwd _ _ = error absurd

listCompFwd' :: forall a. JoinSemilattice a => a × Expr a × List (Qualifier a) -> MayFail (E.Expr a)
-- | list-comp-done
listCompFwd' (α × s_body × Nil) =
   econs α <$> desugFwd' s_body <@> enil α
-- | list-comp-guard
listCompFwd' (α × s_body × Guard s : qs) = do
   e <- desugFwd' (ListComp α s_body qs)
   E.App (E.Lambda (elimBool (ContExpr e) (ContExpr (enil α)))) <$> desugFwd' s
-- | list-comp-decl
listCompFwd' (α × s_body × Declaration (VarDef π s) : qs) = do
   e <- ContExpr <$> desugFwd' (ListComp α s_body qs)
   σ <- pattContFwd π e
   E.App (E.Lambda σ) <$> desugFwd' s
-- | list-comp-gen
listCompFwd' (α × s_body × Generator p s : qs) = do
   e <- ContExpr <$> desugFwd' (ListComp α s_body qs)
   σ <- pattContFwd p e
   E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (orElseFwd (ContElim σ) α)))) <$> desugFwd' s

listCompBwd'
   :: forall a
    . BoundedJoinSemilattice a
   => E.Expr a
   -> Raw Expr × List (Raw Qualifier)
   -> a × Expr a × List (Qualifier a)
-- | list-comp-done
listCompBwd' (E.Constr α2 c (e : E.Constr α1 c' Nil : Nil)) (_ × Nil) | c == cCons && c' == cNil =
   (α1 ∨ α2) × desugBwd' e × Nil
-- list-comp-guard
listCompBwd' (E.App (E.Lambda (ElimConstr m)) e2) (_ × Guard _ : _) =
   case
      desugBwd' (asExpr (get cTrue m)) × asExpr (get cFalse m)
      of
      ListComp β s1' qs' × E.Constr α c Nil | c == cNil ->
         (α ∨ β) × s1' × Guard (desugBwd' e2) : qs'
      _ × _ -> error absurd
-- list-comp-decl
listCompBwd' (E.App (E.Lambda σ) e1) (_ × Declaration (VarDef π _) : _) =
   case desugBwd' (asExpr (pattContBwd π σ)) of
      ListComp β s2' qs' ->
         β × s2' × Declaration (VarDef π (desugBwd' e1)) : qs'
      _ -> error absurd
-- list-comp-gen
listCompBwd' (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e1) (_ × Generator p _ : _) =
   let
      σ' × β = orElseBwd (ContElim σ) (Left p : Nil)
   in
      case desugBwd' (asExpr (pattContBwd p (asElim σ'))) of
         ListComp β' s2' qs' ->
            (β ∨ β') × s2' × Generator p (desugBwd' e1) : qs'
         _ -> error absurd
listCompBwd' _ _ = error absurd

-- List Qualifier × Expr
listCompFwd :: forall a. JoinSemilattice a => List (Qualifier a) × Expr a × a -> MayFail (E.Expr a)
listCompFwd (Nil × s × _) = desugFwd' s
listCompFwd ((Guard s : qs) × s' × α) = do
   e <- listCompFwd (qs × s' × α)
   E.App (E.Lambda (elimBool (ContExpr e) (ContExpr (enil α)))) <$> desugFwd' s
listCompFwd ((Declaration (VarDef π s) : qs) × s' × α) = do
   e <- ContExpr <$> listCompFwd (qs × s' × α)
   σ <- pattContFwd π e
   E.App (E.Lambda σ) <$> desugFwd' s
listCompFwd ((Generator p s : qs) × s' × α) = do
   e <- ContExpr <$> listCompFwd (qs × s' × α)
   σ <- pattContFwd p e
   E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (orElseFwd (ContElim σ) α)))) <$> desugFwd' s

listCompBwd
   :: forall a
    . BoundedJoinSemilattice a
   => E.Expr a
   -> List (Raw Qualifier) × Raw Expr
   -> List (Qualifier a) × Expr a × a
listCompBwd e (Nil × _) = Nil × desugBwd' e × bot
listCompBwd (E.App (E.Lambda (ElimConstr m)) e) ((Guard _ : qs) × s) =
   case listCompBwd (asExpr (get cTrue m)) (qs × s) × asExpr (get cFalse m) of
      (qs' × s' × α) × E.Constr β c Nil | c == cNil -> (Guard (desugBwd' e) : qs') × s' × (α ∨ β)
      _ -> error absurd
listCompBwd (E.App (E.Lambda σ) e) ((Declaration (VarDef π _) : qs) × s) =
   case listCompBwd (asExpr (pattContBwd π σ)) (qs × s) of
      qs' × s' × α -> (Declaration (VarDef π (desugBwd' e)) : qs') × s' × α
listCompBwd (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e) ((Generator p _ : qs) × s) =
   case orElseBwd (ContElim σ) (Left p : Nil) of
      σ' × β -> case listCompBwd (asExpr (pattContBwd p (asElim σ'))) (qs × s) of
         qs' × s' × α -> (Generator p (desugBwd' e) : qs') × s' × (α ∨ β)
listCompBwd _ _ = error absurd

-- NonEmptyList Pattern × Expr
pattsExprFwd :: forall a. JoinSemilattice a => NonEmptyList Pattern × Expr a -> MayFail (Elim a)
pattsExprFwd (NonEmptyList (p :| Nil) × s) = (ContExpr <$> desugFwd' s) >>= pattContFwd p
pattsExprFwd (NonEmptyList (p :| p' : ps) × s) =
   pattContFwd p =<< ContExpr <$> E.Lambda <$> pattsExprFwd (NonEmptyList (p' :| ps) × s)

pattsExprBwd :: forall a. BoundedJoinSemilattice a => NonEmptyList Pattern -> Elim a -> Expr a
pattsExprBwd (NonEmptyList (p :| Nil)) σ = desugBwd' (asExpr (pattContBwd p σ))
pattsExprBwd (NonEmptyList (p :| p' : ps)) σ = next (asExpr (pattContBwd p σ))
   where
   next (E.Lambda τ) = pattsExprBwd (NonEmptyList (p' :| ps)) τ
   next _ = error absurd

-- Pattern × Cont
pattContFwd :: forall a. Pattern -> Cont a -> MayFail (Elim a)
pattContFwd (PVar x) κ = pure (ElimVar x κ)
pattContFwd (PConstr c ps) κ =
   checkArity c (length ps) *> (ElimConstr <$> D.singleton c <$> pattArgsFwd (Left <$> ps) κ)
pattContFwd (PRecord xps) κ =
   ElimRecord (keys xps) <$> pattArgsFwd ((snd >>> Left) <$> sortBy (compare `on` fst) xps) κ
pattContFwd PListEmpty κ = pure (ElimConstr (D.singleton cNil κ))
pattContFwd (PListNonEmpty p o) κ = ElimConstr <$> D.singleton cCons <$> pattArgsFwd (Left p : Right o : Nil) κ

pattContBwd :: forall a. Pattern -> Elim a -> Cont a
pattContBwd (PVar _) (ElimVar _ κ) = κ
pattContBwd (PConstr c ps) (ElimConstr m) = pattArgsBwd (Left <$> ps) (get c m)
pattContBwd (PListEmpty) (ElimConstr m) = get cNil m
pattContBwd (PListNonEmpty p o) (ElimConstr m) = pattArgsBwd (Left p : Right o : Nil) (get cCons m)
pattContBwd (PRecord xps) (ElimRecord _ κ) = pattArgsBwd ((snd >>> Left) <$> sortBy (compare `on` fst) xps) κ
pattContBwd _ _ = error absurd

-- ListRestPattern × Cont
pattCont_ListRest_Fwd :: forall a. ListRestPattern -> Cont a -> MayFail (Elim a)
pattCont_ListRest_Fwd PEnd κ = pure (ElimConstr (D.singleton cNil κ))
pattCont_ListRest_Fwd (PNext p o) κ = ElimConstr <$> D.singleton cCons <$> pattArgsFwd (Left p : Right o : Nil) κ

pattCont_ListRest_Bwd :: forall a. Elim a -> ListRestPattern -> Cont a
pattCont_ListRest_Bwd (ElimVar _ _) _ = error absurd
pattCont_ListRest_Bwd (ElimRecord _ _) _ = error absurd
pattCont_ListRest_Bwd (ElimConstr m) PEnd = get cNil m
pattCont_ListRest_Bwd (ElimConstr m) (PNext p o) = pattArgsBwd (Left p : Right o : Nil) (get cCons m)
pattCont_ListRest_Bwd (ElimSug _ κ) p = pattCont_ListRest_Bwd κ p

-- List (Pattern + ListRestPattern) × Cont
pattArgsFwd :: forall a. List (Pattern + ListRestPattern) -> Cont a -> MayFail (Cont a)
pattArgsFwd Nil κ = pure κ
pattArgsFwd (Left p : πs) κ = ContElim <$> (pattArgsFwd πs κ >>= pattContFwd p)
pattArgsFwd (Right o : πs) κ = ContElim <$> (pattArgsFwd πs κ >>= pattCont_ListRest_Fwd o)

pattArgsBwd :: forall a. List (Pattern + ListRestPattern) -> Endo (Cont a)
pattArgsBwd Nil κ = κ
pattArgsBwd (Left p : πs) σ = pattArgsBwd πs (pattContBwd p (asElim σ))
pattArgsBwd (Right o : πs) σ = pattArgsBwd πs (pattCont_ListRest_Bwd (asElim σ) o)

-- Clauses
clausesFwd :: forall a. JoinSemilattice a => Clauses a -> MayFail (Elim a)
clausesFwd (Clauses bs) = do
   NonEmptyList (σ :| σs) <- traverse pattsExprFwd (unwrap <$> bs)
   foldM maybeJoin σ σs

clausesBwd :: forall a. BoundedJoinSemilattice a => Elim a -> Raw Clauses -> Clauses a
clausesBwd σ (Clauses bs) = Clauses (clauseBwd <$> bs)
   where
   clauseBwd :: Raw Clause -> Clause a
   clauseBwd (Clause (πs × _)) = Clause (πs × pattsExprBwd πs σ)

-- orElse
orElseFwd :: forall a. Cont a -> a -> Cont a
orElseFwd ContNone _ = error absurd
orElseFwd (ContExpr e) _ = ContExpr e
orElseFwd (ContElim (ElimConstr m)) α = ContElim (ElimConstr (unlessFwd (c × orElseFwd κ α) α))
   where
   c × κ = asSingletonMap m
orElseFwd (ContElim (ElimRecord xs κ)) α = ContElim (ElimRecord xs (orElseFwd κ α))
orElseFwd (ContElim (ElimVar x κ)) α = ContElim (ElimVar x (orElseFwd κ α))
orElseFwd (ContElim (ElimSug x σ)) α =
   case orElseFwd (ContElim σ) α of
      ContElim σ' -> ContElim (ElimSug x σ')
      _ -> error absurd

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

-- unless. In forward direction, extend singleton branch to set of branches where any missing constructors have
-- been mapped to the empty list, using anonymous variables in any generated patterns. Going backward, discard
-- all synthesised branches, returning the original singleton branch for c, plus join of annotations on the
-- empty lists used for bodies of synthesised branches.
unlessFwd :: forall a. Ctr × Cont a -> a -> Dict (Cont a)
unlessFwd (c × κ) α =
   let
      defaultBranch c' = c' × applyN (ContElim <<< ElimVar varAnon) (successful (arity c')) (ContExpr (enil α))
      cκs = defaultBranch <$> ((ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ L.singleton c)
   in
      D.fromFoldable ((c × κ) : cκs)

unlessBwd :: forall a. BoundedJoinSemilattice a => Dict (Cont a) -> Ctr -> Cont a × a
unlessBwd m c =
   let
      cs = (ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ L.singleton c
   in
      unsafePartial $ get c m × foldl (∨) bot ((bodyAnn <<< body) <$> cs)
   where
   body :: Partial => Ctr -> Cont a
   body c' = applyN (\(ContElim (ElimVar _ κ)) -> κ) (successful (arity c')) (get c' m)

   bodyAnn :: Partial => Cont a -> a
   bodyAnn (ContExpr (E.Constr α c' Nil)) | c' == cNil = α

-- ======================
-- boilerplate
-- ======================
derive instance Newtype (Clause a) _
derive instance Newtype (Clauses a) _
derive instance Newtype (RecDef a) _
derive instance Functor Clause
derive instance Functor Clauses
derive instance Functor Expr
derive instance Functor ListRest
derive instance Functor VarDef
derive instance Functor Qualifier

instance Functor Module where
   map f (Module defs) = Module (mapDefs f <$> defs)
      where
      mapDefs :: forall a b. (a -> b) -> VarDefs a + RecDefs a -> VarDefs b + RecDefs b
      mapDefs g (Left ds) = Left $ map g <$> ds
      mapDefs g (Right ds) = Right $ (\(x × Clause (ps × s)) -> x × Clause (ps × (g <$> s))) <$> ds

instance JoinSemilattice a => JoinSemilattice (Expr a) where
   join s = definedJoin s
   maybeJoin _ = error unimplemented
   neg = (<$>) neg
