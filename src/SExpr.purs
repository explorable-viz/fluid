module SExpr where

import Prelude hiding (absurd, top)

import Bind (Bind, Var, varAnon, (↦))
import Bind (keys) as B
import Control.Monad.Error.Class (class MonadError)
import Data.Bitraversable (rtraverse)
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (foldl)
import Data.Function (applyN, on)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), drop, length, partition, sortBy, take, zip, zipWith, (:), (\\))
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (foldl1, (:|))
import Data.Profunctor.Strong (first, second, (***))
import Data.Set (toUnfoldable) as S
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence, traverse)
import Data.Tuple (uncurry, fst, snd)
import Data.Unfoldable (replicate)
import DataType (Ctr, DataType, arity, cCons, cFalse, cNil, cTrue, ctrs, dataTypeFor)
import Desugarable (class Desugarable, desugBwd, desug)
import Dict (Dict)
import Dict as D
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), Module(..), RecDefs(..), VarDef(..)) as E
import Lattice (class BoundedJoinSemilattice, class BoundedLattice, class JoinSemilattice, Raw, bot, botOf, top, (∨))
import Partial.Unsafe (unsafePartial)
import Util (type (+), type (×), Endo, absurd, appendList, assert, defined, error, nonEmpty, shapeMismatch, singleton, throw, unimplemented, (×), (≜))
import Util.Map (get, lookup)
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
   = PListVar Var -- currently unsupported in parser; only arise during desugaring
   | PListEnd
   | PListNext Pattern ListRestPattern

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
   desug = exprFwd
   desugBwd = exprBwd

instance Desugarable ListRest E.Expr where
   desug = listRestFwd
   desugBwd = listRestBwd

instance Desugarable Clauses Elim where
   desug = clausesFwd
   desugBwd = clausesBwd

desugarModuleFwd :: forall a m. MonadError Error m => BoundedLattice a => Module a -> m (E.Module a)
desugarModuleFwd = moduleFwd

-- helpers
enil :: forall a. a -> E.Expr a
enil α = E.Constr α cNil Nil

econs :: forall a. a -> E.Expr a -> E.Expr a -> E.Expr a
econs α e e' = E.Constr α cCons (e : e' : Nil)

elimBool :: forall a. Cont a -> Cont a -> Elim a
elimBool κ κ' = ElimConstr (wrap $ D.fromFoldable [ cTrue × κ, cFalse × κ' ])

-- Module. Surface language supports "blocks" of variable declarations; core does not. Currently no backward.
moduleFwd :: forall a m. MonadError Error m => BoundedLattice a => Module a -> m (E.Module a)
moduleFwd (Module ds) = E.Module <$> traverse varDefOrRecDefsFwd (join (flatten <$> ds))
   where
   varDefOrRecDefsFwd :: VarDef a + RecDefs a -> m (E.VarDef a + E.RecDefs a)
   varDefOrRecDefsFwd (Left d) = Left <$> varDefFwd d
   varDefOrRecDefsFwd (Right xcs) = Right <$> recDefsFwd xcs

   flatten :: VarDefs a + RecDefs a -> List (VarDef a + RecDefs a)
   flatten (Left ds') = Left <$> toList ds'
   flatten (Right δ) = pure (Right δ)

-- Use of eliminators to establish module bindings is a bit naff, because we don't really have a notion of
-- "rest of module" to use as continuation. So use empty record (unit tuple) as continuation, and disregard
-- in evaluation.
varDefFwd :: forall a m. MonadError Error m => BoundedLattice a => VarDef a -> m (E.VarDef a)
varDefFwd (VarDef p s) =
   E.VarDef <$> clausesFwd (Clauses (singleton (Clause (singleton p × Record top Nil)))) <*> desug s

-- VarDefs
varDefsFwd :: forall a m. MonadError Error m => BoundedLattice a => VarDefs a × Expr a -> m (E.Expr a)
varDefsFwd (NonEmptyList (d :| Nil) × s) =
   E.Let <$> varDefFwd d <*> desug s
varDefsFwd (NonEmptyList (d :| d' : ds) × s) =
   E.Let <$> varDefFwd d <*> varDefsFwd (NonEmptyList (d' :| ds) × s)

varDefsBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw VarDefs × Raw Expr -> VarDefs a × Expr a
varDefsBwd (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π s1 :| Nil) × s2) =
   NonEmptyList (VarDef π (desugBwd e1 s1) :| Nil) × desugBwd e2 s2
varDefsBwd (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π s1 :| d : ds) × s2) =
   let
      NonEmptyList (d' :| ds') × s2' = varDefsBwd e2 (NonEmptyList (d :| ds) × s2)
   in
      NonEmptyList (VarDef π (desugBwd e1 s1) :| d' : ds') × s2'
varDefsBwd _ (NonEmptyList (_ :| _) × _) = error absurd

-- RecDefs
-- In the formalism, "group by name" is part of the syntax.
recDefsFwd :: forall a m. MonadError Error m => BoundedLattice a => RecDefs a -> m (E.RecDefs a)
recDefsFwd xcs = E.RecDefs top <$> wrap <<< D.fromFoldable <$> traverse recDefFwd xcss
   where
   xcss = map RecDef (groupBy (eq `on` fst) xcs) :: NonEmptyList (RecDef a)

recDefsBwd :: forall a. BoundedJoinSemilattice a => E.RecDefs a -> Raw RecDefs -> RecDefs a
recDefsBwd (E.RecDefs _ ρ) xcs = join (go (groupBy (eq `on` fst) xcs))
   where
   go :: NonEmptyList (Raw RecDefs) -> NonEmptyList (RecDefs a)
   go (NonEmptyList (xcs1 :| xcss)) =
      NonEmptyList (unwrap (recDefBwd (x ↦ get x ρ) (RecDef xcs1)) :| xcss')
      where
      x = fst (head xcs1)
      xcss' = case xcss of
         Nil -> Nil
         xcs2 : xcss'' -> toList (go (NonEmptyList (xcs2 :| xcss'')))

-- RecDef
recDefFwd :: forall a m. MonadError Error m => BoundedLattice a => RecDef a -> m (Bind (Elim a))
recDefFwd xcs = (fst (head (unwrap xcs)) ↦ _) <$> clausesFwd (Clauses (snd <$> unwrap xcs))

recDefBwd :: forall a. BoundedJoinSemilattice a => Bind (Elim a) -> Raw RecDef -> RecDef a
recDefBwd (x ↦ σ) (RecDef bs) = RecDef ((x × _) <$> unwrap (clausesBwd σ (Clauses (snd <$> bs))))

-- Expr
exprFwd :: forall a m. BoundedLattice a => MonadError Error m => JoinSemilattice a => Expr a -> m (E.Expr a)
exprFwd (Var x) = pure (E.Var x)
exprFwd (Op op) = pure (E.Op op)
exprFwd (Int α n) = pure (E.Int α n)
exprFwd (Float α n) = pure (E.Float α n)
exprFwd (Str α s) = pure (E.Str α s)
exprFwd (Constr α c ss) = E.Constr α c <$> traverse desug ss
exprFwd (Record α xss) = E.Record α <$> wrap <<< D.fromFoldable <$> traverse (traverse desug) xss
exprFwd (Dictionary α sss) = E.Dictionary α <$> traverse (traverse desug) sss
exprFwd (Matrix α s (x × y) s') = E.Matrix α <$> desug s <@> x × y <*> desug s'
exprFwd (Lambda bs) = E.Lambda top <$> clausesFwd bs
exprFwd (Project s x) = E.Project <$> desug s <@> x
exprFwd (App s1 s2) = E.App <$> desug s1 <*> desug s2
exprFwd (BinaryApp s1 op s2) = E.App <$> (E.App (E.Op op) <$> desug s1) <*> desug s2
exprFwd (MatchAs s bs) =
   E.App <$> (E.Lambda top <$> clausesFwd (Clauses (Clause <$> first singleton <$> bs))) <*> desug s
exprFwd (IfElse s1 s2 s3) =
   E.App <$> (E.Lambda top <$> (elimBool <$> (ContExpr <$> desug s2) <*> (ContExpr <$> desug s3))) <*> desug s1
exprFwd (ListEmpty α) = pure (enil α)
exprFwd (ListNonEmpty α s l) = econs α <$> desug s <*> desug l
exprFwd (ListEnum s1 s2) = E.App <$> ((E.App (E.Var "enumFromTo")) <$> desug s1) <*> desug s2
exprFwd (ListComp α s qs) = listCompFwd (α × qs × s)
exprFwd (Let ds s) = varDefsFwd (ds × s)
exprFwd (LetRec xcs s) = E.LetRec <$> recDefsFwd xcs <*> desug s

exprBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw Expr -> Expr a
exprBwd (E.Var _) (Var x) = Var x
exprBwd (E.Op _) (Op op) = Op op
exprBwd (E.Int α _) (Int _ n) = Int α n
exprBwd (E.Float α _) (Float _ n) = Float α n
exprBwd (E.Str α _) (Str _ str) = Str α str
exprBwd (E.Constr α _ es) (Constr _ c ss) = Constr α c (uncurry desugBwd <$> zip es ss)
exprBwd (E.Record α xes) (Record _ xss) =
   Record α $ xss <#> \(x ↦ s) -> x ↦ desugBwd (get x xes) s
exprBwd (E.Dictionary α ees) (Dictionary _ sss) =
   Dictionary α (zipWith (\(Pair e e') (Pair s s') -> Pair (desugBwd e s) (desugBwd e' s')) ees sss)
exprBwd (E.Matrix α e1 _ e2) (Matrix _ s1 (x × y) s2) =
   Matrix α (desugBwd e1 s1) (x × y) (desugBwd e2 s2)
exprBwd (E.Lambda _ σ) (Lambda μ) = Lambda (clausesBwd_New σ μ)
exprBwd (E.Project e _) (Project s x) = Project (desugBwd e s) x
exprBwd (E.App e1 e2) (App s1 s2) = App (desugBwd e1 s1) (desugBwd e2 s2)
exprBwd (E.App (E.App (E.Op _) e1) e2) (BinaryApp s1 op s2) =
   BinaryApp (desugBwd e1 s1) op (desugBwd e2 s2)
exprBwd (E.App (E.Lambda _ σ) e) (MatchAs s bs) =
   MatchAs (desugBwd e s)
      (first head <$> unwrap <$> unwrap (clausesBwd σ (Clauses (Clause <$> first singleton <$> bs))))
exprBwd (E.App (E.Lambda _ (ElimConstr m)) e1) (IfElse s1 s2 s3) =
   IfElse (desugBwd e1 s1)
      (desugBwd (asExpr (get cTrue m)) s2)
      (desugBwd (asExpr (get cFalse m)) s3)
exprBwd (E.Constr α _ Nil) (ListEmpty _) = ListEmpty α
exprBwd (E.Constr α _ (e1 : e2 : Nil)) (ListNonEmpty _ s l) =
   ListNonEmpty α (desugBwd e1 s) (desugBwd e2 l)
exprBwd (E.App (E.App (E.Var "enumFromTo") e1) e2) (ListEnum s1 s2) =
   ListEnum (desugBwd e1 s1) (desugBwd e2 s2)
exprBwd e (ListComp _ s qs) =
   let α × qs' × s' = listCompBwd e (qs × s) in ListComp α s' qs'
exprBwd (E.Let d e) (Let ds s) = uncurry Let (varDefsBwd (E.Let d e) (ds × s))
exprBwd (E.LetRec xσs e) (LetRec xcs s) = LetRec (recDefsBwd xσs xcs) (desugBwd e s)
exprBwd _ _ = error absurd

-- ListRest
listRestFwd :: forall a m. MonadError Error m => BoundedLattice a => ListRest a -> m (E.Expr a)
listRestFwd (End α) = pure (enil α)
listRestFwd (Next α s l) = econs α <$> desug s <*> desug l

listRestBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw ListRest -> ListRest a
listRestBwd (E.Constr α _ _) (End _) = End α
listRestBwd (E.Constr α _ (e1 : e2 : Nil)) (Next _ s l) =
   Next α (desugBwd e1 s) (desugBwd e2 l)
listRestBwd _ _ = error absurd

-- List Qualifier × Expr
listCompFwd :: forall a m. MonadError Error m => BoundedLattice a => a × List (Qualifier a) × Expr a -> m (E.Expr a)
listCompFwd (α × Nil × s) =
   econs α <$> desug s <@> enil α
listCompFwd (α × (Guard s : qs) × s') = do
   e <- listCompFwd (α × qs × s')
   E.App (E.Lambda α (elimBool (ContExpr e) (ContExpr (enil α)))) <$> desug s
listCompFwd (α × (Declaration (VarDef p s) : qs) × s') = do
   σ <- clausesFwd (Clauses (singleton (Clause (singleton p × ListComp α s' qs))))
   E.App (E.Lambda α σ) <$> desug s
listCompFwd (α × (Generator p s : qs) × s') = do
   let ks = orElseFwd (ListEmpty α) ((Left p : Nil) × ListComp α s' qs)
   σ <- clausesStateFwd (toList (ks <#> second (Nil × _)))
   E.App (E.App (E.Var "concatMap") (E.Lambda α (asElim σ))) <$> desug s

listCompBwd
   :: forall a
    . BoundedJoinSemilattice a
   => E.Expr a
   -> List (Raw Qualifier) × Raw Expr
   -> a × List (Qualifier a) × Expr a
listCompBwd (E.Constr α2 c (e : E.Constr α1 c' Nil : Nil)) (Nil × s) | c == cCons && c' == cNil =
   (α1 ∨ α2) × Nil × desugBwd e s
listCompBwd (E.App (E.Lambda α' (ElimConstr m)) e) ((Guard s0 : qs) × s) =
   case listCompBwd (asExpr (get cTrue m)) (qs × s) × asExpr (get cFalse m) of
      (α × qs' × s') × E.Constr β c Nil | c == cNil -> (α ∨ α' ∨ β) × (Guard (desugBwd e s0) : qs') × s'
      _ -> error absurd
listCompBwd (E.App (E.Lambda α' σ) e) ((Declaration (VarDef π s0) : qs) × s) =
   case listCompBwd (asExpr (pattContBwd π σ)) (qs × s) of
      α × qs' × s' -> (α ∨ α') × (Declaration (VarDef π (desugBwd e s0)) : qs') × s'
listCompBwd (E.App (E.App (E.Var "concatMap") (E.Lambda α' σ)) e) ((Generator p s0 : qs) × s) =
   case orElseBwd (ContElim σ) (Left p : Nil) of
      σ' × β -> case listCompBwd (asExpr (pattContBwd p (asElim σ'))) (qs × s) of
         α × qs' × s' -> (α ∨ α' ∨ β) × (Generator p (desugBwd e s0) : qs') × s'
listCompBwd _ _ = error absurd

-- Pattern × Cont
pattContBwd :: forall a. Pattern -> Elim a -> Cont a
pattContBwd (PVar _) (ElimVar _ κ) = κ
pattContBwd (PConstr c ps) (ElimConstr m) = pattArgsBwd (Left <$> ps) (get c m)
pattContBwd (PListEmpty) (ElimConstr m) = get cNil m
pattContBwd (PListNonEmpty p o) (ElimConstr m) = pattArgsBwd (Left p : Right o : Nil) (get cCons m)
pattContBwd (PRecord xps) (ElimRecord _ κ) = pattArgsBwd ((snd >>> Left) <$> sortBy (compare `on` fst) xps) κ
pattContBwd _ _ = error absurd

-- ListRestPattern × Cont
pattCont_ListRest_Bwd :: forall a. Elim a -> ListRestPattern -> Cont a
pattCont_ListRest_Bwd (ElimVar _ _) _ = error absurd
pattCont_ListRest_Bwd (ElimRecord _ _) _ = error absurd
pattCont_ListRest_Bwd (ElimConstr _) (PListVar _) = error absurd
pattCont_ListRest_Bwd (ElimConstr m) PListEnd = get cNil m
pattCont_ListRest_Bwd (ElimConstr m) (PListNext p o) = pattArgsBwd (Left p : Right o : Nil) (get cCons m)

-- List (Pattern + ListRestPattern) × Cont
pattArgsBwd :: forall a. List (Pattern + ListRestPattern) -> Endo (Cont a)
pattArgsBwd Nil κ = κ
pattArgsBwd (Left p : πs) σ = pattArgsBwd πs (pattContBwd p (asElim σ))
pattArgsBwd (Right o : πs) σ = pattArgsBwd πs (pattCont_ListRest_Bwd (asElim σ) o)

-- Clauses
clausesFwd :: forall a m. BoundedLattice a => MonadError Error m => Clauses a -> m (Elim a)
clausesFwd μ = clausesStateFwd (toClausesStateFwd μ) <#> asElim

clausesBwd_New :: forall a. BoundedJoinSemilattice a => Elim a -> Raw Clauses -> Clauses a
clausesBwd_New σ μ = toClausesStateBwd (clausesStateBwd (ContElim σ) (toClausesStateFwd μ))

toClausesStateFwd :: forall a. Clauses a -> ClausesState' a
toClausesStateFwd (Clauses μ) = toList μ <#> toClauseStateFwd
   where
   toClauseStateFwd :: Clause a -> ClauseState' a
   toClauseStateFwd (Clause (NonEmptyList (p :| π) × s)) = (Left p : Nil) × π × s

toClausesStateBwd :: forall a. ClausesState' a -> Clauses a
toClausesStateBwd Nil = error (shapeMismatch unit)
toClausesStateBwd (k : ks) = Clauses (NonEmptyList (k :| ks) <#> toClauseStateBwd)
   where
   toClauseStateBwd :: ClauseState' a -> Clause a
   toClauseStateBwd ((Left p : Nil) × π × s) = Clause (NonEmptyList (p :| π) × s)
   toClauseStateBwd _ = error (shapeMismatch unit)

clausesBwd :: forall a. BoundedJoinSemilattice a => Elim a -> Raw Clauses -> Clauses a
clausesBwd σ (Clauses bs) = Clauses (clauseBwd <$> bs)
   where
   clauseBwd :: Raw Clause -> Clause a
   clauseBwd (Clause (πs × s)) = Clause (πs × pattsExprBwd (πs × s) σ)

   pattsExprBwd :: NonEmptyList Pattern × Raw Expr -> Elim a -> Expr a
   pattsExprBwd (NonEmptyList (p :| Nil) × s) σ' = desugBwd (asExpr (pattContBwd p σ')) s
   pattsExprBwd (NonEmptyList (p :| p' : ps) × s) σ' = next (asExpr (pattContBwd p σ'))
      where
      next (E.Lambda _ τ) = pattsExprBwd (NonEmptyList (p' :| ps) × s) τ
      next _ = error absurd

-- Like ClauseState but for curried functions; extra component π' stores remaining top-level patterns.
type ClauseState' a = List (Pattern + ListRestPattern) × List Pattern × Expr a
type ClausesState' a = List (ClauseState' a)

popArgFwd :: forall a m. MonadError Error m => ClausesState' a -> m (ClausesState' a)
popArgFwd ((Nil × (p : π) × s) : ks) = (((Left p : Nil) × π × s) : _) <$> popArgFwd ks
popArgFwd Nil = pure Nil
popArgFwd _ = throw (shapeMismatch unit)

popArgBwd :: forall a. Endo (ClausesState' a)
popArgBwd (((Left p : Nil) × π × s) : ks) = (Nil × (p : π) × s) : popArgBwd ks
popArgBwd Nil = Nil
popArgBwd _ = error absurd

popVarFwd :: forall a m. MonadError Error m => Var -> ClausesState' a -> m (ClausesState' a)
popVarFwd x (((Left (PVar x') : π) × π' × s) : ks) = ((π × π' × s) : _) <$> popVarFwd (x ≜ x') ks
popVarFwd _ Nil = pure Nil
popVarFwd _ _ = throw (shapeMismatch unit)

popVarBwd :: forall a. Var -> Endo (ClausesState' a)
popVarBwd x ((π × π' × s) : ks) = ((Left (PVar x) : π) × π' × s) : popVarBwd x ks
popVarBwd _ Nil = Nil

popListVarFwd :: forall a m. MonadError Error m => Var -> ClausesState' a -> m (ClausesState' a)
popListVarFwd x (((Right (PListVar x') : π) × π' × s) : ks) = ((π × π' × s) : _) <$> popListVarFwd (x ≜ x') ks
popListVarFwd _ Nil = pure Nil
popListVarFwd _ _ = throw (shapeMismatch unit)

popListVarBwd :: forall a. Var -> Endo (ClausesState' a)
popListVarBwd x ((π × π' × s) : ks) = ((Left (PVar x) : π) × π' × s) : popListVarBwd x ks
popListVarBwd _ Nil = Nil

popConstrFwd :: forall a m. MonadError Error m => DataType -> ClausesState' a -> m (List (Ctr × ClausesState' a))
popConstrFwd _ ((Nil × _ × _) : _) = error absurd
popConstrFwd d (((Left (PConstr c π) : π') × π'' × s) : ks) =
   assert (length π == defined (arity c) && defined (dataTypeFor c) == d) $
      forConstr c (((Left <$> π) <> π') × π'' × s) <$> popConstrFwd d ks
popConstrFwd d (((Left PListEmpty : π) × π' × s) : ks) =
   assert (d == defined (dataTypeFor cNil)) $
      forConstr cNil (π × π' × s) <$> popConstrFwd d ks
popConstrFwd d (((Left (PListNonEmpty p o) : π) × π' × s) : ks) =
   assert (d == defined (dataTypeFor cCons)) $
      forConstr cCons ((Left p : Right o : π) × π' × s) <$> popConstrFwd d ks
popConstrFwd _ (((Left _ : _) × _) : _) = throw (shapeMismatch unit)
popConstrFwd d (((Right PListEnd : π) × π' × s) : ks) =
   assert (d == defined (dataTypeFor cNil)) $
      forConstr cNil (π × π' × s) <$> popConstrFwd d ks
popConstrFwd d (((Right (PListNext p o) : π) × π' × s) : ks) =
   assert (d == defined (dataTypeFor cCons)) $
      forConstr cCons ((Left p : Right o : π) × π' × s) <$> popConstrFwd d ks
popConstrFwd _ (((Right _ : _) × _) : _) = throw (shapeMismatch unit)
popConstrFwd _ Nil = pure Nil

forConstr :: forall a. Ctr -> ClauseState' a -> Endo (List (Ctr × ClausesState' a))
forConstr c k Nil = (c × (k : Nil)) : Nil
forConstr c k ((c' × ks') : cks)
   | c == c' = (c' × (k : ks')) : cks
   | otherwise = (c' × ks') : forConstr c k cks

popConstrBwd :: forall a. List (Ctr × ClausesState' a) -> ClausesState' a
popConstrBwd = error "todo"

popRecordFwd :: forall a m. MonadError Error m => List Var -> ClausesState' a -> m (ClausesState' a)
popRecordFwd xs (((Left (PRecord xps) : π) × π' × s) : ks) =
   assert ((xps <#> fst) == xs) $ ((((xps <#> snd >>> Left) <> π) × π' × s) : _) <$> popRecordFwd xs ks
popRecordFwd _ Nil = pure Nil
popRecordFwd _ _ = throw (shapeMismatch unit)

popRecordBwd :: forall a. List Var -> Endo (ClausesState' a)
popRecordBwd xs ((π × π' × s) : ks) =
   ((Left (PRecord xps) : drop (length xs) π) × π' × s) : popRecordBwd xs ks
   where
   xps = zip xs (unsafePartial (\(Left p) -> p) <$> take (length xs) π)

popRecordBwd _ Nil = Nil

clausesStateFwd :: forall a m. BoundedLattice a => MonadError Error m => ClausesState' a -> m (Cont a)
clausesStateFwd Nil = error absurd
clausesStateFwd ((Nil × Nil × s) : Nil) =
   ContExpr <$> desug s
clausesStateFwd ks@((Nil × _) : _) =
   ContExpr <$> E.Lambda top <$> asElim <$> (clausesStateFwd =<< popArgFwd ks)
clausesStateFwd ks@(((Left (PVar x) : _) × _) : _) =
   ContElim <$> ElimVar x <$> (clausesStateFwd =<< popVarFwd x ks)
clausesStateFwd ks@(((Left (PRecord xps) : _) × _) : _) =
   ContElim <$> ElimRecord (B.keys xps) <$> (clausesStateFwd =<< popRecordFwd (xps <#> fst) ks)
clausesStateFwd ks@(((Left (PConstr c _) : _) × _) : _) = do
   kss <- popConstrFwd (defined (dataTypeFor c)) ks
   ContElim <$> ElimConstr <$> wrap <<< D.fromFoldable <$> sequence (rtraverse clausesStateFwd <$> kss)
clausesStateFwd ks@(((Left PListEmpty : _) × _) : _) = do
   kss <- popConstrFwd (defined (dataTypeFor cNil)) ks
   ContElim <$> ElimConstr <$> wrap <<< D.fromFoldable <$> sequence (rtraverse clausesStateFwd <$> kss)
clausesStateFwd ks@(((Left (PListNonEmpty _ _) : _) × _) : _) = do
   kss <- popConstrFwd (defined (dataTypeFor cCons)) ks
   ContElim <$> ElimConstr <$> wrap <<< D.fromFoldable <$> sequence (rtraverse clausesStateFwd <$> kss)
clausesStateFwd ks@(((Right (PListVar x) : _) × _) : _) =
   ContElim <$> ElimVar x <$> (clausesStateFwd =<< popListVarFwd x ks)
clausesStateFwd ks@(((Right PListEnd : _) × _) : _) = do
   kss <- popConstrFwd (defined (dataTypeFor cNil)) ks
   ContElim <$> ElimConstr <$> wrap <<< D.fromFoldable <$> sequence (rtraverse clausesStateFwd <$> kss)
clausesStateFwd ks@(((Right (PListNext _ _) : _) × _) : _) = do
   kss <- popConstrFwd (defined (dataTypeFor cCons)) ks
   ContElim <$> ElimConstr <$> wrap <<< D.fromFoldable <$> sequence (rtraverse clausesStateFwd <$> kss)

clausesStateBwd :: forall a. BoundedJoinSemilattice a => Cont a -> Raw ClausesState' -> ClausesState' a
clausesStateBwd _ Nil = error absurd
clausesStateBwd (ContExpr e) ((Nil × Nil × s) : Nil) =
   (Nil × Nil × desugBwd e s) : Nil
clausesStateBwd (ContExpr (E.Lambda _ σ)) ks@((Nil × _) : _) =
   popArgBwd (clausesStateBwd (ContElim σ) (defined (popArgFwd ks)))
clausesStateBwd (ContExpr _) _ = error absurd
clausesStateBwd (ContElim (ElimVar x κ)) ks@(((Left (PVar _) : _) × _) : _) =
   popVarBwd x (clausesStateBwd κ (defined (popVarFwd x ks)))
clausesStateBwd (ContElim (ElimRecord _ κ)) ks@(((Left (PRecord xps) : _) × _) : _) =
   popRecordBwd (xps <#> fst) (clausesStateBwd κ (defined (popRecordFwd (xps <#> fst) ks)))
clausesStateBwd (ContElim (ElimConstr m)) ks@(((Left (PConstr c _) : _) × _) : _) =
   popConstrBwd (filterMap (\(c' ↦ ks') -> (c' ↦ _) <$> (clausesStateBwd <$> lookup c' m <@> ks')) kss)
   where
   kss = defined (popConstrFwd (defined (dataTypeFor c)) ks)
clausesStateBwd (ContElim (ElimConstr m)) ks@(((Left PListEmpty : _) × _) : _) =
   popConstrBwd (filterMap (\(c' ↦ ks') -> (c' ↦ _) <$> (clausesStateBwd <$> lookup c' m <@> ks')) kss)
   where
   kss = defined (popConstrFwd (defined (dataTypeFor cNil)) ks)
clausesStateBwd (ContElim (ElimConstr m)) ks@(((Left (PListNonEmpty _ _) : _) × _) : _) =
   popConstrBwd (filterMap (\(c' ↦ ks') -> (c' ↦ _) <$> (clausesStateBwd <$> lookup c' m <@> ks')) kss)
   where
   kss = defined (popConstrFwd (defined (dataTypeFor cCons)) ks)
clausesStateBwd (ContElim (ElimVar x κ)) ks@(((Right (PListVar _) : _) × _) : _) =
   popListVarBwd x (clausesStateBwd κ (defined (popListVarFwd x ks)))
clausesStateBwd (ContElim (ElimConstr m)) ks@(((Right PListEnd : _) × _) : _) =
   popConstrBwd (filterMap (\(c' ↦ ks') -> (c' ↦ _) <$> (clausesStateBwd <$> lookup c' m <@> ks')) kss)
   where
   kss = defined (popConstrFwd (defined (dataTypeFor cNil)) ks)
clausesStateBwd (ContElim (ElimConstr m)) ks@(((Right (PListNext _ _) : _) × _) : _) =
   popConstrBwd (filterMap (\(c' ↦ ks') -> (c' ↦ _) <$> (clausesStateBwd <$> lookup c' m <@> ks')) kss)
   where
   kss = defined (popConstrFwd (defined (dataTypeFor cCons)) ks)
clausesStateBwd (ContElim _) _ = error (shapeMismatch unit)

-- First component π is stack of subpatterns active during processing of a single top-level pattern p,
-- initially containing only p and ending up empty.
type ClauseState a = List (Pattern + ListRestPattern) × Expr a

pushPatt :: forall a. Pattern + ListRestPattern -> Endo (ClauseState a)
pushPatt p (π × s) = (p : π) × s

popPatt :: forall a. ClauseState a -> (Pattern + ListRestPattern) × ClauseState a
popPatt ((p : π) × s) = p × (π × s)
popPatt _ = error (shapeMismatch unit)

pushPatts :: forall a. List (Pattern + ListRestPattern) -> Endo (ClauseState a)
pushPatts π (π' × s) = (π <> π') × s

popPatts :: forall a. Int -> ClauseState a -> List (Pattern + ListRestPattern) × ClauseState a
popPatts n (π' × s) = take n π' × drop n π' × s

orElseUnderFwd
   :: forall a
    . Expr a
   -> List (Pattern + ListRestPattern)
   -> ClauseState a
   -> NonEmptyList (List (Pattern + ListRestPattern) × ClauseState a)
orElseUnderFwd s' π k = popPatts (length π) <$> orElseFwd s' (pushPatts π k)

orElseUnderBwd
   :: forall a
    . NonEmptyList (List (Pattern + ListRestPattern) × ClauseState a)
   -> Expr a × List (Pattern + ListRestPattern) × ClauseState a
orElseUnderBwd = error "todo"

orElseFwd :: forall a. Expr a -> ClauseState a -> NonEmptyList (ClauseState a)
orElseFwd _ (Nil × s) = singleton (Nil × s)
orElseFwd s' ((Left (PVar x) : π) × s) =
   orElseFwd s' (π × s) <#> pushPatt (Left (PVar x))
orElseFwd s' ((Left (PConstr c π) : π') × s) = ks `appendList` ks'
   where
   ks = orElseUnderFwd s' (Left <$> π) (π' × s)
      <#> (\(π1 × k) -> pushPatt (Left (PConstr c (unsafePartial (\(Left p) -> p) <$> π1))) k)
   cs = S.toUnfoldable (ctrs (defined (dataTypeFor c))) \\ singleton c
   ks' = cs <#> \c' -> ((π' <#> anon) × s')
      # pushPatt (Left (PConstr c' (replicate (defined (arity c')) (PVar varAnon))))
orElseFwd s' ((Left (PRecord xps) : π) × s) =
   orElseUnderFwd s' (Left <<< snd <$> xps) (π × s)
      <#> \(π1 × k) -> pushPatt (Left (PRecord (zip (fst <$> xps) (unsafePartial (\(Left p) -> p) <$> π1)))) k
orElseFwd s' ((Left PListEmpty : π) × s) = ks `appendList` (k : Nil)
   where
   ks = orElseFwd s' (π × s) <#> pushPatt (Left PListEmpty)
   k = (((π <#> anon) × s') # pushPatt (Left (PConstr cCons (replicate 2 (PVar varAnon)))))
orElseFwd s' ((Left (PListNonEmpty p o) : π) × s) = ks `appendList` (k' : Nil)
   where
   ks = orElseUnderFwd s' (Left p : Right o : Nil) (π × s)
      <#> unsafePartial \((Left p' : Right o' : Nil) × k) -> pushPatt (Left (PListNonEmpty p' o')) k
   k' = (((π <#> anon) × s') # pushPatt (Left PListEmpty))
orElseFwd s' ((Right (PListVar x) : π) × s) =
   orElseFwd s' (π × s) <#> pushPatt (Right (PListVar x))
orElseFwd s' ((Right (PListNext p o) : π) × s) = ks `appendList` (k' : Nil)
   where
   ks = orElseUnderFwd s' (Left p : Right o : Nil) (π × s)
      <#> unsafePartial \((Left p' : Right o' : Nil) × k) -> pushPatt (Right (PListNext p' o')) k
   k' = (((π <#> anon) × s') # pushPatt (Right PListEnd))
orElseFwd s' ((Right PListEnd : π) × s) = ks `appendList` (k : Nil)
   where
   ks = orElseFwd s' (π × s) <#> pushPatt (Right PListEnd)
   k = (((π <#> anon) × s') # pushPatt (Right (PListNext (PVar varAnon) (PListVar varAnon))))

anon :: Pattern + ListRestPattern -> Pattern + ListRestPattern
anon (Left _) = Left (PVar varAnon)
anon (Right _) = Right (PListVar varAnon)

orElseBwd_New
   :: forall a
    . BoundedJoinSemilattice a
   => Raw Expr × Raw ClauseState
   -> NonEmptyList (ClauseState a)
   -> Expr a × Expr a
orElseBwd_New (s' × (Nil × _)) (NonEmptyList (Nil × s :| Nil)) = botOf s' × s
orElseBwd_New (_ × (Nil × _)) _ = error absurd
orElseBwd_New (s' × ((Left (PVar _) : π) × s)) ks =
   orElseBwd_New (s' × (π × s)) (ks <#> popPatt >>> snd)
orElseBwd_New (s' × ((Left (PRecord xps) : π) × s)) ks =
   orElseBwd_New (s' × (((Left <<< snd <$> xps) <> π) × s))
      ( ks
           <#> popPatt
           <#> unsafePartial \(Left (PRecord xps') × k) -> pushPatts (xps' <#> Left <<< snd) k
      )
orElseBwd_New (s' × ((Left (PConstr c π) : π') × s)) ks =
   orElseBwd_New (s' × (((Left <$> π) <> π') × s))
      ( nonEmpty ks_c
           <#> popPatt
           <#> unsafePartial \(Left (PConstr _ π'') × k) -> pushPatts (Left <$> π'') k
      )
      # first ((_ :| (ks_not_c <#> snd)) >>> foldl1 (∨))
   where
   { no: ks_not_c, yes: ks_c } = flip partition (toList ks) case _ of
      (Left (PConstr c' _) : _) × _ -> c' == c
      _ -> false
orElseBwd_New (s' × ((Left PListEmpty : π) × s)) ks =
   orElseBwd_New (s' × (π × s)) (ks <#> popPatt >>> snd)
orElseBwd_New (s' × ((Left (PListNonEmpty p o) : π) × s)) ks =
   orElseBwd_New (s' × ((Left p : Right o : Nil <> π) × s))
      ( ks
           <#> popPatt
           <#> unsafePartial \(Left (PListNonEmpty p' o') × k) -> pushPatts (Left p' : Right o' : Nil) k
      )
orElseBwd_New (s' × ((Right (PListVar _) : π) × s)) ks =
   orElseBwd_New (s' × (π × s)) (ks <#> popPatt >>> snd)
orElseBwd_New (s' × ((Right PListEnd : π) × s)) ks =
   orElseBwd_New (s' × (π × s)) (ks <#> popPatt >>> snd)
orElseBwd_New (s' × ((Right (PListNext p o) : π) × s)) ks =
   orElseBwd_New (s' × ((Left p : Right o : Nil <> π) × s))
      ( ks
           <#> popPatt
           <#> unsafePartial \(Right (PListNext p' o') × k) -> pushPatts (Left p' : Right o' : Nil) k
      )

-- orElse
orElseBwd :: forall a. BoundedJoinSemilattice a => Cont a -> List (Pattern + ListRestPattern) -> Cont a × a
orElseBwd κ Nil = κ × bot
orElseBwd (ContElim (ElimVar _ κ')) (Left (PVar x) : πs) =
   orElseBwd κ' πs # first (\κ'' -> ContElim (ElimVar x κ''))
orElseBwd (ContElim (ElimRecord _ κ')) (Left (PRecord xps) : πs) =
   orElseBwd κ' ((xps <#> (snd >>> Left)) <> πs) # first (\κ'' -> ContElim (ElimRecord (B.keys xps) κ''))
orElseBwd (ContElim (ElimConstr m)) (π : πs) =
   let
      c × πs' = case π of
         -- TODO: refactor so absurd cases aren't necessary
         Left (PVar _) -> error absurd
         Left (PRecord _) -> error absurd
         Left (PConstr c ps) -> c × (Left <$> ps)
         Left PListEmpty -> cNil × Nil
         Left (PListNonEmpty p o) -> cCons × (Left p : Right o : Nil)
         Right (PListVar _) -> error absurd
         Right PListEnd -> cNil × Nil
         Right (PListNext p o) -> cCons × (Left p : Right o : Nil)
      κ' × α = unlessBwd m c
   in
      orElseBwd κ' (πs' <> πs) #
         (\κ'' -> ContElim (ElimConstr (wrap $ D.fromFoldable (singleton (c × κ'') :: List _)))) *** (α ∨ _)
orElseBwd _ _ = error absurd

-- Going backward, discard all synthesised branches, returning the original singleton branch for c, plus
-- join of annotations on empty lists used for bodies of synthesised branches.
unlessBwd :: forall a. BoundedJoinSemilattice a => Dict (Cont a) -> Ctr -> Cont a × a
unlessBwd m c =
   let
      cs = (ctrs (defined (dataTypeFor c)) # S.toUnfoldable) \\ singleton c
   in
      unsafePartial $ get c m × foldl (∨) bot ((bodyAnn <<< body) <$> cs)
   where
   body :: Partial => Ctr -> Cont a
   body c' = applyN (\(ContElim (ElimVar _ κ)) -> κ) (defined $ arity c') (get c' m)

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
   join _ = error unimplemented

derive instance Eq a => Eq (Expr a)
derive instance Generic (Expr a) _
instance Show a => Show (Expr a) where
   show c = genericShow c

derive instance Eq a => Eq (ListRest a)
derive instance Generic (ListRest a) _
instance Show a => Show (ListRest a) where
   show c = genericShow c

derive instance Eq Pattern
derive instance Generic Pattern _
instance Show Pattern where
   show c = genericShow c

derive instance Eq ListRestPattern
derive instance Generic ListRestPattern _
instance Show ListRestPattern where
   show c = genericShow c

derive instance Eq a => Eq (Clause a)
derive instance Generic (Clause a) _
instance Show a => Show (Clause a) where
   show c = genericShow c

derive instance Eq a => Eq (Clauses a)
derive instance Generic (Clauses a) _
instance Show a => Show (Clauses a) where
   show c = genericShow c

derive instance Eq a => Eq (VarDef a)
derive instance Generic (VarDef a) _
instance Show a => Show (VarDef a) where
   show c = genericShow c

derive instance Eq a => Eq (Qualifier a)
derive instance Generic (Qualifier a) _
instance Show a => Show (Qualifier a) where
   show c = genericShow c