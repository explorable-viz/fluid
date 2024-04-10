module SExpr where

import Prelude hiding (absurd, top, unless)
import Bind (Bind, Var, varAnon, (↦))
import Bind (keys) as B
import Control.Monad.Error.Class (class MonadError)
import Data.Bitraversable (rtraverse)
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (length)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), drop, partition, take, zip, zipWith, (:), (\\))
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList, unsnoc)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (foldl1, (:|))
import Data.Profunctor.Strong (first, second)
import Data.Set (toUnfoldable) as S
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd, uncurry)
import Data.Unfoldable (replicate)
import DataType (Ctr, DataType, arity, cCons, cFalse, cNil, cTrue, ctrs, dataTypeFor)
import Desugarable (class Desugarable, desugBwd, desug)
import Dict as D
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), Module(..), RecDefs(..), VarDef(..)) as E
import Lattice (class BoundedJoinSemilattice, class BoundedLattice, class JoinSemilattice, Raw, bot, botOf, top, (∨))
import Partial.Unsafe (unsafePartial)
import Util (type (+), type (×), Endo, absurd, appendList, assert, defined, definitely', error, nonEmpty, shapeMismatch, singleton, throw, unimplemented, (×), (≜))
import Util.Map (get, lookup)
import Util.Pair (Pair(..))
import Util.Set ((∈))

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

pVarAnon :: Pattern
pVarAnon = PVar varAnon

pListVarAnon :: ListRestPattern
pListVarAnon = PListVar varAnon

ctrFor :: Pattern + ListRestPattern -> Maybe Ctr
ctrFor (Left (PVar _)) = Nothing
ctrFor (Left (PConstr c _)) = pure c
ctrFor (Left (PRecord _)) = Nothing
ctrFor (Left PListEmpty) = pure cNil
ctrFor (Left (PListNonEmpty _ _)) = pure cCons
ctrFor (Right (PListVar _)) = Nothing
ctrFor (Right PListEnd) = pure cNil
ctrFor (Right (PListNext _ _)) = pure cCons

subpatts :: Pattern + ListRestPattern -> List (Pattern + ListRestPattern)
subpatts (Left (PVar _)) = Nil
subpatts (Left (PConstr _ ps)) = Left <$> ps
subpatts (Left (PRecord xps)) = Left <$> (xps <#> snd)
subpatts (Left PListEmpty) = Nil
subpatts (Left (PListNonEmpty p o)) = Left p : Right o : Nil
subpatts (Right (PListVar _)) = Nil
subpatts (Right PListEnd) = Nil
subpatts (Right (PListNext p o)) = Left p : Right o : Nil

newtype Clause a = Clause (NonEmptyList Pattern × Expr a)

type Branch a = Var × Clause a
newtype Clauses a = Clauses (NonEmptyList (Clause a))

newtype RecDef a = RecDef (NonEmptyList (Branch a))
type RecDefs a = NonEmptyList (Branch a)

-- The pattern/expr relationship is different to the one in branch (the expr is the "argument", not the "body").
data VarDef a = VarDef Pattern (Expr a)
type VarDefs a = NonEmptyList (VarDef a)

data Qualifier a
   = ListCompGuard (Expr a)
   | ListCompGen Pattern (Expr a)
   | ListCompDecl (VarDef a) -- could allow VarDefs instead

data Module a = Module (List (VarDefs a + RecDefs a))

instance Desugarable Expr E.Expr where
   desug = exprFwd
   desugBwd = exprBwd

instance Desugarable ListRest E.Expr where
   desug :: forall a m. MonadError Error m => BoundedLattice a => ListRest a -> m (E.Expr a)
   desug (End α) = pure (enil α)
   desug (Next α s l) = econs α <$> desug s <*> desug l

   desugBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw ListRest -> ListRest a
   desugBwd (E.Constr α _ _) (End _) = End α
   desugBwd (E.Constr α _ (e1 : e2 : Nil)) (Next _ s l) =
      Next α (desugBwd e1 s) (desugBwd e2 l)
   desugBwd _ _ = error absurd

instance Desugarable Clauses Elim where
   desug :: forall a m. BoundedLattice a => MonadError Error m => Clauses a -> m (Elim a)
   desug μ = clausesStateFwd (toClausesStateFwd μ) <#> asElim

   desugBwd :: forall a. BoundedJoinSemilattice a => Elim a -> Raw Clauses -> Clauses a
   desugBwd σ μ = toClausesStateBwd (clausesStateBwd (ContElim σ) (toClausesStateFwd μ))

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
   E.VarDef <$> desug (Clauses (singleton (Clause (singleton p × Record top Nil)))) <*> desug s

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
   NonEmptyList (VarDef π (desugBwd e1 s1) :| d' : ds') × s2'
   where
   NonEmptyList (d' :| ds') × s2' = varDefsBwd e2 (NonEmptyList (d :| ds) × s2)
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
recDefFwd xcs = (fst (head (unwrap xcs)) ↦ _) <$> desug (Clauses (snd <$> unwrap xcs))

recDefBwd :: forall a. BoundedJoinSemilattice a => Bind (Elim a) -> Raw RecDef -> RecDef a
recDefBwd (x ↦ σ) (RecDef bs) = RecDef ((x × _) <$> unwrap (desugBwd σ (Clauses (snd <$> bs))))

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
exprFwd (Lambda μ) = E.Lambda top <$> desug μ
exprFwd (Project s x) = E.Project <$> desug s <@> x
exprFwd (App s1 s2) = E.App <$> desug s1 <*> desug s2
exprFwd (BinaryApp s1 op s2) = E.App <$> (E.App (E.Op op) <$> desug s1) <*> desug s2
exprFwd (MatchAs s μ) =
   E.App <$> (E.Lambda top <$> desug (Clauses (Clause <$> first singleton <$> μ))) <*> desug s
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
   Record α $ xss # filterMap \(x ↦ s) -> lookup x xes <#> \e -> x ↦ desugBwd e s
exprBwd (E.Dictionary α ees) (Dictionary _ sss) =
   Dictionary α (zipWith (\(Pair e e') (Pair s s') -> Pair (desugBwd e s) (desugBwd e' s')) ees sss)
exprBwd (E.Matrix α e1 _ e2) (Matrix _ s1 (x × y) s2) =
   Matrix α (desugBwd e1 s1) (x × y) (desugBwd e2 s2)
exprBwd (E.Lambda _ σ) (Lambda μ) = Lambda (desugBwd σ μ)
exprBwd (E.Project e _) (Project s x) = Project (desugBwd e s) x
exprBwd (E.App e1 e2) (App s1 s2) = App (desugBwd e1 s1) (desugBwd e2 s2)
exprBwd (E.App (E.App (E.Op _) e1) e2) (BinaryApp s1 op s2) =
   BinaryApp (desugBwd e1 s1) op (desugBwd e2 s2)
exprBwd (E.App (E.Lambda _ σ) e) (MatchAs s μ) =
   MatchAs (desugBwd e s)
      (first head <$> unwrap <$> unwrap (desugBwd σ (Clauses (Clause <$> first singleton <$> μ))))
exprBwd (E.App (E.Lambda _ (ElimConstr m)) e1) (IfElse s1 s2 s3) =
   IfElse (desugBwd e1 s1)
      (if cTrue ∈ m then desugBwd (asExpr (get cTrue m)) s2 else botOf s2)
      (if cFalse ∈ m then desugBwd (asExpr (get cFalse m)) s3 else botOf s3)
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

-- List Qualifier × Expr
listCompFwd :: forall a m. MonadError Error m => BoundedLattice a => a × List (Qualifier a) × Expr a -> m (E.Expr a)
listCompFwd (α × Nil × s) =
   econs α <$> desug s <@> enil α
listCompFwd (α × (ListCompGuard s : qs) × s') = do
   e <- listCompFwd (α × qs × s')
   E.App (E.Lambda α (elimBool (ContExpr e) (ContExpr (enil α)))) <$> desug s
listCompFwd (α × (ListCompDecl (VarDef p s) : qs) × s') = do
   σ <- clausesStateFwd (((Left p : Nil) × Nil × ListComp α s' qs) : Nil)
   E.App (E.Lambda α (asElim σ)) <$> desug s
listCompFwd (α × (ListCompGen p s : qs) × s') = do
   let ks = orElseFwd α ((Left p : Nil) × ListComp α s' qs)
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
listCompBwd (E.App (E.Lambda α' (ElimConstr m)) e) ((ListCompGuard s0 : qs) × s0') =
   listCompBwd (asExpr (get cTrue m)) (qs × s0') × asExpr (get cFalse m)
      # unsafePartial case _ of
           (α × qs' × s') × E.Constr β c Nil | c == cNil -> (α ∨ α' ∨ β) × (ListCompGuard (desugBwd e s0) : qs') × s'
listCompBwd (E.App (E.Lambda α' σ) e) ((ListCompDecl (VarDef p s0) : qs) × s0') =
   clausesStateBwd (ContElim σ) (((Left p : Nil) × Nil × ListComp unit s0' qs) : Nil)
      # unsafePartial case _ of
           ((Left _ : Nil) × Nil × ListComp α s' qs') : Nil ->
              (α ∨ α') × (ListCompDecl (VarDef p (desugBwd e s0)) : qs') × s'
listCompBwd (E.App (E.App (E.Var "concatMap") (E.Lambda α' σ)) e) ((ListCompGen p s0 : qs) × s0') =
   orElseBwd k (nonEmpty ks <#> unsafePartial \(π × Nil × s') -> π × s')
      # unsafePartial case _ of
           β × ListComp α s' qs' -> (α ∨ α' ∨ β) × (ListCompGen p (desugBwd e s0) : qs') × s'
   where
   k = (Left p : Nil) × ListComp unit s0' qs
   ks = clausesStateBwd (ContElim σ) (toList (orElseFwd unit k <#> second (Nil × _)))
listCompBwd _ _ = error absurd

-- Clauses
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
popConstrFwd d (((p : π') × π'' × s) : ks) =
   assert (length π == defined (arity c) && defined (dataTypeFor c) == d) $
      forConstr c ((π <> π') × π'' × s) <$> popConstrFwd d ks
   where
   π = subpatts p
   c = definitely' (ctrFor p)
popConstrFwd _ Nil = pure Nil

forConstr :: forall a. Ctr -> ClauseState' a -> Endo (List (Ctr × ClausesState' a))
forConstr c k Nil = (c × (k : Nil)) : Nil
forConstr c k ((c' × ks') : cks)
   | c == c' = (c' × (k : ks')) : cks
   | otherwise = (c' × ks') : forConstr c k cks

popConstrBwd :: forall a. List (Ctr × ClausesState' a) -> Raw ClausesState' -> ClausesState' a
popConstrBwd _ ((Nil × _ × _) : _) = error absurd
popConstrBwd kss (((p : π') × π'' × _) : ks) =
   case forConstrBwd (definitely' (ctrFor p)) kss of
      Nothing -> popConstrBwd kss ks
      Just ((π1 × π2 × s) × kss')
         | π1 == subpatts p <> π' && π2 == π'' ->
              ((p : π') × π'' × s) : popConstrBwd kss' ks
         | otherwise -> popConstrBwd kss ks
popConstrBwd _ Nil = Nil

forConstrBwd :: forall a. Ctr -> List (Ctr × ClausesState' a) -> Maybe (ClauseState' a × List (Ctr × ClausesState' a))
forConstrBwd _ Nil = Nothing
forConstrBwd c ((c' × ks) : kss)
   | c == c' = case ks of
        Nil -> Nothing
        k : ks' -> Just (k × (c' × ks') : kss)
   | otherwise = second ((c' × ks) : _) <$> forConstrBwd c kss

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

-- Implementing Desugarable would require another newtype
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
clausesStateFwd ks@(((Right (PListVar x) : _) × _) : _) =
   ContElim <$> ElimVar x <$> (clausesStateFwd =<< popListVarFwd x ks)
clausesStateFwd ks@(((p : _) × _) : _) = do
   kss <- popConstrFwd (defined (dataTypeFor (definitely' (ctrFor p)))) ks
   ContElim <$> ElimConstr <$> wrap <<< D.fromFoldable <$> sequence (rtraverse clausesStateFwd <$> kss)

-- Recovers (subset of) clauses in order consistent with their original order.
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
clausesStateBwd (ContElim (ElimVar x κ)) ks@(((Right (PListVar _) : _) × _) : _) =
   popListVarBwd x (clausesStateBwd κ (defined (popListVarFwd x ks)))
clausesStateBwd (ContElim (ElimConstr m)) ks@(((p : _) × _) : _) =
   popConstrBwd (filterMap (\(c' ↦ ks') -> (c' ↦ _) <$> (clausesStateBwd <$> lookup c' m <@> ks')) kss) ks
   where
   kss = defined (popConstrFwd (defined (dataTypeFor (definitely' (ctrFor p)))) ks)
clausesStateBwd (ContElim _) _ = error (shapeMismatch unit)

-- First component π is stack of subpatterns active during processing of a single top-level pattern p,
-- initially containing only p and empty when the recursion terminates.
type ClauseState a = List (Pattern + ListRestPattern) × Expr a

pushPatts :: forall a. List (Pattern + ListRestPattern) -> Endo (ClauseState a)
pushPatts π (π' × s) = (π <> π') × s

popPatts :: forall a. Int -> ClauseState a -> List (Pattern + ListRestPattern) × ClauseState a
popPatts n (π' × s) = take n π' × drop n π' × s

unless :: Pattern + ListRestPattern -> List (Pattern + ListRestPattern)
unless (Left (PVar _)) = Nil
unless (Left (PRecord _)) = Nil
unless (Left (PConstr c _)) =
   (S.toUnfoldable (ctrs (defined (dataTypeFor c))) \\ singleton c)
      <#> \c' -> Left (PConstr c' (replicate (defined (arity c')) pVarAnon))
unless (Left PListEmpty) = Left (PConstr cCons (replicate 2 pVarAnon)) : Nil
unless (Left (PListNonEmpty _ _)) = Left PListEmpty : Nil
unless (Right (PListVar _)) = Nil
unless (Right (PListNext _ _)) = Right PListEnd : Nil
unless (Right PListEnd) = Right (PListNext pVarAnon pListVarAnon) : Nil

orElseFwd :: forall a. a -> ClauseState a -> NonEmptyList (ClauseState a)
orElseFwd α = case _ of
   Nil × s -> singleton (Nil × s)
   (p : π) × s -> case p of
      Left (PVar _) -> orElseFwd α (π × s) <#> pushPatt p
      Left (PRecord xps) -> under <#> \(π' × k) ->
         pushPatt (Left (PRecord (zip (fst <$> xps) (unsafePartial (\(Left p') -> p') <$> π')))) k
      Left (PConstr c _) -> ks `appendList` ks'
         where
         ks = under <#> \(π' × k) ->
            pushPatt (Left (PConstr c (unsafePartial (\(Left p') -> p') <$> π'))) k
         ks' = unless p <#> \p' -> ((π <#> anon) × ListEmpty α) # pushPatt p'
      Left PListEmpty -> ks `appendList` ks'
         where
         ks = orElseFwd α (π × s) <#> pushPatt (Left PListEmpty)
         ks' = unless p <#> \p' -> ((π <#> anon) × ListEmpty α) # pushPatt p'
      Left (PListNonEmpty _ _) -> ks `appendList` ks'
         where
         ks = under <#> unsafePartial \((Left p' : Right o' : Nil) × k) ->
            pushPatt (Left (PListNonEmpty p' o')) k
         ks' = unless p <#> \p' -> ((π <#> anon) × ListEmpty α) # pushPatt p'
      Right (PListVar _) -> orElseFwd α (π × s) <#> pushPatt p
      Right (PListNext _ _) -> ks `appendList` ks'
         where
         ks = under <#> unsafePartial \((Left p' : Right o' : Nil) × k) ->
            pushPatt (Right (PListNext p' o')) k
         ks' = unless p <#> \p' -> ((π <#> anon) × ListEmpty α) # pushPatt p'
      Right PListEnd -> ks `appendList` ks'
         where
         ks = orElseFwd α (π × s) <#> pushPatt (Right PListEnd)
         ks' = unless p <#> \p' -> ((π <#> anon) × ListEmpty α) # pushPatt p'
      where
      under :: NonEmptyList (List (Pattern + ListRestPattern) × ClauseState a)
      under = popPatts (length π') <$> orElseFwd α (pushPatts π' (π × s))
         where
         π' = subpatts p
   where
   pushPatt :: Pattern + ListRestPattern -> Endo (ClauseState a)
   pushPatt p (π × s) = (p : π) × s

anon :: Pattern + ListRestPattern -> Pattern + ListRestPattern
anon (Left _) = Left pVarAnon
anon (Right _) = Right pListVarAnon

orElseBwd
   :: forall a
    . BoundedJoinSemilattice a
   => Raw ClauseState
   -> NonEmptyList (ClauseState a)
   -> a × Expr a
orElseBwd (π0 × s) ks = case π0 of
   Nil -> case ks of
      NonEmptyList (Nil × s' :| Nil) -> bot × s'
      _ -> error absurd
   p : π -> case p of
      Left (PVar _) -> orElseBwd (π × s) (ks <#> popPatt >>> snd)
      Left (PRecord _) -> under ks'
         where
         ks' = ks <#> popPatt >>> unsafePartial \(p'@(Left (PRecord _)) × k) -> pushPatts (subpatts p') k
      Left (PConstr c _) -> under ks_c'
         # first ((_ :| (ks_not_c <#> snd >>> unsafePartial \(ListEmpty α) -> α)) >>> foldl1 (∨))
         where
         { no: ks_not_c, yes: ks_c } = flip partition (toList ks) case _ of
            (Left (PConstr c' _) : _) × _ -> c' == c
            _ -> false
         ks_c' = nonEmpty ks_c <#>
            popPatt >>> unsafePartial \(p'@(Left (PConstr _ _)) × k) -> pushPatts (subpatts p') k
      Left PListEmpty -> orElseBwd (π × s) ks'
         where
         ks' = popIfPresent (Left (PConstr cCons (replicate 2 pVarAnon)) : (π <#> anon)) ks <#> popPatt >>> snd
      Left (PListNonEmpty _ _) -> under ks'
         where
         ks' = popIfPresent (Left PListEmpty : (π <#> anon)) ks <#>
            popPatt >>> unsafePartial \(p'@(Left (PListNonEmpty _ _)) × k) -> pushPatts (subpatts p') k
      Right (PListVar _) -> orElseBwd (π × s) (ks <#> popPatt >>> snd)
      Right PListEnd -> orElseBwd (π × s) ks'
         where
         ks' = popIfPresent (Right (PListNext pVarAnon pListVarAnon) : (π <#> anon)) ks <#> popPatt >>> snd
      Right (PListNext _ _) -> under ks'
         where
         ks' = popIfPresent (Right PListEnd : (π <#> anon)) ks <#>
            popPatt >>> unsafePartial \(p'@(Right (PListNext _ _)) × k) -> pushPatts (subpatts p') k
      where
      under :: NonEmptyList (ClauseState a) -> a × Expr a
      under = orElseBwd ((subpatts p <> π) × s)
   where
   popPatt :: ClauseState a -> (Pattern + ListRestPattern) × ClauseState a
   popPatt ((p : π) × s') = p × (π × s')
   popPatt _ = error absurd

   popIfPresent :: List (Pattern + ListRestPattern) -> NonEmptyList (ClauseState a) -> NonEmptyList (ClauseState a)
   popIfPresent π ks'' = if π == π' then nonEmpty ks' else ks''
      where
      { init: ks', last: π' × _ } = unsnoc ks

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
      mapDefs g (Right ds) = Right $ (\(x × Clause (π × s)) -> x × Clause (π × (g <$> s))) <$> ds

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
