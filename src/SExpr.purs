module SExpr where

import Prelude hiding (absurd, top)

import Bind (Bind, Var, varAnon, (↦), keys)
import Control.Monad.Error.Class (class MonadError)
import Data.Bitraversable (rtraverse)
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl)
import Data.Function (applyN, on)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), drop, length, sortBy, take, zip, zipWith, (:), (\\))
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.Profunctor.Strong (first, (***))
import Data.Set (Set)
import Data.Set (toUnfoldable) as S
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence, traverse)
import Data.Tuple (uncurry, fst, snd)
import Data.Unfoldable (replicate)
import DataType (Ctr, DataType, arity, cCons, cFalse, cNil, cTrue, checkArity, ctrs, dataTypeFor)
import Debug (trace)
import Desugarable (class Desugarable, desugBwd, desug)
import Dict (Dict)
import Dict (fromFoldable) as D
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), Module(..), RecDefs(..), VarDef(..)) as E
import Lattice (class BoundedJoinSemilattice, class BoundedLattice, class JoinSemilattice, Raw, bot, definedJoin, maybeJoin, top, (∨))
import Partial.Unsafe (unsafePartial)
import Util (type (+), type (×), Endo, absurd, appendList, assert, definitelyFromLeft, error, shapeMismatch, singleton, successful, throw, unimplemented, (×), (≜))
import Util.Map (asMaplet, get, maplet)
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
   = PListEnd
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

varDefFwd :: forall a m. MonadError Error m => BoundedLattice a => VarDef a -> m (E.VarDef a)
varDefFwd (VarDef π s) = E.VarDef <$> pattContFwd π (ContNone :: Cont a) <*> desug s

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
exprBwd (E.Lambda _ σ) (Lambda bs) = Lambda (clausesBwd σ bs)
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
listCompFwd (α × (Declaration (VarDef π s) : qs) × s') = do
   e <- ContExpr <$> listCompFwd (α × qs × s')
   σ <- pattContFwd π e
   E.App (E.Lambda α σ) <$> desug s
listCompFwd (α × (Generator p s : qs) × s') = do
   e <- ContExpr <$> listCompFwd (α × qs × s')
   σ <- pattContFwd p e
   E.App (E.App (E.Var "concatMap") (E.Lambda α (asElim (orElseFwd (ContElim σ) α)))) <$> desug s

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
pattContFwd :: forall a m. MonadError Error m => Pattern -> Cont a -> m (Elim a)
pattContFwd (PVar x) κ = pure (ElimVar x κ)
pattContFwd (PConstr c ps) κ =
   checkArity c (length ps) *> (ElimConstr <$> maplet c <$> pattArgsFwd (Left <$> ps) κ)
pattContFwd (PRecord xps) κ =
   ElimRecord (keys xps) <$> pattArgsFwd ((snd >>> Left) <$> sortBy (compare `on` fst) xps) κ
pattContFwd PListEmpty κ = pure (ElimConstr (maplet cNil κ))
pattContFwd (PListNonEmpty p o) κ = ElimConstr <$> maplet cCons <$> pattArgsFwd (Left p : Right o : Nil) κ

pattContBwd :: forall a. Pattern -> Elim a -> Cont a
pattContBwd (PVar _) (ElimVar _ κ) = κ
pattContBwd (PConstr c ps) (ElimConstr m) = pattArgsBwd (Left <$> ps) (get c m)
pattContBwd (PListEmpty) (ElimConstr m) = get cNil m
pattContBwd (PListNonEmpty p o) (ElimConstr m) = pattArgsBwd (Left p : Right o : Nil) (get cCons m)
pattContBwd (PRecord xps) (ElimRecord _ κ) = pattArgsBwd ((snd >>> Left) <$> sortBy (compare `on` fst) xps) κ
pattContBwd _ _ = error absurd

-- ListRestPattern × Cont
pattCont_ListRest_Fwd :: forall a m. MonadError Error m => ListRestPattern -> Cont a -> m (Elim a)
pattCont_ListRest_Fwd PListEnd κ = pure (ElimConstr (maplet cNil κ))
pattCont_ListRest_Fwd (PListNext p o) κ = ElimConstr <$> maplet cCons <$> pattArgsFwd (Left p : Right o : Nil) κ

pattCont_ListRest_Bwd :: forall a. Elim a -> ListRestPattern -> Cont a
pattCont_ListRest_Bwd (ElimVar _ _) _ = error absurd
pattCont_ListRest_Bwd (ElimRecord _ _) _ = error absurd
pattCont_ListRest_Bwd (ElimConstr m) PListEnd = get cNil m
pattCont_ListRest_Bwd (ElimConstr m) (PListNext p o) = pattArgsBwd (Left p : Right o : Nil) (get cCons m)

-- List (Pattern + ListRestPattern) × Cont
pattArgsFwd :: forall a m. MonadError Error m => List (Pattern + ListRestPattern) -> Cont a -> m (Cont a)
pattArgsFwd Nil κ = pure κ
pattArgsFwd (Left p : πs) κ = ContElim <$> (pattArgsFwd πs κ >>= pattContFwd p)
pattArgsFwd (Right o : πs) κ = ContElim <$> (pattArgsFwd πs κ >>= pattCont_ListRest_Fwd o)

pattArgsBwd :: forall a. List (Pattern + ListRestPattern) -> Endo (Cont a)
pattArgsBwd Nil κ = κ
pattArgsBwd (Left p : πs) σ = pattArgsBwd πs (pattContBwd p (asElim σ))
pattArgsBwd (Right o : πs) σ = pattArgsBwd πs (pattCont_ListRest_Bwd (asElim σ) o)

-- Clauses
clausesFwd :: forall a m. BoundedLattice a => MonadError Error m => JoinSemilattice a => Clauses a -> m (Elim a)
clausesFwd (Clauses bs) = do
   -- REMOVE ME
   σ' <- clausesFwd_New (Clauses bs)
   trace
      (σ' × orElse_NewFwd (ListEmpty bot) (first (toList >>> (Left <$> _)) (unwrap (head bs))))
      \_ -> do
         NonEmptyList (σ :| σs) <- traverse pattsExprFwd (unwrap <$> bs)
         foldM maybeJoin σ σs
   where
   pattsExprFwd :: NonEmptyList Pattern × Expr a -> m (Elim a)
   pattsExprFwd (NonEmptyList (p :| Nil) × s) = (ContExpr <$> desug s) >>= pattContFwd p
   pattsExprFwd (NonEmptyList (p :| p' : ps) × s) =
      pattContFwd p =<< ContExpr <$> E.Lambda top <$> pattsExprFwd (NonEmptyList (p' :| ps) × s)

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

clausesFwd_New :: forall a m. BoundedLattice a => MonadError Error m => Clauses a -> m (Elim a)
clausesFwd_New (Clauses cl) = asElim <$> wurble ks
   where
   ks = toList cl <#> \(Clause (NonEmptyList (p :| π) × s)) -> (Left p : Nil) × π × s

-- Like ClauseState but for curried functions; extra component π' stores remaining top-level patterns.
type ClauseState' a = List (Pattern + ListRestPattern) × List Pattern × Expr a
type ClausesState' a = List (ClauseState' a)

popArg :: forall a m. MonadError Error m => ClausesState' a -> m (ClausesState' a)
popArg ((Nil × (p : π) × s) : ks) = (((Left p : Nil) × π × s) : _) <$> popArg ks
popArg Nil = pure Nil
popArg _ = throw (shapeMismatch unit)

popVar :: forall a m. MonadError Error m => Var -> ClausesState' a -> m (ClausesState' a)
popVar x (((Left (PVar x') : π) × π' × s) : ks) = ((π × π' × s) : _) <$> popVar (x ≜ x') ks
popVar _ Nil = pure Nil
popVar _ _ = throw (shapeMismatch unit)

popConstr :: forall a m. MonadError Error m => DataType -> ClausesState' a -> m (List (Ctr × ClausesState' a))
popConstr d (((Left (PConstr c π) : π') × π'' × s) : ks) =
   assert (length π == successful (arity c) && successful (dataTypeFor c) == d) $
      forConstr c (((Left <$> π) <> π') × π'' × s) <$> popConstr d ks
popConstr _ Nil = pure Nil
popConstr _ _ = throw (shapeMismatch unit)

forConstr :: forall a. Ctr -> ClauseState' a -> Endo (List (Ctr × ClausesState' a))
forConstr c k Nil = (c × (k : Nil)) : Nil
forConstr c k ((c' × ks') : cks)
   | c == c' = (c' × (k : ks')) : cks
   | otherwise = (c' × ks') : forConstr c k cks

popRecord :: forall a m. MonadError Error m => Set Var -> ClausesState' a -> m (ClausesState' a)
popRecord xs (((Left (PRecord xps) : π) × π' × s) : ks) =
   assert (keys xps == xs) $ ((((xps <#> snd >>> Left) <> π) × π' × s) : _) <$> popRecord xs ks
popRecord _ Nil = pure Nil
popRecord _ _ = throw (shapeMismatch unit)

-- RENAME
wurble :: forall a m. BoundedLattice a => MonadError Error m => ClausesState' a -> m (Cont a)
wurble Nil = error absurd
wurble ((Nil × Nil × s) : Nil) =
   ContExpr <$> desug s
wurble ks@((Nil × _) : _) =
   ContExpr <$> E.Lambda top <$> asElim <$> (wurble =<< popArg ks)
wurble ks@(((Left (PVar x) : _) × _) : _) =
   ContElim <$> ElimVar x <$> (wurble =<< popVar x ks)
wurble ks@(((Left (PRecord xps) : _) × _) : _) =
   ContElim <$> ElimRecord (keys xps) <$> (wurble =<< popRecord (keys xps) ks)
wurble ks@(((Left (PConstr c _) : _) × _) : _) = do
   ckls <- popConstr (successful (dataTypeFor c)) ks
   ContElim <$> ElimConstr <$> wrap <<< D.fromFoldable <$> sequence (rtraverse wurble <$> ckls)
wurble (((Left PListEmpty : _) × _) : _) =
   error unimplemented
wurble (((Left (PListNonEmpty _ _) : _) × _) : _) =
   error unimplemented
wurble (((Right PListEnd : _) × _) : _) =
   error unimplemented
wurble (((Right (PListNext _ _) : _) × _) : _) =
   error unimplemented

-- First component π is stack of subpatterns active during processing of a single top-level pattern p,
-- initially containing only p and ending up empty.
type ClauseState a = List (Pattern + ListRestPattern) × Expr a

pushPatt :: forall a. Pattern + ListRestPattern -> Endo (ClauseState a)
pushPatt p = pushPatts (singleton p)

pushPatts :: forall a. List (Pattern + ListRestPattern) -> Endo (ClauseState a)
pushPatts π1 (π × s) = (π1 <> π) × s

popPatts :: forall a. Int -> ClauseState a -> List (Pattern + ListRestPattern) × ClauseState a
popPatts n (π × s) = take n π × drop n π × s

withPatts
   :: forall a
    . List (Pattern + ListRestPattern)
   -> (ClauseState a -> NonEmptyList (ClauseState a))
   -> ClauseState a
   -> NonEmptyList (List (Pattern + ListRestPattern) × ClauseState a)
withPatts π f k = popPatts (length π) <$> f (pushPatts π k)

orElse_NewFwd :: forall a. Expr a -> ClauseState a -> NonEmptyList (ClauseState a)
orElse_NewFwd _ (Nil × s) = singleton (Nil × s)
orElse_NewFwd s' ((Left (PVar x) : π) × s) =
   orElse_NewFwd s' (π × s) <#> pushPatt (Left (PVar x))
orElse_NewFwd s' ((Left (PConstr c π) : π') × s) = ks `appendList` ks'
   where
   ks = withPatts (Left <$> π) (orElse_NewFwd s') (π' × s)
      <#> (\(π1 × k) -> pushPatt (Left (PConstr c (definitelyFromLeft <$> π1))) k)
   cs = (ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ singleton c
   ks' = cs <#> \c' -> ((π' <#> anon) × s')
      # pushPatt (Left (PConstr c' (replicate (successful (arity c')) (PVar varAnon))))
orElse_NewFwd s' ((Left (PRecord xps) : π) × s) =
   withPatts (Left <<< snd <$> xps) (orElse_NewFwd s') (π × s)
      <#> (\(π1 × k) -> pushPatt (Left (PRecord (zip (fst <$> xps) (definitelyFromLeft <$> π1)))) k)
orElse_NewFwd s' ((Left PListEmpty : π) × s) =
   orElse_NewFwd s' (π × s) <#> pushPatt (Left PListEmpty)
orElse_NewFwd s' ((Left (PListNonEmpty p o) : π) × s) =
   withPatts (Left p : Right o : Nil) (orElse_NewFwd s') (π × s) <#> uncurry pushPatts
orElse_NewFwd s' ((Right (PListNext p o) : π) × s) =
   withPatts (Left p : Right o : Nil) (orElse_NewFwd s') (π × s) <#> uncurry pushPatts
orElse_NewFwd s' ((Right PListEnd : π) × s) =
   orElse_NewFwd s' (π × s) <#> pushPatt (Right PListEnd)

anon :: Pattern + ListRestPattern -> Pattern + ListRestPattern
anon (Left _) = Left (PVar varAnon)
anon (Right o) = Right (anonListRest o)
   where
   anonListRest :: ListRestPattern -> ListRestPattern
   anonListRest PListEnd = PListEnd
   anonListRest (PListNext _ o') = PListNext (PVar varAnon) (anonListRest o')

-- orElse
orElseFwd :: forall a. Cont a -> a -> Cont a
orElseFwd ContNone _ = error absurd
orElseFwd (ContExpr e) _ = ContExpr e
orElseFwd (ContElim (ElimConstr m)) α = ContElim (ElimConstr (unlessFwd (c × orElseFwd κ α) α))
   where
   c × κ = asMaplet m
orElseFwd (ContElim (ElimRecord xs κ)) α = ContElim (ElimRecord xs (orElseFwd κ α))
orElseFwd (ContElim (ElimVar x κ)) α = ContElim (ElimVar x (orElseFwd κ α))

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
         Right PListEnd -> cNil × Nil
         Right (PListNext p o) -> cCons × (Left p : Right o : Nil)
      κ' × α = unlessBwd m c
   in
      orElseBwd κ' (πs' <> πs) #
         (\κ'' -> ContElim (ElimConstr (wrap $ D.fromFoldable (singleton (c × κ'') :: List _)))) *** (α ∨ _)
orElseBwd _ _ = error absurd

-- In forward direction, extend singleton branch to set of branches where any missing constructors have
-- been mapped to the empty list, using anonymous variables in any generated patterns. Going backward, discard
-- all synthesised branches, returning the original singleton branch for c, plus join of annotations on the
-- empty lists used for bodies of synthesised branches.
unlessFwd :: forall a. Ctr × Cont a -> a -> Dict (Cont a)
unlessFwd (c × κ) α = wrap $ D.fromFoldable ((c × κ) : cκs)
   where
   defaultBranch c' = c' × applyN (ContElim <<< ElimVar varAnon) (successful (arity c')) (ContExpr (enil α))
   cκs = defaultBranch <$> ((ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ singleton c)

unlessBwd :: forall a. BoundedJoinSemilattice a => Dict (Cont a) -> Ctr -> Cont a × a
unlessBwd m c =
   let
      cs = (ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ singleton c
   in
      unsafePartial $ get c m × foldl (∨) bot ((bodyAnn <<< body) <$> cs)
   where
   body :: Partial => Ctr -> Cont a
   body c' = applyN (\(ContElim (ElimVar _ κ)) -> κ) (successful $ arity c') (get c' m)

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