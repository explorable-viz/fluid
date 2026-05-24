module SExpr where

import Prelude hiding (absurd, top, unless)

import Bind (Bind, Var, varAnon, (↦))
import Bind (keys) as B
import Data.Set (Set, empty, insert, member, singleton, unions) as Set
import Control.Monad.Error.Class (class MonadError)
import Data.Bitraversable (rtraverse)
import Data.Either (Either(..))
import Data.Foldable (for_, length)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), drop, take, unzip, zip, zipWith, (:))
import Data.List (difference) as L
import Data.List.NonEmpty (NonEmptyList(..), foldr, groupBy, head, toList)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Profunctor.Strong (first, second)
import Data.Set (toUnfoldable) as S
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd)
import Data.Unfoldable (replicate)
import DataType (Ctr, DataType, arity, cCons, cParagraph, cFalse, cNil, cTrue, ctrs, dataTypeFor)
import Desugarable (class Desugarable, desug)
import Dict as D
import Effect.Exception (Error)
import Expr (class BV, class FV, Cont(..), Elim(..), asElim, bv, fv)
import Expr (Expr(..), Module(..), RecDefs(..), Stmt(..), VarDef(..)) as E
import Util.Set ((\\), (∪))
import Lattice (class BoundedLattice, class JoinSemilattice, bot, top)
import Partial.Unsafe (unsafePartial)
import Util (type (+), type (×), Endo, absurd, appendList, assert, defined, definitely, error, shapeMismatch, singleton, throw, unimplemented, (×), (≜))
import Util.Pair (Pair(..))

-- Surface language expressions.

data Expr a
   = Var Var
   | Op Var
   | Int a Int
   | Float a Number
   | Str a String
   | Constr a Ctr (List (Expr a))
   | Dictionary a (List (DictEntry a × Expr a))
   | Matrix a (Expr a) (Var × Var) (Expr a)
   | Lambda (LambdaClause a)
   | Project (Expr a) Var
   | DProject (Expr a) (Expr a)
   | App (Expr a) (Expr a)
   | BinaryApp (Expr a) Var (Expr a)
   | UnaryPrefixApp Var (Expr a)
   | Ternary (Expr a) (Expr a) (Expr a)
   | Paragraph (Paragraph a)
   | ListEmpty a
   | ListNonEmpty a (Expr a) (ListRest a)
   | ListEnum (Expr a) (Expr a)
   | ListComp a (Expr a) (List (Qualifier a))
   | DocExpr (Expr a) (Expr a)

data DictEntry a = ExprKey (Expr a) | VarKey a Var

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

data ParagraphElem a = Token String | Unquote (Expr a)
type Paragraph a = List (ParagraphElem a)

pVarAnon :: Pattern
pVarAnon = PVar varAnon

pListVarAnon :: ListRestPattern
pListVarAnon = PListVar varAnon

showPattern :: Pattern + ListRestPattern -> String
showPattern (Left p') = show p'
showPattern (Right p') = show p'

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

data Stmt a
   = Return (Expr a)
   | If (NonEmptyList (Expr a × Stmt a)) (Maybe (Stmt a))
   | Match (Expr a) (NonEmptyList (Pattern × Stmt a))
   | Def (VarDef a)
   | DefRec (RecDefs a)
   | Pass
   | ExprStmt (Expr a)
   | Assert (Expr a) (Maybe (Expr a))
   | Seq (Stmt a) (Stmt a)

data Clause a = Clause a (NonEmptyList Pattern × Stmt a)

type Branch a = Var × Clause a
newtype Clauses a = Clauses (NonEmptyList (Clause a))

-- Lambdas accept exactly one clause whose body is an expression (no defs / return-keyword).
newtype LambdaClause a = LambdaClause (NonEmptyList Pattern × Expr a)

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

instance Desugarable DictEntry E.Expr where
   desug (ExprKey e) = desug e
   desug (VarKey α v) = pure (E.Str α v)

instance Desugarable Expr E.Expr where
   desug = exprFwd

instance Desugarable Stmt E.Stmt where
   desug = stmtFwd

instance Desugarable ListRest E.Expr where
   desug :: forall a m. MonadError Error m => BoundedLattice a => ListRest a -> m (E.Expr a)
   desug (End α) = pure (enil α)
   desug (Next α s l) = econs α <$> desug s <*> desug l

instance Desugarable Clauses Elim where
   desug :: forall a m. BoundedLattice a => MonadError Error m => Clauses a -> m (Elim a)
   desug μ = clausesStateFwd (toClausesStateFwd μ) <#> asElim

instance Desugarable LambdaClause Elim where
   desug :: forall a m. BoundedLattice a => MonadError Error m => LambdaClause a -> m (Elim a)
   desug (LambdaClause (ps × e)) = desug (Clauses (singleton (Clause bot (ps × Return e))))

desugarModuleFwd :: forall a m. MonadError Error m => BoundedLattice a => Module a -> m (E.Module a)
desugarModuleFwd = moduleFwd

-- helpers
enil :: forall a. a -> E.Expr a
enil α = E.Constr α cNil Nil

econs :: forall a. a -> E.Expr a -> E.Expr a -> E.Expr a
econs α e e' = E.Constr α cCons (e : e' : Nil)

elimBool :: forall a. Cont a -> Cont a -> Elim a
elimBool κ κ' = ElimConstr (D.fromFoldable [ cTrue × κ, cFalse × κ' ])

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
-- "rest of module" to use as continuation. So use empty dictionary (unit tuple) as continuation, and disregard
-- in evaluation.
varDefFwd :: forall a m. MonadError Error m => BoundedLattice a => VarDef a -> m (E.VarDef a)
varDefFwd (VarDef p s) =
   E.VarDef <$> desug (Clauses (singleton (Clause bot (singleton p × Return (Dictionary top Nil))))) <*> desug s

recDefsFwd :: forall a m. MonadError Error m => BoundedLattice a => RecDefs a -> m (E.RecDefs a)
recDefsFwd xcs = do
   let xcss = map RecDef (groupBy (eq `on` fst) xcs)
   let names = (fst <<< head <<< unwrap) <$> toList xcss
   for_ (firstDuplicate names) \x ->
      throw $ "Non-contiguous clauses for: " <> x
   E.RecDefs top <$> D.fromFoldable <$> traverse recDefFwd xcss
   where
   firstDuplicate :: List Var -> Maybe Var
   firstDuplicate = go Set.empty
      where
      go _ Nil = Nothing
      go seen (x : xs)
         | x `Set.member` seen = Just x
         | otherwise = go (Set.insert x seen) xs

-- RecDef
recDefFwd :: forall a m. MonadError Error m => BoundedLattice a => RecDef a -> m (Bind (Elim a))
recDefFwd xcs = (fst (head (unwrap xcs)) ↦ _) <$> desug (Clauses (snd <$> unwrap xcs))

paragraphFwd :: forall m a. BoundedLattice a => MonadError Error m => List (ParagraphElem a) -> m (E.Expr a)
paragraphFwd elems = do
   es <- paragraphElemsFwd elems
   pure (E.Constr bot cParagraph (es : Nil))

paragraphElemsFwd
   :: forall a m
    . BoundedLattice a
   => MonadError Error m
   => List (ParagraphElem a)
   -> m (E.Expr a)
paragraphElemsFwd Nil = pure (enil bot)
paragraphElemsFwd (Token s : elems) = do
   e' <- paragraphElemsFwd elems
   pure (econs bot (E.Str bot s) e')
paragraphElemsFwd (Unquote s : elems) = do
   e <- desug s
   e' <- paragraphElemsFwd elems
   pure (econs bot e e')

-- Expr
exprFwd :: forall a m. BoundedLattice a => MonadError Error m => JoinSemilattice a => Expr a -> m (E.Expr a)
exprFwd (Var x) =
   pure $ E.Var x
exprFwd (Op op) =
   pure $ E.Op op
exprFwd (Int α n) =
   pure $ E.Int α n
exprFwd (Float α n) =
   pure $ (E.Float α n)
exprFwd (Str α s) =
   pure $ E.Str α s
exprFwd (Constr α c ss) =
   E.Constr α c <$> traverse desug ss
exprFwd (Dictionary α sss) = do
   let ks × ss = unzip sss
   ks' <- traverse desug ks
   es <- traverse desug ss
   E.Dictionary α <$> pure (zipWith Pair ks' es)
exprFwd (Matrix α s (x × y) s') =
   E.Matrix α <$> desug s <@> x × y <*> desug s'
exprFwd (Lambda μ) =
   E.Lambda top <$> desug μ
exprFwd (Project s x) =
   E.DProject <$> desug s <@> E.Str top x
exprFwd (DProject s x) =
   E.DProject <$> desug s <*> desug x
exprFwd (App s1 s2) =
   E.App <$> desug s1 <*> desug s2
exprFwd (BinaryApp s1 op s2) =
   E.App <$> (E.App (E.Op op) <$> desug s1) <*> desug s2
exprFwd (UnaryPrefixApp op s) =
   E.App (E.Op op) <$> desug s
exprFwd (Ternary cond e1 e2) =
   E.App
      <$> (E.Lambda top <$> (elimBool <$> (ContStmt <$> E.Return <$> desug e1) <*> (ContStmt <$> E.Return <$> desug e2)))
      <*> desug cond
exprFwd (Paragraph elems) =
   paragraphFwd elems
exprFwd (ListEmpty α) =
   pure $ enil α
exprFwd (ListNonEmpty α s l) =
   econs α <$> desug s <*> desug l
exprFwd (ListEnum s1 s2) =
   E.App
      <$> (E.App (E.Var "range") <$> desug s1)
      <*> (E.App <$> (E.App (E.Op "+") <$> desug s2) <@> (E.Int top 1))
exprFwd (ListComp α s (ListCompGen p s' : qs)) = unsafePartial $
   listCompFwd (α × (ListCompGen p s' : qs) × s)
exprFwd (ListComp α s qs) =
   listCompFwd (α × qs × s)
exprFwd (DocExpr s s') = do
   e <- exprFwd s
   e' <- exprFwd s'
   pure $ E.DocExpr e e'

type IfElseClauses a = NonEmptyList (Expr a × Stmt a) × Stmt a

stmtFwd :: forall a m. BoundedLattice a => MonadError Error m => Stmt a -> m (E.Stmt a)
stmtFwd (Def vd) = E.Def <$> varDefFwd vd
stmtFwd (DefRec xcs) = E.DefRec <$> recDefsFwd xcs
stmtFwd (Match s μ) = do
   κ <- clausesStateFwd (toClausesStateFwd (Clauses (Clause bot <$> first singleton <$> μ)))
   E.Match <$> desug s <@> asElim κ
stmtFwd (If sss s) = ifElseFwd (sss × fromMaybe Pass s)
stmtFwd (Return e) = E.Return <$> desug e
stmtFwd Pass = pure E.Pass
stmtFwd (ExprStmt e) = E.ExprStmt <$> desug e
stmtFwd (Assert cond msg_opt) =
   stmtFwd (If (singleton (App (Var "not") cond × ExprStmt (App (Var "error") msg))) Nothing)
   where
   msg = fromMaybe (Str top "AssertionError") msg_opt
stmtFwd (Seq s1 s2) = E.Seq <$> stmtFwd s1 <*> stmtFwd s2

ifElseFwd :: forall a m. BoundedLattice a => MonadError Error m => IfElseClauses a -> m (E.Stmt a)
ifElseFwd (sss × s) =
   foldr clause (stmtFwd s) sss
   where
   clause (s1 × b) e3 = do
      cond <- desug s1
      b' <- stmtFwd b
      e3' <- e3
      pure $ E.Match cond (elimBool (ContStmt b') (ContStmt e3'))

-- List Qualifier × Expr
listCompFwd :: forall a m. MonadError Error m => BoundedLattice a => a × List (Qualifier a) × Expr a -> m (E.Expr a)
listCompFwd (α × Nil × s) =
   econs α <$> desug s <@> enil α
listCompFwd (α × (ListCompGuard s : qs) × s') = do
   e <- listCompFwd (α × qs × s')
   E.App (E.Lambda α (elimBool (ContStmt (E.Return e)) (ContStmt (E.Return (enil α))))) <$> desug s
listCompFwd (α × (ListCompDecl (VarDef p s) : qs) × s') = do
   σ <- clausesStateFwd (((Left p : Nil) × Nil × Return (ListComp α s' qs)) : Nil)
   E.App (E.Lambda α (asElim σ)) <$> desug s
listCompFwd (α × (ListCompGen p s : qs) × s') = do
   let ks = orElseFwd α ((Left p : Nil) × Return (ListComp α s' qs))
   σ <- clausesStateFwd (toList (ks <#> second (Nil × _)))
   E.App (E.App (E.Var "concat_map") (E.Lambda α (asElim σ))) <$> desug s

-- Clauses
toClausesStateFwd :: forall a. Clauses a -> ClausesState' a
toClausesStateFwd (Clauses μ) = toList μ <#> toClauseStateFwd
   where
   toClauseStateFwd :: Clause a -> ClauseState' a
   toClauseStateFwd (Clause _ (NonEmptyList (p :| π) × b)) = (Left p : Nil) × π × b

-- Like ClauseState but for curried functions; extra component π' stores remaining top-level patterns.
type ClauseState' a = List (Pattern + ListRestPattern) × List Pattern × Stmt a
type ClausesState' a = List (ClauseState' a)

popArgFwd :: forall a m. MonadError Error m => ClausesState' a -> m (ClausesState' a)
popArgFwd ((Nil × (p : π) × s) : ks) = (((Left p : Nil) × π × s) : _) <$> popArgFwd ks
popArgFwd Nil = pure Nil
popArgFwd _ = throw (shapeMismatch unit)

popVarFwd :: forall a m. MonadError Error m => Var -> ClausesState' a -> m (ClausesState' a)
popVarFwd x (((Left (PVar x') : π) × π' × s) : ks) = ((π × π' × s) : _) <$> popVarFwd (x ≜ x') ks
popVarFwd _ Nil = pure Nil
popVarFwd _ _ = throw (shapeMismatch unit)

popListVarFwd :: forall a m. MonadError Error m => Var -> ClausesState' a -> m (ClausesState' a)
popListVarFwd x (((Right (PListVar x') : π) × π' × s) : ks) = ((π × π' × s) : _) <$> popListVarFwd (x ≜ x') ks
popListVarFwd _ Nil = pure Nil
popListVarFwd _ _ = throw (shapeMismatch unit)

popConstrFwd :: forall a m. MonadError Error m => DataType -> ClausesState' a -> m (List (Ctr × ClausesState' a))
popConstrFwd _ ((Nil × _ × _) : _) = error absurd
popConstrFwd d (((p : π') × π'' × s) : ks) =
   assert (length π == defined (arity c) && defined (dataTypeFor c) == d) $
      forConstrFwd c ((π <> π') × π'' × s) <$> popConstrFwd d ks
   where
   π = subpatts p
   c = definitely ("Failed to distinguish constructor: " <> showPattern p) (ctrFor p)
popConstrFwd _ Nil = pure Nil

forConstrFwd :: forall a. Ctr -> ClauseState' a -> Endo (List (Ctr × ClausesState' a))
forConstrFwd c k Nil = (c × (k : Nil)) : Nil
forConstrFwd c k ((c' × ks') : cks)
   | c == c' = (c' × (k : ks')) : cks
   | otherwise = (c' × ks') : forConstrFwd c k cks

popRecordFwd :: forall a m. MonadError Error m => List Var -> ClausesState' a -> m (ClausesState' a)
popRecordFwd xs (((Left (PRecord xps) : π) × π' × s) : ks) =
   assert ((xps <#> fst) == xs) $ ((((xps <#> snd >>> Left) <> π) × π' × s) : _) <$> popRecordFwd xs ks
popRecordFwd _ Nil = pure Nil
popRecordFwd _ _ = throw (shapeMismatch unit)

-- Implementing Desugarable would require another newtype
clausesStateFwd :: forall a m. BoundedLattice a => MonadError Error m => ClausesState' a -> m (Cont a)
clausesStateFwd ks = case ks of
   Nil -> error absurd
   (Nil × Nil × b) : Nil ->
      ContStmt <$> stmtFwd b
   (Nil × _) : _ ->
      ContStmt <$> E.Return <$> E.Lambda top <$> asElim <$> (clausesStateFwd =<< popArgFwd ks)
   ((Left (PVar x) : _) × _) : _ ->
      ContElim <$> ElimVar x <$> (clausesStateFwd =<< popVarFwd x ks)
   ((Left (PRecord xps) : _) × _) : _ ->
      ContElim <$> ElimDict (B.keys xps) <$> (clausesStateFwd =<< popRecordFwd (xps <#> fst) ks)
   ((Right (PListVar x) : _) × _) : _ ->
      ContElim <$> ElimVar x <$> (clausesStateFwd =<< popListVarFwd x ks)
   ((p : _) × _) : _ -> do
      kss <- popConstrFwd (defined (dataTypeFor (definitely ("clausesStateFwd ctrFor failed for: " <> showPattern p) (ctrFor p)))) ks
      ContElim <$> ElimConstr <$> D.fromFoldable <$> sequence (rtraverse clausesStateFwd <$> kss)

-- First component π is stack of subpatterns active during processing of a single top-level pattern p,
-- initially containing only p and empty when the recursion terminates.
type ClauseState a = List (Pattern + ListRestPattern) × Stmt a

unless :: Pattern + ListRestPattern -> List (Pattern + ListRestPattern)
unless (Left (PVar _)) = Nil
unless (Left (PRecord _)) = Nil
unless (Left (PConstr c _)) =
   (S.toUnfoldable (ctrs (defined (dataTypeFor c))) `L.difference` singleton c)
      <#> \c' -> Left (PConstr c' (replicate (defined (arity c')) pVarAnon))
unless (Left PListEmpty) = Left (PConstr cCons (replicate 2 pVarAnon)) : Nil
unless (Left (PListNonEmpty _ _)) = Left PListEmpty : Nil
unless (Right (PListVar _)) = Nil
unless (Right (PListNext _ _)) = Right PListEnd : Nil
unless (Right PListEnd) = Right (PListNext pVarAnon pListVarAnon) : Nil

orElseFwd :: forall a. a -> ClauseState a -> NonEmptyList (ClauseState a)
orElseFwd α = case _ of
   Nil × s -> singleton (Nil × s)
   (p : π) × s ->
      (orElseFwd α ((π' <> π) × s) <#> popPatts (length π') <#> pushPattFor p)
         `appendList`
            (unless p <#> \p' -> ((π <#> anon) × Return (ListEmpty α)) # pushPatt p')
      where
      π' = subpatts p
   where
   pushPatt :: Pattern + ListRestPattern -> Endo (ClauseState a)
   pushPatt p (π × s) = (p : π) × s

   popPatts :: Int -> ClauseState a -> List (Pattern + ListRestPattern) × ClauseState a
   popPatts n (π' × s) = take n π' × drop n π' × s

   pushPattFor :: Pattern + ListRestPattern -> List (Pattern + ListRestPattern) × ClauseState a -> ClauseState a
   pushPattFor (Left (PVar x)) = \(_ × k) ->
      pushPatt (Left (PVar x)) k
   pushPattFor (Left (PRecord xps)) = \(π × k) ->
      pushPatt (Left (PRecord (zip (fst <$> xps) (unsafePartial (\(Left p) -> p) <$> π)))) k
   pushPattFor (Left (PConstr c _)) = \(π × k) ->
      pushPatt (Left (PConstr c (unsafePartial (\(Left p) -> p) <$> π))) k
   pushPattFor (Left PListEmpty) = \(_ × k) ->
      pushPatt (Left PListEmpty) k
   pushPattFor (Left (PListNonEmpty _ _)) = unsafePartial \((Left p : Right o : Nil) × k) ->
      pushPatt (Left (PListNonEmpty p o)) k
   pushPattFor (Right (PListVar x)) = \(_ × k) ->
      pushPatt (Right (PListVar x)) k
   pushPattFor (Right (PListNext _ _)) = unsafePartial \((Left p : Right o : Nil) × k) ->
      pushPatt (Right (PListNext p o)) k
   pushPattFor (Right PListEnd) = \(_ × k) ->
      pushPatt (Right PListEnd) k

anon :: Pattern + ListRestPattern -> Pattern + ListRestPattern
anon (Left _) = Left pVarAnon
anon (Right _) = Right pListVarAnon

-- ======================
-- boilerplate
-- ======================
derive instance Newtype (Clauses a) _
derive instance Newtype (LambdaClause a) _
derive instance Newtype (RecDef a) _
derive instance Functor Stmt
derive instance Functor Clause
derive instance Functor Clauses
derive instance Functor LambdaClause
derive instance Functor DictEntry
derive instance Functor ListRest
derive instance Functor VarDef
derive instance Functor Qualifier
derive instance Functor ParagraphElem
derive instance Functor Expr

instance Functor Module where
   map f (Module defs) = Module (mapDefs f <$> defs)
      where
      mapDefs :: forall a b. (a -> b) -> VarDefs a + RecDefs a -> VarDefs b + RecDefs b
      mapDefs g (Left ds) = Left $ map g <$> ds
      mapDefs g (Right ds) = Right $ (\(x × Clause α (π × b)) -> x × Clause (g α) (π × (g <$> b))) <$> ds

instance JoinSemilattice a => JoinSemilattice (Expr a) where
   join _ = error unimplemented

derive instance Eq a => Eq (DictEntry a)
derive instance Generic (DictEntry a) _
instance Show a => Show (DictEntry a) where
   show c = genericShow c

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

derive instance Eq a => Eq (Stmt a)
derive instance Generic (Stmt a) _
instance Show a => Show (Stmt a) where
   show c = genericShow c

derive instance Eq a => Eq (Clause a)
derive instance Generic (Clause a) _
instance Show a => Show (Clause a) where
   show c = genericShow c

derive instance Eq a => Eq (Clauses a)
derive instance Generic (Clauses a) _
instance Show a => Show (Clauses a) where
   show c = genericShow c

derive instance Eq a => Eq (LambdaClause a)
derive instance Generic (LambdaClause a) _
instance Show a => Show (LambdaClause a) where
   show c = genericShow c

derive instance Eq a => Eq (VarDef a)
derive instance Generic (VarDef a) _
instance Show a => Show (VarDef a) where
   show c = genericShow c

derive instance Eq a => Eq (Qualifier a)
derive instance Generic (Qualifier a) _
instance Show a => Show (Qualifier a) where
   show c = genericShow c

derive instance Eq a => Eq (ParagraphElem a)
derive instance Generic (ParagraphElem a) _
instance Show a => Show (ParagraphElem a) where
   show c = genericShow c

-- ======================
-- Free / bound variables
-- ======================

instance BV Pattern where
   bv (PVar x) = Set.singleton x
   bv (PConstr _ ps) = Set.unions (bv <$> ps)
   bv (PRecord xps) = Set.unions ((bv <<< snd) <$> xps)
   bv PListEmpty = Set.empty
   bv (PListNonEmpty p lr) = bv p ∪ bv lr

instance BV ListRestPattern where
   bv (PListNext p lr) = bv p ∪ bv lr
   bv (PListVar x) = Set.singleton x
   bv PListEnd = Set.empty

instance FV (Expr a) where
   fv (Var x) = Set.singleton x
   fv (Op op) = Set.singleton op
   fv (Int _ _) = Set.empty
   fv (Float _ _) = Set.empty
   fv (Str _ _) = Set.empty
   fv (Constr _ _ es) = Set.unions (fv <$> es)
   fv (Dictionary _ entries) = Set.unions ((\(k × v) -> fv k ∪ fv v) <$> entries)
   fv (Matrix _ body (x × y) source) = (fv body \\ (Set.singleton x ∪ Set.singleton y)) ∪ fv source
   fv (Lambda lc) = fv lc
   fv (Project e _) = fv e
   fv (DProject e e') = fv e ∪ fv e'
   fv (App e e') = fv e ∪ fv e'
   fv (BinaryApp e op e') = fv e ∪ Set.singleton op ∪ fv e'
   fv (UnaryPrefixApp op e) = Set.singleton op ∪ fv e
   fv (Ternary cond e1 e2) = fv cond ∪ fv e1 ∪ fv e2
   fv (Paragraph elems) = Set.unions (fv <$> elems)
   fv (ListEmpty _) = Set.empty
   fv (ListNonEmpty _ e l) = fv e ∪ fv l
   fv (ListEnum e1 e2) = fv e1 ∪ fv e2
   fv (ListComp _ e quals) = qualsFv quals e
   fv (DocExpr e e') = fv e ∪ fv e'

instance FV (Stmt a) where
   fv (Return e) = fv e
   fv (If clauses elseBody) =
      Set.unions ((\(c × b) -> fv c ∪ fv b) <$> clauses) ∪ fv elseBody
   fv (Match scrut branches) =
      fv scrut ∪ Set.unions ((\(p × b) -> fv b \\ bv p) <$> branches)
   fv (Def vd) = fv vd
   fv (DefRec rs) = fvRecDefs rs
   fv Pass = Set.empty
   fv (ExprStmt e) = fv e
   fv (Assert cond msg) = fv cond ∪ maybe Set.empty fv msg
   fv (Seq s1 s2) = fv s1 ∪ fv s2

instance FV (VarDef a) where
   fv (VarDef _ e) = fv e

instance FV (LambdaClause a) where
   fv (LambdaClause (ps × e)) = fv e \\ Set.unions (bv <$> ps)

instance FV (Clause a) where
   fv (Clause _ (ps × b)) = fv b \\ Set.unions (bv <$> ps)

instance FV (DictEntry a) where
   fv (ExprKey e) = fv e
   fv (VarKey _ _) = Set.empty

instance FV (ListRest a) where
   fv (End _) = Set.empty
   fv (Next _ e l) = fv e ∪ fv l

instance FV (ParagraphElem a) where
   fv (Token _) = Set.empty
   fv (Unquote e) = fv e

fvRecDefs :: forall a. RecDefs a -> Set.Set Var
fvRecDefs rs =
   Set.unions (fv <$> (snd <$> rs)) \\ Set.unions (Set.singleton <<< fst <$> rs)

-- List-comprehension qualifiers bind their variables for subsequent qualifiers
-- (and the producing expression). Process right-to-left.
qualsFv :: forall a. List (Qualifier a) -> Expr a -> Set.Set Var
qualsFv Nil e = fv e
qualsFv (q : qs) e = case q of
   ListCompGuard cond -> fv cond ∪ qualsFv qs e
   ListCompGen p src -> fv src ∪ (qualsFv qs e \\ bv p)
   ListCompDecl (VarDef p src) -> fv src ∪ (qualsFv qs e \\ bv p)
