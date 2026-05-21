module SExpr where

import Prelude hiding (absurd, top, unless)

import Bind (Bind, Var, varAnon, (↦))
import Bind (keys) as B
import Control.Monad.Error.Class (class MonadError)
import Data.Bitraversable (rtraverse)
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), drop, take, unzip, zip, zipWith, (:), (\\))
import Data.List.NonEmpty (NonEmptyList(..), foldr, groupBy, head, toList)
import Data.Maybe (Maybe(..))
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
import Expr (Cont(..), Elim(..), asElim)
import Expr (Expr(..), Module(..), RecDefs(..), VarDef(..)) as E
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
   | Lambda (Clauses a)
   | Project (Expr a) Var
   | DProject (Expr a) (Expr a)
   | App (Expr a) (Expr a)
   | BinaryApp (Expr a) Var (Expr a)
   | UnaryPrefixApp Var (Expr a)
   | MatchAs (Expr a) (NonEmptyList (Pattern × Block a))
   | IfElse (NonEmptyList (Expr a × Block a)) (Block a)
   | Paragraph (Paragraph a)
   | ListEmpty a
   | ListNonEmpty a (Expr a) (ListRest a)
   | ListEnum (Expr a) (Expr a)
   | ListComp a (Expr a) (List (Qualifier a))
   | Let (VarDefs a) (Expr a)
   | LetRec (RecDefs a) (Expr a)
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

data Stmt a = Return (Expr a)
newtype Block a = Block (NonEmptyList (Stmt a))

returns :: forall a. Expr a -> Block a
returns e = Block (NonEmptyList (Return e :| Nil))

newtype Clause a = Clause (NonEmptyList Pattern × Block a)

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

instance Desugarable DictEntry E.Expr where
   desug (ExprKey e) = desug e
   desug (VarKey α v) = pure (E.Str α v)

instance Desugarable Expr E.Expr where
   desug = exprFwd

instance Desugarable ListRest E.Expr where
   desug :: forall a m. MonadError Error m => BoundedLattice a => ListRest a -> m (E.Expr a)
   desug (End α) = pure (enil α)
   desug (Next α s l) = econs α <$> desug s <*> desug l

instance Desugarable Clauses Elim where
   desug :: forall a m. BoundedLattice a => MonadError Error m => Clauses a -> m (Elim a)
   desug μ = clausesStateFwd (toClausesStateFwd μ) <#> asElim

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
   E.VarDef <$> desug (Clauses (singleton (Clause (singleton p × returns (Dictionary top Nil))))) <*> desug s

-- VarDefs
varDefsFwd :: forall a m. MonadError Error m => BoundedLattice a => VarDefs a × Expr a -> m (E.Expr a)
varDefsFwd (NonEmptyList (d :| Nil) × s) =
   E.Let <$> varDefFwd d <*> desug s
varDefsFwd (NonEmptyList (d :| d' : ds) × s) =
   E.Let <$> varDefFwd d <*> varDefsFwd (NonEmptyList (d' :| ds) × s)

-- RecDefs
-- In the formalism, "group by name" is part of the syntax.
recDefsFwd :: forall a m. MonadError Error m => BoundedLattice a => RecDefs a -> m (E.RecDefs a)
recDefsFwd xcs = E.RecDefs top <$> D.fromFoldable <$> traverse recDefFwd xcss
   where
   xcss = map RecDef (groupBy (eq `on` fst) xcs) :: NonEmptyList (RecDef a)

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
exprFwd (MatchAs s μ) =
   E.App <$> (E.Lambda top <$> desug (Clauses (Clause <$> first singleton <$> μ))) <*> desug s
exprFwd (IfElse sss s) =
   ifElseFwd (sss × s)
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
exprFwd (Let ds s) =
   varDefsFwd (ds × s)
exprFwd (LetRec xcs s) =
   E.LetRec <$> recDefsFwd xcs <*> desug s
exprFwd (DocExpr s s') = do
   e <- exprFwd s
   e' <- exprFwd s'
   pure $ E.DocExpr e e'

type IfElseClauses a = NonEmptyList (Expr a × Block a) × Block a

blockFwd :: forall a m. BoundedLattice a => MonadError Error m => Block a -> m (E.Expr a)
blockFwd (Block (NonEmptyList (Return e :| Nil))) = desug e
blockFwd _ = error "blockFwd: non-singleton block"

ifElseFwd :: forall a m. BoundedLattice a => MonadError Error m => IfElseClauses a -> m (E.Expr a)
ifElseFwd (sss × s) =
   foldr clause (blockFwd s) sss
   where
   clause (s1 × b) e3 =
      E.App
         <$> (E.Lambda top <$> (elimBool <$> (ContExpr <$> blockFwd b) <*> (ContExpr <$> e3)))
         <*> desug s1

-- List Qualifier × Expr
listCompFwd :: forall a m. MonadError Error m => BoundedLattice a => a × List (Qualifier a) × Expr a -> m (E.Expr a)
listCompFwd (α × Nil × s) =
   econs α <$> desug s <@> enil α
listCompFwd (α × (ListCompGuard s : qs) × s') = do
   e <- listCompFwd (α × qs × s')
   E.App (E.Lambda α (elimBool (ContExpr e) (ContExpr (enil α)))) <$> desug s
listCompFwd (α × (ListCompDecl (VarDef p s) : qs) × s') = do
   σ <- clausesStateFwd (((Left p : Nil) × Nil × returns (ListComp α s' qs)) : Nil)
   E.App (E.Lambda α (asElim σ)) <$> desug s
listCompFwd (α × (ListCompGen p s : qs) × s') = do
   let ks = orElseFwd α ((Left p : Nil) × returns (ListComp α s' qs))
   σ <- clausesStateFwd (toList (ks <#> second (Nil × _)))
   E.App (E.App (E.Var "concat_map") (E.Lambda α (asElim σ))) <$> desug s

-- Clauses
toClausesStateFwd :: forall a. Clauses a -> ClausesState' a
toClausesStateFwd (Clauses μ) = toList μ <#> toClauseStateFwd
   where
   toClauseStateFwd :: Clause a -> ClauseState' a
   toClauseStateFwd (Clause (NonEmptyList (p :| π) × b)) = (Left p : Nil) × π × b

-- Like ClauseState but for curried functions; extra component π' stores remaining top-level patterns.
type ClauseState' a = List (Pattern + ListRestPattern) × List Pattern × Block a
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
      ContExpr <$> blockFwd b
   (Nil × _) : _ ->
      ContExpr <$> E.Lambda top <$> asElim <$> (clausesStateFwd =<< popArgFwd ks)
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
type ClauseState a = List (Pattern + ListRestPattern) × Block a

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
   (p : π) × s ->
      (orElseFwd α ((π' <> π) × s) <#> popPatts (length π') <#> pushPattFor p)
         `appendList`
            (unless p <#> \p' -> ((π <#> anon) × returns (ListEmpty α)) # pushPatt p')
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
derive instance Newtype (Block a) _
derive instance Newtype (Clause a) _
derive instance Newtype (Clauses a) _
derive instance Newtype (RecDef a) _
derive instance Functor Stmt
derive instance Functor Block
derive instance Functor Clause
derive instance Functor Clauses
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
      mapDefs g (Right ds) = Right $ (\(x × Clause (π × b)) -> x × Clause (π × (g <$> b))) <$> ds

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

derive instance Eq a => Eq (Block a)
derive instance Generic (Block a) _
instance Show a => Show (Block a) where
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

derive instance Eq a => Eq (ParagraphElem a)
derive instance Generic (ParagraphElem a) _
instance Show a => Show (ParagraphElem a) where
   show c = genericShow c
