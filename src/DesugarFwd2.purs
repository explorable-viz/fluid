module DesugarFwd2 where

import Prelude hiding (absurd, otherwise)

import Bindings (Bind, (↦), keys, varAnon)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Function (applyN, on)
import Data.List (List(..), (:), (\\), length, sortBy)
import Data.List (singleton) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList)
import Data.NonEmpty ((:|))
import Data.Set (toUnfoldable) as S
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, uncurry)
import DataType (Ctr, arity, checkArity, ctrs, cCons, cFalse, cNil, cTrue, dataTypeFor)
import Dict (Dict, asSingletonMap)
import Dict (fromFoldable, singleton) as D
import Expr2 (Cont(..), Elim(..), asElim, thunkSugar)
import Expr2 (Expr(..), Module(..), RecDefs, VarDef(..)) as E
import Lattice2 (class JoinSemilattice, maybeJoin)
import SExpr2 (Branch, Clause, ListRest(..), ListRestPattern(..), Module(..), Pattern(..), Qualifier(..), RecDefs, SExpr(..), VarDef(..), VarDefs)
import Util (type (+), type (×), MayFail, absurd, error, successful, (×))


desugarFwd :: forall a. JoinSemilattice a => SExpr a -> MayFail (E.Expr a)
desugarFwd = exprFwd

desugarModuleFwd :: forall a. JoinSemilattice a => Module a -> MayFail (E.Module a)
desugarModuleFwd = moduleFwd

enil :: forall a. a -> E.Expr a
enil α = E.Constr α cNil Nil

econs :: forall a. a -> E.Expr a -> E.Expr a -> E.Expr a
econs α e e' = E.Constr α cCons (e : e' : Nil)

elimBool :: forall a. Cont a -> Cont a -> Elim a
elimBool κ κ' = ElimConstr (D.fromFoldable [ cTrue × κ, cFalse × κ' ])

-- Surface language supports "blocks" of variable declarations; core does not.
moduleFwd :: forall a. JoinSemilattice a => Module a -> MayFail (E.Module a)
moduleFwd (Module ds) = 
   E.Module <$> traverse varDefOrRecDefsFwd (join (desugarDefs <$> ds))
   where
   varDefOrRecDefsFwd :: VarDef a + RecDefs a -> MayFail (E.VarDef a + E.RecDefs a)
   varDefOrRecDefsFwd (Left d) = Left <$> varDefFwd d
   varDefOrRecDefsFwd (Right xcs) = Right <$> recDefsFwd xcs

   desugarDefs :: VarDefs a + RecDefs a -> List (VarDef a + RecDefs a)
   desugarDefs (Left ds') = Left <$> toList ds'
   desugarDefs (Right δ) = pure (Right δ)

varDefFwd :: forall a. JoinSemilattice a => VarDef a -> MayFail (E.VarDef a)
varDefFwd (VarDef π s) = E.VarDef <$> patternFwd π (ContNone :: Cont a) <*> pure s

varDefsFwd :: forall a. JoinSemilattice a => VarDefs a × E.Expr a -> MayFail (E.Expr a)
varDefsFwd (NonEmptyList (d :| Nil) × s) =
   E.Let <$> varDefFwd d <*> pure s
varDefsFwd (NonEmptyList (d :| d' : ds) × s) =
   E.Let <$> varDefFwd d <*> varDefsFwd (NonEmptyList (d' :| ds) × s)

-- In the formalism, "group by name" is part of the syntax.
-- cs desugar_fwd σ
recDefsFwd :: forall a. JoinSemilattice a => RecDefs a -> MayFail (E.RecDefs a)
recDefsFwd xcs = D.fromFoldable <$> traverse recDefFwd xcss
   where
   xcss = groupBy (eq `on` fst) xcs :: NonEmptyList (NonEmptyList (Clause a))

recDefFwd :: forall a. JoinSemilattice a => NonEmptyList (Clause a) -> MayFail (Bind (Elim a))
recDefFwd xcs = (fst (head xcs) ↦ _) <$> branchesFwd_curried (snd <$> xcs)

-- s desugar_fwd e
exprFwd :: forall a. JoinSemilattice a => SExpr a -> MayFail (E.Expr a)
exprFwd (BinaryApp s1 op s2) = pure (E.App (E.App (E.Op op) s1) s2)
exprFwd (MatchAs s bs) = E.App <$> (E.Lambda <$> branchesFwd_uncurried bs) <*> pure s
exprFwd (IfElse s1 s2 s3) = 
   E.App (E.Lambda (elimBool (ContExpr s2) (ContExpr s3))) <$> pure s1
exprFwd (ListEmpty α) = pure (enil α)
exprFwd (ListNonEmpty α s l) = pure (econs α s (thunkSugar l))
exprFwd (ListEnum s1 s2) = E.App <$> ((E.App (E.Var "enumFromTo")) <$> pure s1) <*> pure s2
-- | List-comp-done
exprFwd (ListComp _ s_body (NonEmptyList (Guard (E.Constr α2 c Nil) :| Nil))) | c == cTrue =
   pure (econs α2 s_body (enil α2))
-- | List-comp-last
exprFwd (ListComp α s_body (NonEmptyList (q :| Nil))) =
   exprFwd (ListComp α s_body (NonEmptyList (q :| Guard (E.Constr α cTrue Nil) : Nil)))
-- | List-comp-guard
exprFwd (ListComp α s_body (NonEmptyList (Guard s :| q : qs))) = do
   let e = thunkSugar (ListComp α s_body (NonEmptyList (q :| qs)))
   E.App (E.Lambda (elimBool (ContExpr e) (ContExpr (enil α)))) <$> pure s
-- | List-comp-decl
exprFwd (ListComp α s_body (NonEmptyList (Declaration (VarDef π s) :| q : qs))) = do
   let e = thunkSugar (ListComp α s_body (NonEmptyList (q :| qs)))
   σ <- patternFwd π (ContExpr e :: Cont a)
   E.App (E.Lambda σ) <$> pure s
-- | List-comp-gen
exprFwd (ListComp α s_body (NonEmptyList (Generator p s :| q : qs))) = do
   let e = thunkSugar (ListComp α s_body (NonEmptyList (q :| qs)))
   σ <- patternFwd p (ContExpr e)
   E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (totaliseFwd (ContElim σ) α)))) <$> pure s
exprFwd (Let ds s) = varDefsFwd (ds × s)
exprFwd (LetRec xcs s) = E.LetRec <$> recDefsFwd xcs <*> pure s

-- l desugar_fwd e
listRestFwd :: forall a. JoinSemilattice a => ListRest a -> MayFail (E.Expr a)
listRestFwd (End α) = pure (enil α)
listRestFwd (Next α s l) = pure (econs α s (thunkSugar l))

-- ps, e desugar_fwd σ
patternsFwd :: forall a. JoinSemilattice a => NonEmptyList Pattern × E.Expr a -> MayFail (Elim a)
patternsFwd (NonEmptyList (p :| Nil) × e) = branchFwd_uncurried p e
patternsFwd (NonEmptyList (p :| p' : ps) × e) =
   patternFwd p =<< ContExpr <$> E.Lambda <$> patternsFwd (NonEmptyList (p' :| ps) × e)

patternFwd :: forall a. Pattern -> Cont a -> MayFail (Elim a)
patternFwd (PVar x) κ = pure (ElimVar x κ)
patternFwd (PConstr c ps) κ =
   checkArity c (length ps) *> (ElimConstr <$> D.singleton c <$> argPatternFwd (Left <$> ps) κ)
patternFwd (PRecord xps) κ = ElimRecord (keys xps) <$> recordPatternFwd (sortBy (flip compare `on` fst) xps) κ
patternFwd PListEmpty κ = pure (ElimConstr (D.singleton cNil κ))
patternFwd (PListNonEmpty p o) κ = ElimConstr <$> D.singleton cCons <$> argPatternFwd (Left p : Right o : Nil) κ

-- o, κ desugar_fwd σ
listRestPatternFwd :: forall a. ListRestPattern -> Cont a -> MayFail (Elim a)
listRestPatternFwd PEnd κ = pure (ElimConstr (D.singleton cNil κ))
listRestPatternFwd (PNext p o) κ = ElimConstr <$> D.singleton cCons <$> argPatternFwd (Left p : Right o : Nil) κ

argPatternFwd :: forall a. List (Pattern + ListRestPattern) -> Cont a -> MayFail (Cont a)
argPatternFwd Nil κ = pure κ
argPatternFwd (Left p : πs) κ = ContElim <$> (argPatternFwd πs κ >>= patternFwd p)
argPatternFwd (Right o : πs) κ = ContElim <$> (argPatternFwd πs κ >>= listRestPatternFwd o)

recordPatternFwd :: forall a. List (Bind Pattern) -> Cont a -> MayFail (Cont a)
recordPatternFwd Nil κ = pure κ
recordPatternFwd (_ ↦ p : xps) κ = patternFwd p κ >>= ContElim >>> recordPatternFwd xps

branchFwd_uncurried :: forall a. JoinSemilattice a => Pattern -> E.Expr a -> MayFail (Elim a)
branchFwd_uncurried p s = let cont = ContExpr s in patternFwd p cont

branchesFwd_curried :: forall a. JoinSemilattice a => NonEmptyList (Branch a) -> MayFail (Elim a)
branchesFwd_curried bs = do
   NonEmptyList (σ :| σs) <- traverse patternsFwd bs
   foldM maybeJoin σ σs

branchesFwd_uncurried :: forall a. JoinSemilattice a => NonEmptyList (Pattern × E.Expr a) -> MayFail (Elim a)
branchesFwd_uncurried bs = do
   NonEmptyList (σ :| σs) <- traverse (uncurry branchFwd_uncurried) bs
   foldM maybeJoin σ σs

totaliseFwd :: forall a. Cont a -> a -> Cont a
totaliseFwd ContNone _ = error absurd
totaliseFwd (ContExpr e) _ = ContExpr e
totaliseFwd (ContElim (ElimConstr m)) α = ContElim (ElimConstr (totaliseConstrFwd (c × totaliseFwd κ α) α))
   where
   c × κ = asSingletonMap m
totaliseFwd (ContElim (ElimRecord xs κ)) α = ContElim (ElimRecord xs (totaliseFwd κ α))
totaliseFwd (ContElim (ElimVar x κ)) α = ContElim (ElimVar x (totaliseFwd κ α))

-- Extend singleton branch to set of branches where any missing constructors have been mapped to the empty list,
-- using anonymous variables in any generated patterns.
totaliseConstrFwd :: forall a. Ctr × Cont a -> a -> Dict (Cont a)
totaliseConstrFwd (c × κ) α =
   let
      defaultBranch c' = c' × applyN (ContElim <<< ElimVar varAnon) (successful (arity c')) (ContExpr (enil α))
      cκs = defaultBranch <$> ((ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ L.singleton c)
   in
      D.fromFoldable ((c × κ) : cκs)
