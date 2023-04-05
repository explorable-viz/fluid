module DesugarFwd where

import Prelude hiding (absurd, otherwise)

import Bindings (Bind, (↦), keys, varAnon)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Function (applyN, on)
import Data.List (List(..), (:), (\\), length, sortBy)
import Data.List (singleton) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList, singleton)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Profunctor.Strong (first)
import Data.Set (toUnfoldable) as S
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import DataType (Ctr, arity, checkArity, ctrs, cCons, cFalse, cNil, cTrue, dataTypeFor)
import Dict (Dict, asSingletonMap)
import Dict (fromFoldable, singleton) as D
import Expr (Cont(..), Elim(..), asElim)
import Expr (Expr(..), Module(..), RecDefs, VarDef(..)) as E
import Lattice (class JoinSemilattice, maybeJoin)
import SExpr (Clause(..), Expr(..), ListRestPattern(..), ListRest(..), Module(..), Pattern(..), VarDefs, VarDef(..), RecDefs, RecDef(..), Qualifier(..))
import Util (type (+), type (×), MayFail, absurd, error, successful, (×))

desugarFwd :: forall a. JoinSemilattice a => Expr a -> MayFail (E.Expr a)
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
moduleFwd (Module ds) = E.Module <$> traverse varDefOrRecDefsFwd (join (desugarDefs <$> ds))
   where
   varDefOrRecDefsFwd :: VarDef a + RecDefs a -> MayFail (E.VarDef a + E.RecDefs a)
   varDefOrRecDefsFwd (Left d) = Left <$> varDefFwd d
   varDefOrRecDefsFwd (Right xcs) = Right <$> recDefsFwd xcs

   desugarDefs :: VarDefs a + RecDefs a -> List (VarDef a + RecDefs a)
   desugarDefs (Left ds') = Left <$> toList ds'
   desugarDefs (Right δ) = pure (Right δ)

varDefFwd :: forall a. JoinSemilattice a => VarDef a -> MayFail (E.VarDef a)
varDefFwd (VarDef π s) = E.VarDef <$> pattContFwd π (ContNone :: Cont a) <*> exprFwd s

varDefsFwd :: forall a. JoinSemilattice a => VarDefs a × Expr a -> MayFail (E.Expr a)
varDefsFwd (NonEmptyList (d :| Nil) × s) =
   E.Let <$> varDefFwd d <*> exprFwd s
varDefsFwd (NonEmptyList (d :| d' : ds) × s) =
   E.Let <$> varDefFwd d <*> varDefsFwd (NonEmptyList (d' :| ds) × s)

-- In the formalism, "group by name" is part of the syntax.
-- cs desugar_fwd σ
recDefsFwd :: forall a. JoinSemilattice a => RecDefs a -> MayFail (E.RecDefs a)
recDefsFwd xcs = D.fromFoldable <$> traverse recDefFwd xcss
   where
   xcss = map RecDef (groupBy (eq `on` fst) xcs) :: NonEmptyList (RecDef a)

recDefFwd :: forall a. JoinSemilattice a => RecDef a -> MayFail (Bind (Elim a))
recDefFwd xcs = (fst (head (unwrap xcs)) ↦ _) <$> clausesFwd (snd <$> (unwrap xcs))

-- s desugar_fwd e
exprFwd :: forall a. JoinSemilattice a => Expr a -> MayFail (E.Expr a)
exprFwd (Var x) = pure (E.Var x)
exprFwd (Op op) = pure (E.Op op)
exprFwd (Int α n) = pure (E.Int α n)
exprFwd (Float α n) = pure (E.Float α n)
exprFwd (Str α s) = pure (E.Str α s)
exprFwd (Constr α c ss) = E.Constr α c <$> traverse exprFwd ss
exprFwd (Record α xss) = E.Record α <$> D.fromFoldable <$> traverse (traverse exprFwd) xss
exprFwd (Dictionary α sss) = E.Dictionary α <$> traverse (traverse exprFwd) sss
exprFwd (Matrix α s (x × y) s') = E.Matrix α <$> exprFwd s <@> x × y <*> exprFwd s'
exprFwd (Lambda bs) = E.Lambda <$> clausesFwd bs
exprFwd (Project s x) = E.Project <$> exprFwd s <@> x
exprFwd (App s1 s2) = E.App <$> exprFwd s1 <*> exprFwd s2
exprFwd (BinaryApp s1 op s2) = E.App <$> (E.App (E.Op op) <$> exprFwd s1) <*> exprFwd s2
exprFwd (MatchAs s bs) = E.App <$> (E.Lambda <$> (clausesFwd ((map (Clause <$> first singleton) bs)))) <*> exprFwd s -- checked
exprFwd (IfElse s1 s2 s3) = do -- checked
   e2 <- exprFwd s2
   e3 <- exprFwd s3
   E.App (E.Lambda (elimBool (ContExpr e2) (ContExpr e3))) <$> exprFwd s1
exprFwd (ListEmpty α) = pure (enil α) -- checked
exprFwd (ListNonEmpty α s l) = econs α <$> exprFwd s <*> listRestFwd l -- checked, valid if listrestfwd valid
exprFwd (ListEnum s1 s2) = E.App <$> ((E.App (E.Var "enumFromTo")) <$> exprFwd s1) <*> exprFwd s2 -- checked
-- -- | List-comp-done, checked
-- exprFwd (ListComp _ s_body ((Guard (Constr α2 c Nil)) : Nil)) | c == cTrue =
--    econs α2 <$> exprFwd s_body <@> enil α2
-- | List-comp-done 
exprFwd (ListComp α s_body Nil) =
   econs α <$> (exprFwd s_body) <@> enil α
-- -- | List-comp-last ?
-- exprFwd (ListComp α s_body ((q : Nil))) =
--    exprFwd (ListComp α s_body ((q : Guard (Constr α cTrue Nil) : Nil)))

-- | List-comp-guard checked
exprFwd (ListComp α s_body (Guard s : qs)) = do
   e <- exprFwd (ListComp α s_body qs)
   E.App (E.Lambda (elimBool (ContExpr e) (ContExpr (enil α)))) <$> exprFwd s

-- | List-comp-decl checked
exprFwd (ListComp α s_body ((Declaration (VarDef π s) : qs))) = do
   e <- exprFwd (ListComp α s_body qs)
   σ <- pattContFwd π (ContExpr e :: Cont a)
   E.App (E.Lambda σ) <$> exprFwd s

-- | List-comp-gen checked
exprFwd (ListComp α s_body ((Generator p s : qs))) = do
   e <- exprFwd (ListComp α s_body qs) -- effectively the rules premise
   σ <- pattContFwd p (ContExpr e)
   E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (orElse (ContElim σ) α)))) <$> exprFwd s
exprFwd (Let ds s) = varDefsFwd (ds × s)
exprFwd (LetRec xcs s) = E.LetRec <$> recDefsFwd xcs <*> exprFwd s

-- l desugar_fwd e
listRestFwd :: forall a. JoinSemilattice a => ListRest a -> MayFail (E.Expr a)
listRestFwd (End α) = pure (enil α)
listRestFwd (Next α s l) = econs α <$> exprFwd s <*> listRestFwd l

-- ps, e desugar_fwd σ
pattsExprFwd :: forall a. JoinSemilattice a => NonEmptyList Pattern × Expr a -> MayFail (Elim a)
pattsExprFwd (NonEmptyList (p :| Nil) × e) = clauseFwd_uncurried p e
pattsExprFwd (NonEmptyList (p :| p' : ps) × e) =
   pattContFwd p =<< ContExpr <$> E.Lambda <$> pattsExprFwd (NonEmptyList (p' :| ps) × e)

pattContFwd :: forall a. Pattern -> Cont a -> MayFail (Elim a)
pattContFwd (PVar x) κ = pure (ElimVar x κ)
pattContFwd (PConstr c ps) κ =
   checkArity c (length ps) *> (ElimConstr <$> D.singleton c <$> pattCont_arg_Fwd (Left <$> ps) κ)
pattContFwd (PRecord xps) κ = ElimRecord (keys xps) <$> pattCont_record_Fwd (sortBy (flip compare `on` fst) xps) κ
pattContFwd PListEmpty κ = pure (ElimConstr (D.singleton cNil κ))
pattContFwd (PListNonEmpty p o) κ = ElimConstr <$> D.singleton cCons <$> pattCont_arg_Fwd (Left p : Right o : Nil) κ

-- o, κ desugar_fwd σ
pattCont_ListRest_Fwd :: forall a. ListRestPattern -> Cont a -> MayFail (Elim a)
pattCont_ListRest_Fwd PEnd κ = pure (ElimConstr (D.singleton cNil κ))
pattCont_ListRest_Fwd (PNext p o) κ = ElimConstr <$> D.singleton cCons <$> pattCont_arg_Fwd (Left p : Right o : Nil) κ

pattCont_arg_Fwd :: forall a. List (Pattern + ListRestPattern) -> Cont a -> MayFail (Cont a)
pattCont_arg_Fwd Nil κ = pure κ
pattCont_arg_Fwd (Left p : πs) κ = ContElim <$> (pattCont_arg_Fwd πs κ >>= pattContFwd p)
pattCont_arg_Fwd (Right o : πs) κ = ContElim <$> (pattCont_arg_Fwd πs κ >>= pattCont_ListRest_Fwd o)

pattCont_record_Fwd :: forall a. List (Bind Pattern) -> Cont a -> MayFail (Cont a)
pattCont_record_Fwd Nil κ = pure κ
pattCont_record_Fwd (_ ↦ p : xps) κ = pattContFwd p κ >>= ContElim >>> pattCont_record_Fwd xps

-- initial: branchFwd_uncurried, new: clauseFwd_uncurried
clauseFwd_uncurried :: forall a. JoinSemilattice a => Pattern -> Expr a -> MayFail (Elim a)
clauseFwd_uncurried p s = (ContExpr <$> exprFwd s) >>= pattContFwd p

-- initial: branchesFwd_curried, new: clausesFwd
clausesFwd :: forall a. JoinSemilattice a => NonEmptyList (Clause a) -> MayFail (Elim a)
clausesFwd bs = do
   NonEmptyList (σ :| σs) <- traverse pattsExprFwd (map unwrap bs)
   foldM maybeJoin σ σs

-- initial: branchesFwd_uncurried, new: clausesFwd_uncurried
-- clausesFwd_uncurried :: forall a. JoinSemilattice a => NonEmptyList (Pattern × Expr a) -> MayFail (Elim a)
-- clausesFwd_uncurried bs = do
--    NonEmptyList (σ :| σs) <- traverse (uncurry clauseFwd_uncurried) bs
--    foldM maybeJoin σ σs

orElse :: forall a. Cont a -> a -> Cont a
orElse ContNone _ = error absurd
orElse (ContExpr e) _ = ContExpr e
orElse (ContElim (ElimConstr m)) α = ContElim (ElimConstr (unlessFwd (c × orElse κ α) α))
   where
   c × κ = asSingletonMap m
orElse (ContElim (ElimRecord xs κ)) α = ContElim (ElimRecord xs (orElse κ α))
orElse (ContElim (ElimVar x κ)) α = ContElim (ElimVar x (orElse κ α))

-- Extend singleton branch to set of branches where any missing constructors have been mapped to the empty list,
-- using anonymous variables in any generated patterns.
unlessFwd :: forall a. Ctr × Cont a -> a -> Dict (Cont a)
unlessFwd (c × κ) α =
   let
      defaultBranch c' = c' × applyN (ContElim <<< ElimVar varAnon) (successful (arity c')) (ContExpr (enil α)) -- corresponds to elimArgs
      cκs = defaultBranch <$> ((ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ L.singleton c)
   in
      D.fromFoldable ((c × κ) : cκs)
