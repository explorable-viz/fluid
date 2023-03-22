module SExpr2 where

import Prelude hiding (absurd, join)
import Prelude (join) as P
import Bindings (Var, Bind, varAnon, (↦), keys)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldM)
import Data.Function (applyN, on)
import Data.List (List(..), (:), (\\), sortBy, length)
import Data.List (singleton) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList)
import Data.NonEmpty ((:|))
import Data.Set (toUnfoldable) as S
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, uncurry)
import DataType (Ctr, arity, cCons, cFalse, cNil, cTrue, checkArity, ctrs, dataTypeFor)
import Dict (asSingletonMap, Dict)
import Dict as D
import Expr2 (class Desugarable, class Desugarable2, Cont(..), Elim(..), Expr, asElim, desug, thunkSugar)
import Expr2 (Expr(..), RecDefs, VarDef(..), Module(..)) as E
import Lattice2 (class JoinSemilattice, definedJoin, join, neg, maybeJoin)
import Util (type (×), (×), type (+), absurd, error, unimplemented, successful, MayFail)

scons :: forall a. a -> E.Expr a -> E.Expr a -> E.Expr a
scons ann head tail = E.Constr ann cCons (head : tail : Nil)

snil :: forall a. a -> E.Expr a
snil ann = E.Constr ann cNil Nil

instance Desugarable2 SExpr where
   desug2 = exprFwd

instance Desugarable SExpr where
   -- Correct
   desug (BinaryApp l op r) = E.App (E.App (E.Op op) l) r
   -- Correct dependent on "clauses" which is meant to be equivalent to branchesFwd2_uncurried
   desug (MatchAs guard patterns) = E.App (E.Lambda (branchesFwd2_uncurried patterns)) guard
   -- Correct
   desug (IfElse guard trueP falseP) = E.App (E.Lambda (elimBool (ContExpr trueP) (ContExpr falseP))) guard
   -- Correct
   desug (ListEmpty ann) = E.Constr ann cNil Nil

   -- Needs to be checked but either this is correct or, we need thunkSugar in parsing
   desug (ListNonEmpty ann head rest) = scons ann head (E.Sugar (thunkSugar rest))

   -- Correct
   desug (ListEnum head last) = E.App (E.App (E.Var "enumFromTo") head) last
   desug (ListComp _ body (NonEmptyList (Guard (E.Constr ann2 c Nil) :| Nil))) | c == cTrue =
      scons ann2 body (snil ann2) -- Should be correct
   -- Need to check this one
   desug (ListComp ann body (NonEmptyList (q :| Nil))) =
      desug (ListComp ann body (NonEmptyList (q :| Guard (E.Constr ann cTrue Nil) : Nil))) -- may need to be thunkSugar
   -- Need to check
   desug (ListComp ann body (NonEmptyList (Guard s :| q : qs))) =
      let
         e = E.Sugar $ thunkSugar (ListComp ann body (NonEmptyList (q :| qs)))
      in
         E.App (E.Lambda (elimBool (ContExpr e) (ContExpr (snil ann)))) s
   -- Need to check the thunkSugar's here
   desug (ListComp ann body (NonEmptyList (Declaration (VarDef pi s) :| q : qs))) =
      let
         e = E.Sugar $ thunkSugar (ListComp ann body (NonEmptyList (q :| qs)))
         sig = patternFwd2 pi (ContExpr e :: Cont _)
      in
         E.App (E.Lambda sig) s
   -- Need to check thunkSugars
   desug (ListComp ann body (NonEmptyList (Generator p s :| q : qs))) =
      let
         e = E.Sugar $ thunkSugar (ListComp ann body (NonEmptyList (q :| qs)))
         sig = patternFwd2 p (ContExpr e)
      in
         E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (totalCont (ContElim sig) ann)))) s
   -- these 2 are correct if their auxiliaries are correct
   desug (Let defs exp) = processVarDefs (defs × exp)
   desug (LetRec recdefs exp) = E.LetRec (processRecDefs recdefs) exp

-- ListRest auxiliaries
instance Desugarable2 ListRest where
   desug2 (End ann) = Right $ E.Constr ann cNil Nil
   desug2 (Next ann head rest) = Right $ scons ann head (E.Sugar (thunkSugar rest))

-- vardefsFwd equivalent
processVarDefs :: forall a. JoinSemilattice a => VarDefs a × E.Expr a -> E.Expr a
processVarDefs (NonEmptyList (d :| Nil) × exp) = E.Let (processVarDef d) exp
processVarDefs (NonEmptyList (d :| d' : ds) × exp) =
   E.Let (processVarDef d) (processVarDefs (NonEmptyList (d' :| ds) × exp))

--vardefFwd equivalent
processVarDef :: forall a. JoinSemilattice a => VarDef a -> E.VarDef a
processVarDef (VarDef pat exp) = E.VarDef (patternFwd2 pat (ContNone :: Cont a)) exp

-- recdefsFwd equivalent
processRecDefs :: forall a. JoinSemilattice a => RecDefs a -> E.RecDefs a
processRecDefs cls = D.fromFoldable $ map processRecDef clss
   where
   clss = groupBy (eq `on` fst) cls :: NonEmptyList (NonEmptyList (Clause a))

-- recdefFwd equivalent
processRecDef :: forall a. JoinSemilattice a => NonEmptyList (Clause a) -> Bind (Elim a)
processRecDef x =
   let
      pairer = (fst (head x) ↦ _) :: forall b. b -> Bind b
      cls = branchesFwd2_curried (map snd x) :: Elim a
   in
      pairer cls

-- clause functions equivalent to branches
branchFwd2 :: forall a. JoinSemilattice a => Pattern × Expr a -> Elim a
branchFwd2 (pat × exp) = let cont = ContExpr exp in patternFwd2 pat cont

branchesFwd2_curried :: forall a. JoinSemilattice a => NonEmptyList (Branch a) -> Elim a
branchesFwd2_curried cls =
   let
      NonEmptyList (head :| rest) = map patternsFwd2 cls
   in
      foldl join head rest

branchesFwd2_uncurried :: forall a. JoinSemilattice a => NonEmptyList (Pattern × Expr a) -> Elim a
branchesFwd2_uncurried cls =
   let
      NonEmptyList (head :| rest) = map branchFwd2 cls
   in
      foldl join head rest

-- these are equivalent to patternsFwd2 etc
patternFwd2 :: forall a. Pattern -> Cont a -> Elim a
patternFwd2 (PVar x) k = ElimVar x k
patternFwd2 (PConstr c ps) k =
   ElimConstr ((D.singleton c) (argPat (map Left ps) k))
patternFwd2 (PRecord bps) k = ElimRecord (keys bps) (recordPat (sortBy (flip compare `on` fst) bps) k)
patternFwd2 PListEmpty k = ElimConstr (D.singleton cNil k)
patternFwd2 (PListNonEmpty p lrp) k = ElimConstr (D.singleton cCons (argPat (Left p : Right lrp : Nil) k))

patternsFwd2 :: forall a. JoinSemilattice a => NonEmptyList Pattern × Expr a -> Elim a
patternsFwd2 (NonEmptyList (p :| Nil) × exp) = branchFwd2 (p × exp)
patternsFwd2 (NonEmptyList (p :| p' : ps) × exp) =
   patternFwd2 p (ContExpr (E.Lambda (patternsFwd2 (NonEmptyList (p' :| ps) × exp))))

argPat :: forall a. List (Pattern + ListRestPattern) -> Cont a -> Cont a
argPat Nil k = k
argPat (Left p : pis) k = let apf = argPat pis k in ContElim (patternFwd2 p apf)
argPat (Right o : pis) k = let apf = argPat pis k in ContElim (listRestPat o apf)

listRestPat :: forall a. ListRestPattern -> Cont a -> Elim a
listRestPat PEnd k = ElimConstr (D.singleton cNil k)
listRestPat (PNext p o) k = ElimConstr (D.singleton cCons (argPat (Left p : Right o : Nil) k))

recordPat :: forall a. List (Bind Pattern) -> Cont a -> Cont a
recordPat Nil k = k
recordPat (_ ↦ p : _) k = ContElim (patternFwd2 p k)

-- Totalize equivalents
totalCont :: forall a. Cont a -> a -> Cont a
totalCont ContNone _ = error absurd
totalCont (ContExpr e) _ = ContExpr e
totalCont (ContElim (ElimConstr m)) ann = ContElim (ElimConstr (totalizeCtr (c × totalCont k ann) ann))
   where
   c × k = asSingletonMap m
totalCont (ContElim (ElimRecord xs k)) ann = ContElim (ElimRecord xs (totalCont k ann))
totalCont (ContElim (ElimVar x k)) ann = ContElim (ElimVar x (totalCont k ann))

totalizeCtr :: forall a. Ctr × Cont a -> a -> D.Dict (Cont a)
totalizeCtr (c × k) ann =
   let
      defaultBranch c' = c' × applyN (ContElim <<< ElimVar varAnon) (successful (arity c')) (ContExpr (snil ann))
      cks = map defaultBranch ((ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ L.singleton c)
   in
      D.fromFoldable ((c × k) : cks)

-- Surface language expressions.
data SExpr a
   = BinaryApp (Expr a) Var (Expr a)
   | MatchAs (Expr a) (NonEmptyList (Pattern × Expr a))
   | IfElse (Expr a) (Expr a) (Expr a)
   | ListEmpty a -- called [] in the paper
   | ListNonEmpty a (Expr a) (ListRest a)
   | ListEnum (Expr a) (Expr a)
   | ListComp a (Expr a) (NonEmptyList (Qualifier a))
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

-- in the spec, "clause" doesn't include the function name
type Branch a = NonEmptyList Pattern × Expr a
type Clause a = Var × Branch a
type RecDefs a = NonEmptyList (Clause a)

-- The pattern/expr relationship is different to the one in branch (the expr is the "argument", not the "body").
-- Using a data type makes for easier overloading.
data VarDef a = VarDef Pattern (Expr a)
type VarDefs a = NonEmptyList (VarDef a)

data Qualifier a
   = Guard (Expr a)
   | Generator Pattern (Expr a)
   | Declaration (VarDef a) -- could allow VarDefs instead

data Module a = Module (List (VarDefs a + RecDefs a))

-- ======================
-- boilerplate
-- ======================
derive instance Functor SExpr
derive instance Functor ListRest
derive instance Functor VarDef
derive instance Functor Qualifier

instance Functor Module where
   map f (Module defs) = Module (mapDefs f <$> defs)
      where
      mapDefs :: forall a b. (a -> b) -> VarDefs a + RecDefs a -> VarDefs b + RecDefs b
      mapDefs g (Left ds) = Left $ map g <$> ds
      mapDefs g (Right ds) = Right $ (\(x × (ps × s)) -> x × (ps × (g <$> s))) <$> ds

instance JoinSemilattice a => JoinSemilattice (SExpr a) where
   join s = definedJoin s
   maybeJoin _ = error unimplemented
   neg = (<$>) neg

{-
   let term = parse "[1, 2, 3, 4]" = Sugar (S.ListNonEmpty (1) (LR.Next (2) (LR.Next (3) (LR.Next (4) (LR.End)))))
   let term2 = desug term = E.Constr cCons (1 : Sugar (LR.Next 2 (LR.Next 3 (LR.Next 4 LR.End))))
   eval term2 = V.Constr cCons (1 : (eval (Sugar (LR.Next 2 (LR.Next 3 (LR.Next 4 LR.End))))))
-}

enil :: forall a. a -> E.Expr a
enil α = E.Constr α cNil Nil

econs :: forall a. a -> E.Expr a -> E.Expr a -> E.Expr a
econs α e e' = E.Constr α cCons (e : e' : Nil)

elimBool :: forall a. Cont a -> Cont a -> Elim a
elimBool κ κ' = ElimConstr (D.fromFoldable [ cTrue × κ, cFalse × κ' ])

-- Surface language supports "blocks" of variable declarations; core does not.
moduleFwd :: forall a. JoinSemilattice a => Module a -> MayFail (E.Module a)
moduleFwd (Module ds) =
   E.Module <$> traverse varDefOrRecDefsFwd (P.join (desugarDefs <$> ds))
   where
   varDefOrRecDefsFwd :: (VarDef a + RecDefs a) -> MayFail (E.VarDef a + E.RecDefs a)
   varDefOrRecDefsFwd (Left d) = Left <$> varDefFwd d
   varDefOrRecDefsFwd (Right xcs) = Right <$> recDefsFwd xcs

   desugarDefs :: (VarDefs a + RecDefs a) -> List (VarDef a + RecDefs a)
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
exprFwd (ListNonEmpty α s l) = pure (econs α s (E.Sugar (thunkSugar l)))
exprFwd (ListEnum s1 s2) = E.App <$> ((E.App (E.Var "enumFromTo")) <$> pure s1) <*> pure s2
-- | List-comp-done
exprFwd (ListComp _ s_body (NonEmptyList (Guard (E.Constr α2 c Nil) :| Nil))) | c == cTrue =
   pure (econs α2 s_body (enil α2))
-- | List-comp-last
exprFwd (ListComp α s_body (NonEmptyList (q :| Nil))) =
   pure $ E.Sugar (thunkSugar (ListComp α s_body (NonEmptyList (q :| Guard (E.Constr α cTrue Nil) : Nil))))
-- | List-comp-guard
exprFwd (ListComp α s_body (NonEmptyList (Guard s :| q : qs))) = do
   let e = E.Sugar $ thunkSugar (ListComp α s_body (NonEmptyList (q :| qs)))
   E.App (E.Lambda (elimBool (ContExpr e) (ContExpr (enil α)))) <$> pure s
-- | List-comp-decl
exprFwd (ListComp α s_body (NonEmptyList (Declaration (VarDef π s) :| q : qs))) = do
   let e = E.Sugar $ thunkSugar (ListComp α s_body (NonEmptyList (q :| qs)))
   σ <- patternFwd π (ContExpr e :: Cont a)
   E.App (E.Lambda σ) <$> pure s
-- | List-comp-gen
exprFwd (ListComp α s_body (NonEmptyList (Generator p s :| q : qs))) = do
   let e = E.Sugar $ thunkSugar (ListComp α s_body (NonEmptyList (q :| qs)))
   σ <- patternFwd p (ContExpr e)
   E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (totaliseFwd (ContElim σ) α)))) <$> pure s
exprFwd (Let ds s) = varDefsFwd (ds × s)
exprFwd (LetRec xcs s) = E.LetRec <$> recDefsFwd xcs <*> pure s

-- l desugar_fwd e
listRestFwd :: forall a. JoinSemilattice a => ListRest a -> MayFail (E.Expr a)
listRestFwd (End α) = pure (enil α)
listRestFwd (Next α s l) = pure (econs α s (E.Sugar (thunkSugar l)))

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
