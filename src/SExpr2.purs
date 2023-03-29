module SExpr2 where

import Prelude hiding (absurd, join)

import Bindings (Var, Bind, varAnon, (↦), keys)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Function (applyN, on)
import Data.List (List(..), (:), (\\), sortBy, length)
import Data.List (singleton) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Set (toUnfoldable) as S
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, uncurry)
import DataType (Ctr, arity, cCons, cFalse, cNil, cTrue, checkArity, ctrs, dataTypeFor)
import Dict (asSingletonMap, Dict)
import Dict as D
import Expr2 (Expr(..), Module(..), VarDef(..), RecDefs) as E
import Expr2 (class Desugarable2, Cont(..), Elim(..), Expr, Sugar'(..), asElim, thunkSugar)
import Lattice2 (class JoinSemilattice, definedJoin, neg, maybeJoin)
import Prelude (join) as P
import Unsafe.Coerce (unsafeCoerce)
import Util (type (×), (×), type (+), absurd, error, unimplemented, successful, MayFail)

scons :: forall a. a -> E.Expr a -> E.Expr a -> E.Expr a
scons ann head tail = E.Constr ann cCons (head : tail : Nil)

snil :: forall a. a -> E.Expr a
snil ann = E.Constr ann cNil Nil

unwrapSugar :: forall s e a. JoinSemilattice a => Desugarable2 s e => Sugar' e a -> s a
unwrapSugar (Sugar' k) = k unsafeCoerce

instance Desugarable2 SExpr Expr where
   desug2 = exprFwd

-- ListRest auxiliaries
instance Desugarable2 ListRest Expr where
   desug2 (End ann) = Right $ E.Constr ann cNil Nil
   desug2 (Next ann head rest) = Right $ scons ann head (E.Sugar (thunkSugar rest))

-- instance Desugarable2 (NonEmptyList Branch) Elim where
--    desug2 = branchesFwd_curried
-- Surface language expressions.
data SExpr a
   = Record a (List (Bind (Expr a)))
   | BinaryApp (Expr a) Var (Expr a)
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
newtype Branch a = Branch (NonEmptyList Pattern × Expr a)
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
derive instance Newtype (Branch a) _
derive instance Functor Branch
derive instance Functor SExpr
derive instance Functor ListRest
derive instance Functor VarDef
derive instance Functor Qualifier

instance Functor Module where
   map f (Module defs) = Module (mapDefs f <$> defs)
      where
      mapDefs :: forall a b. (a -> b) -> VarDefs a + RecDefs a -> VarDefs b + RecDefs b
      mapDefs g (Left ds) = Left $ map g <$> ds
      mapDefs g (Right ds) = Right $ (\(x × Branch (ps × s)) -> x × Branch (ps × (g <$> s))) <$> ds

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
exprFwd (Record α xss) = pure (E.Record α (D.fromFoldable xss))
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
   NonEmptyList (σ :| σs) <- traverse patternsFwd (map unwrap bs)
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
