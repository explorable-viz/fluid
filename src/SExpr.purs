module SExpr where

import Prelude hiding (absurd, top)

import Bindings (Bind, Var, varAnon, (↦), keys)
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl)
import Data.Function (applyN, on)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:), (\\), length, sortBy, zip, zipWith)
import Data.List (singleton) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head, toList, singleton)
import Data.List.NonEmpty (singleton) as NE
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty ((:|))
import Data.Profunctor.Strong (first, (***))
import Data.Set (toUnfoldable) as S
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (uncurry, fst, snd)
import DataType (Ctr, arity, checkArity, ctrs, cCons, cFalse, cNil, cTrue, dataTypeFor)
import Desugarable (class Desugarable, desugBwd, desug)
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

derive instance Eq a => Eq (Expr a)
derive instance generalExpr :: Generic (Expr a) _
instance showExpr :: Show a => Show (Expr a) where
   show c = genericShow c

data ListRest a
   = End a
   | Next a (Expr a) (ListRest a)

derive instance Eq a => Eq (ListRest a)
derive instance genericListRest :: Generic (ListRest a) _
instance showListRest :: Show a => Show (ListRest a) where
   show c = genericShow c

data Pattern
   = PVar Var
   | PConstr Ctr (List Pattern)
   | PRecord (List (Bind Pattern))
   | PListEmpty
   | PListNonEmpty Pattern ListRestPattern

derive instance Eq Pattern
derive instance genericPattern :: Generic Pattern _
instance showPattern :: Show Pattern where
   show c = genericShow c

data ListRestPattern
   = PEnd
   | PNext Pattern ListRestPattern

derive instance Eq ListRestPattern
derive instance genericListRestPattern :: Generic ListRestPattern _
instance showListRestPattern :: Show ListRestPattern where
   show c = genericShow c

newtype Clause a = Clause (NonEmptyList Pattern × Expr a)

derive instance Eq a => Eq (Clause a)
derive instance genericClause :: Generic (Clause a) _
instance showClause :: Show a => Show (Clause a) where
   show c = genericShow c

type Branch a = Var × Clause a

newtype Clauses a = Clauses (NonEmptyList (Clause a))

derive instance Eq a => Eq (Clauses a)
derive instance genericClauses :: Generic (Clauses a) _
instance showClauses :: Show a => Show (Clauses a) where
   show c = genericShow c

newtype RecDef a = RecDef (NonEmptyList (Branch a))
type RecDefs a = NonEmptyList (Branch a)

-- The pattern/expr relationship is different to the one in branch (the expr is the "argument", not the "body").
-- Using a data type makes for easier overloading.
data VarDef a = VarDef Pattern (Expr a)

derive instance Eq a => Eq (VarDef a)
derive instance genericVarDef :: Generic (VarDef a) _
instance showVarDef :: Show a => Show (VarDef a) where
   show c = genericShow c

type VarDefs a = NonEmptyList (VarDef a)

data Qualifier a
   = Guard (Expr a)
   | Generator Pattern (Expr a)
   | Declaration (VarDef a) -- could allow VarDefs instead

derive instance Eq a => Eq (Qualifier a)
derive instance genericQualifier :: Generic (Qualifier a) _
instance showQualifier :: Show a => Show (Qualifier a) where
   show c = genericShow c

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
varDefFwd (VarDef π s) = E.VarDef <$> pattContFwd π (ContNone :: Cont a) <*> desug s

-- VarDefs
varDefsFwd :: forall a. JoinSemilattice a => VarDefs a × Expr a -> MayFail (E.Expr a)
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
exprFwd (Constr α c ss) = E.Constr α c <$> traverse desug ss
exprFwd (Record α xss) = E.Record α <$> D.fromFoldable <$> traverse (traverse desug) xss
exprFwd (Dictionary α sss) = E.Dictionary α <$> traverse (traverse desug) sss
exprFwd (Matrix α s (x × y) s') = E.Matrix α <$> desug s <@> x × y <*> desug s'
exprFwd (Lambda bs) = E.Lambda <$> clausesFwd bs
exprFwd (Project s x) = E.Project <$> desug s <@> x
exprFwd (App s1 s2) = E.App <$> desug s1 <*> desug s2
exprFwd (BinaryApp s1 op s2) = E.App <$> (E.App (E.Op op) <$> desug s1) <*> desug s2
exprFwd (MatchAs s bs) =
   E.App <$> (E.Lambda <$> clausesFwd (Clauses (Clause <$> first singleton <$> bs))) <*> desug s
exprFwd (IfElse s1 s2 s3) =
   E.App <$> (E.Lambda <$> (elimBool <$> (ContExpr <$> desug s2) <*> (ContExpr <$> desug s3))) <*> desug s1
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
exprBwd (E.Lambda σ) (Lambda bs) = Lambda (clausesBwd σ bs)
exprBwd (E.Project e _) (Project s x) = Project (desugBwd e s) x
exprBwd (E.App e1 e2) (App s1 s2) = App (desugBwd e1 s1) (desugBwd e2 s2)
exprBwd (E.App (E.App (E.Op _) e1) e2) (BinaryApp s1 op s2) =
   BinaryApp (desugBwd e1 s1) op (desugBwd e2 s2)
exprBwd (E.App (E.Lambda σ) e) (MatchAs s bs) =
   MatchAs (desugBwd e s)
      (first head <$> unwrap <$> unwrap (clausesBwd σ (Clauses (Clause <$> first NE.singleton <$> bs))))
exprBwd (E.App (E.Lambda (ElimConstr m)) e1) (IfElse s1 s2 s3) =
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
listRestFwd :: forall a. JoinSemilattice a => ListRest a -> MayFail (E.Expr a)
listRestFwd (End α) = pure (enil α)
listRestFwd (Next α s l) = econs α <$> desug s <*> desug l

listRestBwd :: forall a. BoundedJoinSemilattice a => E.Expr a -> Raw ListRest -> ListRest a
listRestBwd (E.Constr α _ _) (End _) = End α
listRestBwd (E.Constr α _ (e1 : e2 : Nil)) (Next _ s l) =
   Next α (desugBwd e1 s) (desugBwd e2 l)
listRestBwd _ _ = error absurd

-- List Qualifier × Expr
listCompFwd :: forall a. JoinSemilattice a => a × List (Qualifier a) × Expr a -> MayFail (E.Expr a)
listCompFwd (α × Nil × s) =
   econs α <$> desug s <@> enil α
listCompFwd (α × (Guard s : qs) × s') = do
   e <- listCompFwd (α × qs × s')
   E.App (E.Lambda (elimBool (ContExpr e) (ContExpr (enil α)))) <$> desug s
listCompFwd (α × (Declaration (VarDef π s) : qs) × s') = do
   e <- ContExpr <$> listCompFwd (α × qs × s')
   σ <- pattContFwd π e
   E.App (E.Lambda σ) <$> desug s
listCompFwd (α × (Generator p s : qs) × s') = do
   e <- ContExpr <$> listCompFwd (α × qs × s')
   σ <- pattContFwd p e
   E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (orElseFwd (ContElim σ) α)))) <$> desug s

listCompBwd
   :: forall a
    . BoundedJoinSemilattice a
   => E.Expr a
   -> List (Raw Qualifier) × Raw Expr
   -> a × List (Qualifier a) × Expr a
listCompBwd (E.Constr α2 c (e : E.Constr α1 c' Nil : Nil)) (Nil × s) | c == cCons && c' == cNil =
   (α1 ∨ α2) × Nil × desugBwd e s
listCompBwd (E.App (E.Lambda (ElimConstr m)) e) ((Guard s0 : qs) × s) =
   case listCompBwd (asExpr (get cTrue m)) (qs × s) × asExpr (get cFalse m) of
      (α × qs' × s') × E.Constr β c Nil | c == cNil -> (α ∨ β) × (Guard (desugBwd e s0) : qs') × s'
      _ -> error absurd
listCompBwd (E.App (E.Lambda σ) e) ((Declaration (VarDef π s0) : qs) × s) =
   case listCompBwd (asExpr (pattContBwd π σ)) (qs × s) of
      α × qs' × s' -> α × (Declaration (VarDef π (desugBwd e s0)) : qs') × s'
listCompBwd (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e) ((Generator p s0 : qs) × s) =
   case orElseBwd (ContElim σ) (Left p : Nil) of
      σ' × β -> case listCompBwd (asExpr (pattContBwd p (asElim σ'))) (qs × s) of
         α × qs' × s' -> (α ∨ β) × (Generator p (desugBwd e s0) : qs') × s'
listCompBwd _ _ = error absurd

-- NonEmptyList Pattern × Expr
pattsExprFwd :: forall a. JoinSemilattice a => NonEmptyList Pattern × Expr a -> MayFail (Elim a)
pattsExprFwd (NonEmptyList (p :| Nil) × s) = (ContExpr <$> desug s) >>= pattContFwd p
pattsExprFwd (NonEmptyList (p :| p' : ps) × s) =
   pattContFwd p =<< ContExpr <$> E.Lambda <$> pattsExprFwd (NonEmptyList (p' :| ps) × s)

pattsExprBwd :: forall a. BoundedJoinSemilattice a => NonEmptyList Pattern × Raw Expr -> Elim a -> Expr a
pattsExprBwd (NonEmptyList (p :| Nil) × s) σ = desugBwd (asExpr (pattContBwd p σ)) s
pattsExprBwd (NonEmptyList (p :| p' : ps) × s) σ = next (asExpr (pattContBwd p σ))
   where
   next (E.Lambda τ) = pattsExprBwd (NonEmptyList (p' :| ps) × s) τ
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
   clauseBwd (Clause (πs × s)) = Clause (πs × pattsExprBwd (πs × s) σ)

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

