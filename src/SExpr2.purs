module SExpr2 where

import Prelude hiding (absurd, top)

import Ann (Raw)
import Bindings (Bind, Var, varAnon, (↦), keys)
import BoolAlg (BoolAlg, slices)
import Control.Monad.Error.Class (class MonadError)
import Data.Either (Either(..))
import Data.Foldable (foldl)
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
import Desugarable2 (class Desugarable, desugBwd, desug)
import Dict (Dict, asSingletonMap, get)
import Dict (fromFoldable, singleton) as D
import Effect.Exception (Error)
import Expr (Cont(..), Elim(..), asElim, asExpr)
import Expr (Expr(..), Module(..), RecDefs, VarDef(..)) as E
import Partial.Unsafe (unsafePartial)
import Util (type (+), type (×), Endo, absurd, error, successful, (×))
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

desugarModuleFwd :: forall a m. MonadError Error m => BoolAlg a -> Module a -> m (E.Module a)
desugarModuleFwd = moduleFwd

-- helpers
enil :: forall a. a -> E.Expr a
enil α = E.Constr α cNil Nil

econs :: forall a. a -> E.Expr a -> E.Expr a -> E.Expr a
econs α e e' = E.Constr α cCons (e : e' : Nil)

elimBool :: forall a. Cont a -> Cont a -> Elim a
elimBool κ κ' = ElimConstr (D.fromFoldable [ cTrue × κ, cFalse × κ' ])

-- Module. Surface language supports "blocks" of variable declarations; core does not. Currently no backward.
moduleFwd :: forall a m. MonadError Error m => BoolAlg a -> Module a -> m (E.Module a)
moduleFwd 𝒶 (Module ds) = E.Module <$> traverse varDefOrRecDefsFwd (join (flatten <$> ds))
   where
   varDefOrRecDefsFwd :: VarDef a + RecDefs a -> m (E.VarDef a + E.RecDefs a)
   varDefOrRecDefsFwd (Left d) = Left <$> varDefFwd 𝒶 d
   varDefOrRecDefsFwd (Right xcs) = Right <$> recDefsFwd 𝒶 xcs

   flatten :: VarDefs a + RecDefs a -> List (VarDef a + RecDefs a)
   flatten (Left ds') = Left <$> toList ds'
   flatten (Right δ) = pure (Right δ)

varDefFwd :: forall a m. MonadError Error m => BoolAlg a -> VarDef a -> m (E.VarDef a)
varDefFwd 𝒶 (VarDef π s) = E.VarDef <$> pattContFwd π (ContNone :: Cont a) <*> desug 𝒶 s

-- VarDefs
varDefsFwd :: forall a m. MonadError Error m => BoolAlg a -> VarDefs a × Expr a -> m (E.Expr a)
varDefsFwd 𝒶 (NonEmptyList (d :| Nil) × s) =
   E.Let <$> varDefFwd 𝒶 d <*> desug 𝒶 s
varDefsFwd 𝒶 (NonEmptyList (d :| d' : ds) × s) =
   E.Let <$> varDefFwd 𝒶 d <*> varDefsFwd 𝒶 (NonEmptyList (d' :| ds) × s)

varDefsBwd :: forall a. BoolAlg a -> E.Expr a -> Raw VarDefs × Raw Expr -> VarDefs a × Expr a
varDefsBwd 𝒶 (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π s1 :| Nil) × s2) =
   NonEmptyList (VarDef π (desugBwd 𝒶 e1 s1) :| Nil) × desugBwd 𝒶 e2 s2
varDefsBwd 𝒶 (E.Let (E.VarDef _ e1) e2) (NonEmptyList (VarDef π s1 :| d : ds) × s2) =
   let
      NonEmptyList (d' :| ds') × s2' = varDefsBwd 𝒶 e2 (NonEmptyList (d :| ds) × s2)
   in
      NonEmptyList (VarDef π (desugBwd 𝒶 e1 s1) :| d' : ds') × s2'
varDefsBwd _ _ (NonEmptyList (_ :| _) × _) = error absurd

-- RecDefs
-- In the formalism, "group by name" is part of the syntax.
recDefsFwd :: forall a m. MonadError Error m => BoolAlg a -> RecDefs a -> m (E.RecDefs a)
recDefsFwd 𝒶 xcs = D.fromFoldable <$> traverse (recDefFwd 𝒶) xcss
   where
   xcss = map RecDef (groupBy (eq `on` fst) xcs) :: NonEmptyList (RecDef a)

recDefsBwd :: forall a. BoolAlg a -> E.RecDefs a -> Raw RecDefs -> RecDefs a
recDefsBwd 𝒶 ρ xcs = join (go (groupBy (eq `on` fst) xcs))
   where
   go :: NonEmptyList (Raw RecDefs) -> NonEmptyList (RecDefs a)
   go (NonEmptyList (xcs1 :| xcss)) =
      let
         x = fst (head xcs1)
         xcss' = case xcss of
            Nil -> Nil
            xcs2 : xcss'' -> toList (go (NonEmptyList (xcs2 :| xcss'')))
      in
         NonEmptyList (unwrap (recDefBwd 𝒶 (x ↦ get x ρ) (RecDef xcs1)) :| xcss')

-- RecDef
recDefFwd :: forall a m. MonadError Error m => BoolAlg a -> RecDef a -> m (Bind (Elim a))
recDefFwd 𝒶 xcs = (fst (head (unwrap xcs)) ↦ _) <$> clausesFwd 𝒶 (Clauses (snd <$> unwrap xcs))

recDefBwd :: forall a. BoolAlg a -> Bind (Elim a) -> Raw RecDef -> RecDef a
recDefBwd 𝒶 (x ↦ σ) (RecDef bs) = RecDef ((x × _) <$> unwrap (clausesBwd 𝒶 σ (Clauses (snd <$> bs))))

-- Expr
exprFwd :: forall a m. MonadError Error m => BoolAlg a -> Expr a -> m (E.Expr a)
exprFwd _ (Var x) = pure (E.Var x)
exprFwd _ (Op op) = pure (E.Op op)
exprFwd _ (Int α n) = pure (E.Int α n)
exprFwd _ (Float α n) = pure (E.Float α n)
exprFwd _ (Str α s) = pure (E.Str α s)
exprFwd 𝒶 (Constr α c ss) = E.Constr α c <$> traverse (desug 𝒶) ss
exprFwd 𝒶 (Record α xss) = E.Record α <$> D.fromFoldable <$> traverse (traverse (desug 𝒶)) xss
exprFwd 𝒶 (Dictionary α sss) = E.Dictionary α <$> traverse (traverse (desug 𝒶)) sss
exprFwd 𝒶 (Matrix α s (x × y) s') = E.Matrix α <$> desug 𝒶 s <@> x × y <*> desug 𝒶 s'
exprFwd 𝒶 (Lambda bs) = E.Lambda <$> clausesFwd 𝒶 bs
exprFwd 𝒶 (Project s x) = E.Project <$> desug 𝒶 s <@> x
exprFwd 𝒶 (App s1 s2) = E.App <$> desug 𝒶 s1 <*> desug 𝒶 s2
exprFwd 𝒶 (BinaryApp s1 op s2) = E.App <$> (E.App (E.Op op) <$> desug 𝒶 s1) <*> desug 𝒶 s2
exprFwd 𝒶 (MatchAs s bs) =
   E.App <$> (E.Lambda <$> clausesFwd 𝒶 (Clauses (Clause <$> first singleton <$> bs))) <*> desug 𝒶 s
exprFwd 𝒶 (IfElse s1 s2 s3) =
   E.App <$> (E.Lambda <$> (elimBool <$> (ContExpr <$> desug 𝒶 s2) <*> (ContExpr <$> desug 𝒶 s3))) <*> desug 𝒶 s1
exprFwd _ (ListEmpty α) = pure (enil α)
exprFwd 𝒶 (ListNonEmpty α s l) = econs α <$> desug 𝒶 s <*> desug 𝒶 l
exprFwd 𝒶 (ListEnum s1 s2) = E.App <$> ((E.App (E.Var "enumFromTo")) <$> desug 𝒶 s1) <*> desug 𝒶 s2
exprFwd 𝒶 (ListComp α s qs) = listCompFwd 𝒶 (α × qs × s)
exprFwd 𝒶 (Let ds s) = varDefsFwd 𝒶 (ds × s)
exprFwd 𝒶 (LetRec xcs s) = E.LetRec <$> recDefsFwd 𝒶 xcs <*> desug 𝒶 s

exprBwd :: forall a. BoolAlg a -> E.Expr a -> Raw Expr -> Expr a
exprBwd _ (E.Var _) (Var x) = Var x
exprBwd _ (E.Op _) (Op op) = Op op
exprBwd _ (E.Int α _) (Int _ n) = Int α n
exprBwd _ (E.Float α _) (Float _ n) = Float α n
exprBwd _ (E.Str α _) (Str _ str) = Str α str
exprBwd 𝒶 (E.Constr α _ es) (Constr _ c ss) = Constr α c (uncurry (desugBwd 𝒶) <$> zip es ss)
exprBwd 𝒶 (E.Record α xes) (Record _ xss) =
   Record α $ xss <#> \(x ↦ s) -> x ↦ desugBwd 𝒶 (get x xes) s
exprBwd 𝒶 (E.Dictionary α ees) (Dictionary _ sss) =
   Dictionary α (zipWith (\(Pair e e') (Pair s s') -> Pair (desugBwd 𝒶 e s) (desugBwd 𝒶 e' s')) ees sss)
exprBwd 𝒶 (E.Matrix α e1 _ e2) (Matrix _ s1 (x × y) s2) =
   Matrix α (desugBwd 𝒶 e1 s1) (x × y) (desugBwd 𝒶 e2 s2)
exprBwd 𝒶 (E.Lambda σ) (Lambda bs) = Lambda (clausesBwd 𝒶 σ bs)
exprBwd 𝒶 (E.Project e _) (Project s x) = Project (desugBwd 𝒶 e s) x
exprBwd 𝒶 (E.App e1 e2) (App s1 s2) = App (desugBwd 𝒶 e1 s1) (desugBwd 𝒶 e2 s2)
exprBwd 𝒶 (E.App (E.App (E.Op _) e1) e2) (BinaryApp s1 op s2) =
   BinaryApp (desugBwd 𝒶 e1 s1) op (desugBwd 𝒶 e2 s2)
exprBwd 𝒶 (E.App (E.Lambda σ) e) (MatchAs s bs) =
   MatchAs (desugBwd 𝒶 e s)
      (first head <$> unwrap <$> unwrap (clausesBwd 𝒶 σ (Clauses (Clause <$> first NE.singleton <$> bs))))
exprBwd 𝒶 (E.App (E.Lambda (ElimConstr m)) e1) (IfElse s1 s2 s3) =
   IfElse (desugBwd 𝒶 e1 s1)
      (desugBwd 𝒶 (asExpr (get cTrue m)) s2)
      (desugBwd 𝒶 (asExpr (get cFalse m)) s3)
exprBwd _ (E.Constr α _ Nil) (ListEmpty _) = ListEmpty α
exprBwd 𝒶 (E.Constr α _ (e1 : e2 : Nil)) (ListNonEmpty _ s l) =
   ListNonEmpty α (desugBwd 𝒶 e1 s) (desugBwd 𝒶 e2 l)
exprBwd 𝒶 (E.App (E.App (E.Var "enumFromTo") e1) e2) (ListEnum s1 s2) =
   ListEnum (desugBwd 𝒶 e1 s1) (desugBwd 𝒶 e2 s2)
exprBwd 𝒶 e (ListComp _ s qs) =
   let α × qs' × s' = listCompBwd 𝒶 e (qs × s) in ListComp α s' qs'
exprBwd 𝒶 (E.Let d e) (Let ds s) = uncurry Let (varDefsBwd 𝒶 (E.Let d e) (ds × s))
exprBwd 𝒶 (E.LetRec xσs e) (LetRec xcs s) = LetRec (recDefsBwd 𝒶 xσs xcs) (desugBwd 𝒶 e s)
exprBwd _ _ _ = error absurd

-- ListRest
listRestFwd :: forall a m. MonadError Error m => BoolAlg a -> ListRest a -> m (E.Expr a)
listRestFwd _ (End α) = pure (enil α)
listRestFwd 𝒶 (Next α s l) = econs α <$> desug 𝒶 s <*> desug 𝒶 l

listRestBwd :: forall a. BoolAlg a -> E.Expr a -> Raw ListRest -> ListRest a
listRestBwd _ (E.Constr α _ _) (End _) = End α
listRestBwd 𝒶 (E.Constr α _ (e1 : e2 : Nil)) (Next _ s l) =
   Next α (desugBwd 𝒶 e1 s) (desugBwd 𝒶 e2 l)
listRestBwd _ _ _ = error absurd

-- List Qualifier × Expr
listCompFwd :: forall a m. MonadError Error m => BoolAlg a -> a × List (Qualifier a) × Expr a -> m (E.Expr a)
listCompFwd 𝒶 (α × Nil × s) =
   econs α <$> desug 𝒶 s <@> enil α
listCompFwd 𝒶 (α × (Guard s : qs) × s') = do
   e <- listCompFwd 𝒶 (α × qs × s')
   E.App (E.Lambda (elimBool (ContExpr e) (ContExpr (enil α)))) <$> desug 𝒶 s
listCompFwd 𝒶 (α × (Declaration (VarDef π s) : qs) × s') = do
   e <- ContExpr <$> listCompFwd 𝒶 (α × qs × s')
   σ <- pattContFwd π e
   E.App (E.Lambda σ) <$> desug 𝒶 s
listCompFwd 𝒶 (α × (Generator p s : qs) × s') = do
   e <- ContExpr <$> listCompFwd 𝒶 (α × qs × s')
   σ <- pattContFwd p e
   E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (orElseFwd (ContElim σ) α)))) <$> desug 𝒶 s

listCompBwd
   :: forall a
    . BoolAlg a
   -> E.Expr a
   -> List (Raw Qualifier) × Raw Expr
   -> a × List (Qualifier a) × Expr a
listCompBwd 𝒶 (E.Constr α2 c (e : E.Constr α1 c' Nil : Nil)) (Nil × s) | c == cCons && c' == cNil =
   (α1 `𝒶.join` α2) × Nil × desugBwd 𝒶 e s
listCompBwd 𝒶 (E.App (E.Lambda (ElimConstr m)) e) ((Guard s0 : qs) × s) =
   case listCompBwd 𝒶 (asExpr (get cTrue m)) (qs × s) × asExpr (get cFalse m) of
      (α × qs' × s') × E.Constr β c Nil | c == cNil -> (α `𝒶.join` β) × (Guard (desugBwd 𝒶 e s0) : qs') × s'
      _ -> error absurd
listCompBwd 𝒶 (E.App (E.Lambda σ) e) ((Declaration (VarDef π s0) : qs) × s) =
   case listCompBwd 𝒶 (asExpr (pattContBwd π σ)) (qs × s) of
      α × qs' × s' -> α × (Declaration (VarDef π (desugBwd 𝒶 e s0)) : qs') × s'
listCompBwd 𝒶 (E.App (E.App (E.Var "concatMap") (E.Lambda σ)) e) ((Generator p s0 : qs) × s) =
   case orElseBwd 𝒶 (ContElim σ) (Left p : Nil) of
      σ' × β -> case listCompBwd 𝒶 (asExpr (pattContBwd p (asElim σ'))) (qs × s) of
         α × qs' × s' -> (α `𝒶.join` β) × (Generator p (desugBwd 𝒶 e s0) : qs') × s'
listCompBwd _ _ _ = error absurd

-- NonEmptyList Pattern × Expr
pattsExprFwd :: forall a m. MonadError Error m => BoolAlg a -> NonEmptyList Pattern × Expr a -> m (Elim a)
pattsExprFwd 𝒶 (NonEmptyList (p :| Nil) × s) = (ContExpr <$> desug 𝒶 s) >>= pattContFwd p
pattsExprFwd 𝒶 (NonEmptyList (p :| p' : ps) × s) =
   pattContFwd p =<< ContExpr <$> E.Lambda <$> pattsExprFwd 𝒶 (NonEmptyList (p' :| ps) × s)

pattsExprBwd :: forall a. BoolAlg a -> NonEmptyList Pattern × Raw Expr -> Elim a -> Expr a
pattsExprBwd 𝒶 (NonEmptyList (p :| Nil) × s) σ = desugBwd 𝒶 (asExpr (pattContBwd p σ)) s
pattsExprBwd 𝒶 (NonEmptyList (p :| p' : ps) × s) σ = next (asExpr (pattContBwd p σ))
   where
   next (E.Lambda τ) = pattsExprBwd 𝒶 (NonEmptyList (p' :| ps) × s) τ
   next _ = error absurd

-- Pattern × Cont
pattContFwd :: forall a m. MonadError Error m => Pattern -> Cont a -> m (Elim a)
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
pattCont_ListRest_Fwd :: forall a m. MonadError Error m => ListRestPattern -> Cont a -> m (Elim a)
pattCont_ListRest_Fwd PEnd κ = pure (ElimConstr (D.singleton cNil κ))
pattCont_ListRest_Fwd (PNext p o) κ = ElimConstr <$> D.singleton cCons <$> pattArgsFwd (Left p : Right o : Nil) κ

pattCont_ListRest_Bwd :: forall a. Elim a -> ListRestPattern -> Cont a
pattCont_ListRest_Bwd (ElimVar _ _) _ = error absurd
pattCont_ListRest_Bwd (ElimRecord _ _) _ = error absurd
pattCont_ListRest_Bwd (ElimConstr m) PEnd = get cNil m
pattCont_ListRest_Bwd (ElimConstr m) (PNext p o) = pattArgsBwd (Left p : Right o : Nil) (get cCons m)

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
clausesFwd :: forall a m. MonadError Error m => BoolAlg a -> Clauses a -> m (Elim a)
clausesFwd 𝒶 (Clauses bs) = do
   NonEmptyList (σ :| σs) <- traverse (pattsExprFwd 𝒶) (unwrap <$> bs)
   pure $ foldl (\σ1 σ2 -> (slices 𝒶 σ1).join σ1 σ2) σ σs -- previously maybeJoin

clausesBwd :: forall a. BoolAlg a -> Elim a -> Raw Clauses -> Clauses a
clausesBwd 𝒶 σ (Clauses bs) = Clauses (clauseBwd <$> bs)
   where
   clauseBwd :: Raw Clause -> Clause a
   clauseBwd (Clause (πs × s)) = Clause (πs × pattsExprBwd 𝒶 (πs × s) σ)

-- orElse
orElseFwd :: forall a. Cont a -> a -> Cont a
orElseFwd ContNone _ = error absurd
orElseFwd (ContExpr e) _ = ContExpr e
orElseFwd (ContElim (ElimConstr m)) α = ContElim (ElimConstr (unlessFwd (c × orElseFwd κ α) α))
   where
   c × κ = asSingletonMap m
orElseFwd (ContElim (ElimRecord xs κ)) α = ContElim (ElimRecord xs (orElseFwd κ α))
orElseFwd (ContElim (ElimVar x κ)) α = ContElim (ElimVar x (orElseFwd κ α))

orElseBwd :: forall a. BoolAlg a -> Cont a -> List (Pattern + ListRestPattern) -> Cont a × a
orElseBwd 𝒶 κ Nil = κ × 𝒶.bot
orElseBwd _ ContNone _ = error absurd
orElseBwd 𝒶 (ContElim (ElimVar _ κ')) (Left (PVar x) : πs) =
   orElseBwd 𝒶 κ' πs # first (\κ'' -> ContElim (ElimVar x κ''))
orElseBwd 𝒶 (ContElim (ElimRecord _ κ')) (Left (PRecord xps) : πs) =
   orElseBwd 𝒶 κ' ((xps <#> (snd >>> Left)) <> πs) # first (\κ'' -> ContElim (ElimRecord (keys xps) κ''))
orElseBwd 𝒶 (ContElim (ElimConstr m)) (π : πs) =
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
      κ' × α = unlessBwd 𝒶 m c
   in
      orElseBwd 𝒶 κ' (πs' <> πs) #
         (\κ'' -> ContElim (ElimConstr (D.fromFoldable (singleton (c × κ''))))) *** (α `𝒶.join` _)
orElseBwd _ _ _ = error absurd

-- In forward direction, extend singleton branch to set of branches where any missing constructors have
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

unlessBwd :: forall a. BoolAlg a -> Dict (Cont a) -> Ctr -> Cont a × a
unlessBwd 𝒶 m c =
   let
      cs = (ctrs (successful (dataTypeFor c)) # S.toUnfoldable) \\ L.singleton c
   in
      unsafePartial $ get c m × foldl (𝒶.join) 𝒶.bot ((bodyAnn <<< body) <$> cs)
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

