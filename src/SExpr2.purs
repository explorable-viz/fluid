module SExpr2 where

import Prelude

import Bindings (Var, Bind, varAnon, (↦), keys)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (applyN, on)
import Data.List (List(..), (:), (\\), sortBy)
import Data.List (singleton) as L
import Data.List.NonEmpty (NonEmptyList(..), groupBy, head)
import Data.NonEmpty ((:|))
import Data.Set (toUnfoldable) as S
import Data.Traversable (traverse)
import Data.Tuple (fst, snd, uncurry)
import DataType (Ctr, arity, checkArity, cCons, cFalse, cNil, cTrue, ctrs, dataTypeFor)
import Dict (asSingletonMap)
import Dict as D
import Expr2 (Expr(..), RecDefs, VarDef(..)) as E
import Expr2 (class Desugarable, Cont(..), Elim(..), Expr, asElim, desug, mkSugar)
import Lattice2 (class JoinSemilattice, definedJoin, join, neg)
import Util (type (×), (×), type (+), absurd, error, unimplemented, successful)

scons :: forall a. a -> E.Expr a -> E.Expr a -> E.Expr a
scons ann head tail = E.Constr ann cCons (head : tail : Nil)

snil :: forall a. a -> E.Expr a
snil ann = E.Constr ann cNil Nil

elimBool :: forall a'. Cont a' -> Cont a' -> Elim a'
elimBool κ κ' = ElimConstr (D.fromFoldable [ cTrue × κ, cFalse × κ' ])

instance JoinSemilattice a => Desugarable SExpr a where
    desug (BinaryApp l op r)           = E.App (E.App (E.Op op) l) r 
    desug (MatchAs guard patterns)     = E.App (E.Lambda (clauses patterns)) guard 
    desug (IfElse guard trueP falseP)  = E.App (E.Lambda (elimBool (ContExpr trueP) (ContExpr falseP))) guard                                             
    desug (ListEmpty ann)              = E.Constr ann cNil Nil

    desug (ListNonEmpty ann head rest) = scons ann head (mkSugar rest)

    desug (ListEnum head last)         = E.App (E.App (E.Var "enumFromTo") head) last
    desug (ListComp _ body (NonEmptyList (Guard (E.Constr ann2 c Nil) :| Nil))) | c == cTrue = 
        scons ann2 body (snil ann2) -- not sure about this at all
    desug (ListComp ann body (NonEmptyList (q :| Nil))) = 
        desug (ListComp ann body (NonEmptyList  (q :| Guard (E.Constr ann cTrue Nil) : Nil))) -- may need to be mkSugar
    desug (ListComp ann body (NonEmptyList (Guard s :| q : qs))) = 
        let e = mkSugar (ListComp ann body (NonEmptyList (q :| qs))) in
        E.App (E.Lambda (elimBool (ContExpr e) (ContExpr (snil ann)))) s
    desug (ListComp ann body (NonEmptyList (Declaration (VarDef pi s) :| q : qs))) = 
        let 
            e   = mkSugar (ListComp ann body (NonEmptyList (q :| qs)))
            sig = desugPWithC pi (ContExpr e :: Cont a)
        in
        E.App (E.Lambda sig) (mkSugar s)
    desug (ListComp ann body (NonEmptyList (Generator p s :| q : qs))) = 
        let 
            e = mkSugar (ListComp ann body (NonEmptyList (q :| qs)))
            sig = desugPWithC p (ContExpr e)
        in
        E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (totalCont (ContElim sig) ann)))) s
    desug (Let defs exp)               = processVarDefs (defs × exp)
    desug (LetRec recdefs exp)         = E.LetRec (processRecDefs recdefs) exp

instance JoinSemilattice a => Desugarable ListRest a where
    desug (End ann) = E.Constr ann cNil Nil
    desug (Next ann head rest) = scons ann (desug head) (mkSugar rest)

-- instance JoinSemilattice a => Desugarable VarDefs a where
processVarDefs :: forall a. JoinSemilattice a => VarDefs a × E.Expr a -> E.Expr a
processVarDefs (NonEmptyList (d :| Nil) × exp) = E.Let (processVarDef d) (mkSugar exp)
processVarDefs (NonEmptyList (d :| d' : ds) × exp) = 
    E.Let (processVarDef d) (processVarDefs (NonEmptyList (d' :| ds) × exp))

processVarDef :: forall a. JoinSemilattice a => VarDef a -> E.VarDef a
processVarDef (VarDef pat exp) = E.VarDef (desugPWithC pat (ContNone :: Cont a)) (mkSugar exp)

processRecDefs :: forall a. JoinSemilattice a => RecDefs a -> E.RecDefs a
processRecDefs cls = D.fromFoldable $ map processRecDef clss
    where
    clss = groupBy (eq `on` fst) cls :: NonEmptyList (NonEmptyList (Clause a))

processRecDef :: forall a. JoinSemilattice a => NonEmptyList (Clause a) -> Bind (Elim a)
processRecDef x = 
    let pairer = (fst (head x) ↦ _)          :: forall b. b -> Bind b   
        cls    =  clausesCurried (map snd x) :: Elim a
    in 
        pairer cls

clause :: forall a. JoinSemilattice a => Pattern × Expr a -> Elim a
clause (pat × exp) = let cont = ContExpr exp in desugPWithC pat cont

clausesCurried :: forall a. JoinSemilattice a => NonEmptyList (Branch a) -> Elim a
clausesCurried cls = 
            let NonEmptyList (head :| rest) = traverse desugPWithC cls in
                foldl join head rest
clauses :: forall a. JoinSemilattice a => NonEmptyList (Pattern × Expr a) -> Elim a
clauses cls = 
            let NonEmptyList (head :| rest) = map clause cls in
                foldl join head rest

desugPWithC :: forall a. Pattern -> Cont a -> Elim a
desugPWithC (PVar x)              k = ElimVar x k
desugPWithC (PConstr c ps)        k = 
    ElimConstr ((D.singleton c) (argPat (map Left ps) k)) 
desugPWithC (PRecord bps)         k = ElimRecord (keys bps) (recordPat (sortBy (flip compare `on` fst) bps) k)
desugPWithC  PListEmpty           k = ElimConstr (D.singleton cNil k)
desugPWithC (PListNonEmpty p lrp) k = ElimConstr (D.singleton cCons (argPat (Left p : Right lrp : Nil) k))

desugPsWithC :: forall a. JoinSemilattice a => NonEmptyList Pattern × Expr a -> Elim a
desugPsWithC (NonEmptyList (p :| Nil) × exp)     = clause (p × exp)
desugPsWithC (NonEmptyList (p :| p' : ps) × exp) = 
    desugPWithC p (ContExpr (E.Lambda (desugPsWithC (NonEmptyList (p' :| ps) × exp))))

argPat :: forall a. List (Pattern + ListRestPattern) -> Cont a -> Cont a
argPat Nil k = k
argPat (Left p : pis) k = let apf = argPat pis k   in ContElim (desugPWithC p apf)
argPat (Right o: pis) k = let apf = argPat pis k in ContElim (listRestPat o apf)

listRestPat :: forall a. ListRestPattern -> Cont a -> Elim a
listRestPat PEnd k = ElimConstr (D.singleton cNil k)
listRestPat (PNext p o) k = ElimConstr (D.singleton cCons (argPat (Left p : Right o : Nil) k))

recordPat :: forall a. List (Bind Pattern) -> Cont a -> Cont a
recordPat Nil k = k
recordPat (_ ↦ p : xps) k = ContElim (desugPWithC p k)

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
        cks = map defaultBranch ((ctrs (successful (dataTypeFor c)) # S.toUnfoldable ) \\ L.singleton c)
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
   | Next a (SExpr a) (ListRest a)

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
type Branch a = NonEmptyList Pattern × SExpr a
type Clause a = Var × Branch a
type RecDefs a = NonEmptyList (Clause a)

-- The pattern/expr relationship is different to the one in branch (the expr is the "argument", not the "body").
-- Using a data type makes for easier overloading.
data VarDef a = VarDef Pattern (SExpr a)
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
