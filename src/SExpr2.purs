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
import Data.Tuple (fst, snd)
import DataType (Ctr, arity, cCons, cFalse, cNil, cTrue, ctrs, dataTypeFor)
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
    -- Correct
    desug (BinaryApp l op r)           = E.App (E.App (E.Op op) l) r 
    -- Correct dependent on "clauses" which is meant to be equivalent to branchesFwd_uncurried
    desug (MatchAs guard patterns)     = E.App (E.Lambda (branchesFwd_uncurried patterns)) guard 
    -- Correct
    desug (IfElse guard trueP falseP)  = E.App (E.Lambda (elimBool (ContExpr trueP) (ContExpr falseP))) guard
    -- Correct                                             
    desug (ListEmpty ann)              = E.Constr ann cNil Nil


    -- Needs to be checked but either this is correct or, we need mkSugar in parsing
    desug (ListNonEmpty ann head rest) = scons ann head (mkSugar rest)
    
    
    
    -- Correct
    desug (ListEnum head last)         = E.App (E.App (E.Var "enumFromTo") head) last
    desug (ListComp _ body (NonEmptyList (Guard (E.Constr ann2 c Nil) :| Nil))) | c == cTrue = 
        scons ann2 body (snil ann2) -- Should be correct
    -- Need to check this one
    desug (ListComp ann body (NonEmptyList (q :| Nil))) = 
        desug (ListComp ann body (NonEmptyList  (q :| Guard (E.Constr ann cTrue Nil) : Nil))) -- may need to be mkSugar 
    -- Need to check
    desug (ListComp ann body (NonEmptyList (Guard s :| q : qs))) = 
        let e = mkSugar (ListComp ann body (NonEmptyList (q :| qs))) in
        E.App (E.Lambda (elimBool (ContExpr e) (ContExpr (snil ann)))) s
    -- Need to check the mkSugar's here
    desug (ListComp ann body (NonEmptyList (Declaration (VarDef pi s) :| q : qs))) = 
        let 
            e   = mkSugar (ListComp ann body (NonEmptyList (q :| qs)))
            sig = patternFwd pi (ContExpr e :: Cont a)
        in
        E.App (E.Lambda sig) (mkSugar s)
    -- Need to check mkSugars
    desug (ListComp ann body (NonEmptyList (Generator p s :| q : qs))) = 
        let 
            e = mkSugar (ListComp ann body (NonEmptyList (q :| qs)))
            sig = patternFwd p (ContExpr e)
        in
        E.App (E.App (E.Var "concatMap") (E.Lambda (asElim (totalCont (ContElim sig) ann)))) s
    -- these 2 are correct if their auxiliaries are correct
    desug (Let defs exp)               = processVarDefs (defs × exp)
    desug (LetRec recdefs exp)         = E.LetRec (processRecDefs recdefs) exp




-- ListRest auxiliaries
instance JoinSemilattice a => Desugarable ListRest a where
    desug (End ann) = E.Constr ann cNil Nil
    desug (Next ann head rest) = scons ann head (mkSugar rest)






-- vardefsFwd equivalent
processVarDefs :: forall a. JoinSemilattice a => VarDefs a × E.Expr a -> E.Expr a
processVarDefs (NonEmptyList (d :| Nil) × exp) = E.Let (processVarDef d) (mkSugar exp)
processVarDefs (NonEmptyList (d :| d' : ds) × exp) = 
    E.Let (processVarDef d) (processVarDefs (NonEmptyList (d' :| ds) × exp))


--vardefFwd equivalent
processVarDef :: forall a. JoinSemilattice a => VarDef a -> E.VarDef a
processVarDef (VarDef pat exp) = E.VarDef (patternFwd pat (ContNone :: Cont a)) (mkSugar exp)

-- recdefsFwd equivalent
processRecDefs :: forall a. JoinSemilattice a => RecDefs a -> E.RecDefs a
processRecDefs cls = D.fromFoldable $ map processRecDef clss
    where
    clss = groupBy (eq `on` fst) cls :: NonEmptyList (NonEmptyList (Clause a))
-- recdefFwd equivalent
processRecDef :: forall a. JoinSemilattice a => NonEmptyList (Clause a) -> Bind (Elim a)
processRecDef x = 
    let pairer = (fst (head x) ↦ _)          :: forall b. b -> Bind b   
        cls    =  branchesFwd_curried (map snd x) :: Elim a
    in 
        pairer cls

-- clause functions equivalent to branches
branchFwd :: forall a. JoinSemilattice a => Pattern × Expr a -> Elim a
branchFwd (pat × exp) = let cont = ContExpr exp in patternFwd pat cont

branchesFwd_curried :: forall a. JoinSemilattice a => NonEmptyList (Branch a) -> Elim a
branchesFwd_curried cls = 
            let NonEmptyList (head :| rest) = map patternsFwd cls in
                foldl join head rest
branchesFwd_uncurried :: forall a. JoinSemilattice a => NonEmptyList (Pattern × Expr a) -> Elim a
branchesFwd_uncurried cls = 
            let NonEmptyList (head :| rest) = map branchFwd cls in
                foldl join head rest

-- these are equivalent to patternsFwd etc
patternFwd :: forall a. Pattern -> Cont a -> Elim a
patternFwd (PVar x)              k = ElimVar x k
patternFwd (PConstr c ps)        k = 
    ElimConstr ((D.singleton c) (argPat (map Left ps) k)) 
patternFwd (PRecord bps)         k = ElimRecord (keys bps) (recordPat (sortBy (flip compare `on` fst) bps) k)
patternFwd  PListEmpty           k = ElimConstr (D.singleton cNil k)
patternFwd (PListNonEmpty p lrp) k = ElimConstr (D.singleton cCons (argPat (Left p : Right lrp : Nil) k))

patternsFwd :: forall a. JoinSemilattice a => NonEmptyList Pattern × Expr a -> Elim a
patternsFwd (NonEmptyList (p :| Nil) × exp)     = branchFwd (p × exp)
patternsFwd (NonEmptyList (p :| p' : ps) × exp) = 
    patternFwd p (ContExpr (E.Lambda (patternsFwd (NonEmptyList (p' :| ps) × exp))))

argPat :: forall a. List (Pattern + ListRestPattern) -> Cont a -> Cont a
argPat Nil k = k
argPat (Left p : pis) k = let apf = argPat pis k   in ContElim (patternFwd p apf)
argPat (Right o: pis) k = let apf = argPat pis k in ContElim (listRestPat o apf)

listRestPat :: forall a. ListRestPattern -> Cont a -> Elim a
listRestPat PEnd k = ElimConstr (D.singleton cNil k)
listRestPat (PNext p o) k = ElimConstr (D.singleton cCons (argPat (Left p : Right o : Nil) k))

recordPat :: forall a. List (Bind Pattern) -> Cont a -> Cont a
recordPat Nil k = k
recordPat (_ ↦ p : xps) k = ContElim (patternFwd p k)

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