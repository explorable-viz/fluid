module WellFormed where

import Prelude

import Bind (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.List.NonEmpty (head, tail)
import Data.Traversable (traverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, unions)
import Data.Set as Set
import Data.Tuple (fst, snd)
import Effect.Exception (Error)
import Expr (bv, fv)
import Expr (Module(..), RecDefs(..)) as E
import Lattice (Raw)
import SExpr (Clause(..), DictEntry(..), Expr(..), LambdaClause(..), ListRest(..), Module, ParagraphElem(..), Stmt(..), VarDef(..)) as S
import Util (throw, (×))
import Util.Map (keys)
import Util.Set ((\\), (∪))
import Val (Env)

-- ======================
-- Entry points
-- ======================

checkProgram :: forall m. MonadError Error m => Set Var -> Raw S.Stmt -> m Unit
checkProgram initialScope s = void $ checkDA (mapFromSet true initialScope) s

checkModule :: forall m. MonadError Error m => Raw S.Module -> m Unit
checkModule _ = pure unit

-- Names exported by a desugared module.
moduleExports :: forall a. E.Module a -> Set Var
moduleExports (E.Module ds) = unions (defNames <$> ds)
   where
   defNames (Left vd) = bv vd
   defNames (Right (E.RecDefs _ ρ)) = keys ρ

envNames :: forall a. Env a -> Set Var
envNames = keys

-- ======================
-- Definite-assignment contexts and result types (PurePy spec §2.1)
-- ======================

-- Context Γ : Var ⇀ B, where B = {tt, ff}. Absent key = ⊥ (undefined).
-- True = definitely assigned (tt); False = not definitely assigned (ff).
type Ctx = Map Var Boolean

-- Well-formedness result type R ::= Returns | Assigns Δ.
data Result = Returns | Assigns Ctx

derive instance Eq Result

-- Sequential composition Γ · Δ on contexts. Right-biased: Δ overrides Γ.
overrideCtx :: Ctx -> Ctx -> Ctx
overrideCtx = flip Map.union

-- Parallel composition Γ ⊕ Δ on contexts.
-- Both defined → conjunction of statuses.
-- Only one defined → ff (the var is "lost" in the merge).
-- Both ⊥ → ⊥.
mergeCtx :: Ctx -> Ctx -> Ctx
mergeCtx γ1 γ2 =
   foldl (\acc k -> Map.insert k (mergedAt k) acc) Map.empty allKeys
   where
   allKeys :: Set Var
   allKeys = Set.fromFoldable (Map.keys γ1) `Set.union` Set.fromFoldable (Map.keys γ2)
   mergedAt k = case Map.lookup k γ1, Map.lookup k γ2 of
      Just a, Just b -> a && b
      _, _ -> false

-- Lifted to Result. Returns is zero for · and unit for ⊕.
overrideRes :: Result -> Result -> Result
overrideRes _ Returns = Returns
overrideRes Returns _ = Returns
overrideRes (Assigns a) (Assigns b) = Assigns (overrideCtx a b)

mergeRes :: Result -> Result -> Result
mergeRes Returns r = r
mergeRes r Returns = r
mergeRes (Assigns a) (Assigns b) = Assigns (mergeCtx a b)

-- ======================
-- Syntactic helpers (PurePy spec §2.2)
-- ======================

-- assigns(s): over-approximation of variables assigned anywhere in s, without
-- descending into nested function definitions. Each def's NAME is included;
-- its body's assigns are not.
assigns :: forall a. S.Stmt a -> Set Var
assigns S.Pass = Set.empty
assigns (S.Def (S.VarDef p _)) = bv p
assigns (S.ExprStmt _) = Set.empty
assigns (S.Assert _ _) = Set.empty
assigns (S.Return _) = Set.empty
assigns (S.If clauses elseBody) =
   unions (assigns <$> (snd <$> clauses)) ∪ assigns elseBody
assigns (S.Match _ branches) =
   unions (assigns <$> (snd <$> branches))
assigns (S.DefRec rs) = unions (Set.singleton <<< fst <$> rs)
assigns (S.Seq s1 s2) = assigns s1 ∪ assigns s2

-- captures(s): vars from the enclosing scope referenced by closures (lambdas or
-- nested function definitions) within s. The interesting cases are def-regions
-- and lambdas; other forms just recurse.
captures :: forall a. S.Stmt a -> Set Var
captures S.Pass = Set.empty
captures (S.Def (S.VarDef _ e)) = capturesE e
captures (S.ExprStmt e) = capturesE e
captures (S.Assert cond msg) = capturesE cond ∪ maybe Set.empty capturesE msg
captures (S.Return e) = capturesE e
captures (S.If clauses elseBody) =
   unions ((\(cond × body) -> capturesE cond ∪ captures body) <$> clauses)
      ∪ captures elseBody
captures (S.Match scrut branches) =
   capturesE scrut ∪ unions ((\(_ × body) -> captures body) <$> branches)
captures (S.DefRec rs) =
   (unions (branchCaptures <$> rs)) \\ unions (Set.singleton <<< fst <$> rs)
   where
   branchCaptures (_ × S.Clause (ps × body)) =
      (fv body \\ unions (bv <$> ps)) \\ assigns body
captures (S.Seq s1 s2) = captures s1 ∪ captures s2

-- captures lifted to expressions. Vars on their own don't capture; only
-- closure-introducing forms do.
capturesE :: forall a. S.Expr a -> Set Var
capturesE (S.Var _) = Set.empty
capturesE (S.Op _) = Set.empty
capturesE (S.Int _ _) = Set.empty
capturesE (S.Float _ _) = Set.empty
capturesE (S.Str _ _) = Set.empty
capturesE (S.Constr _ _ es) = unions (capturesE <$> es)
capturesE (S.Dictionary _ entries) =
   unions ((\(k × v) -> capturesEntry k ∪ capturesE v) <$> entries)
   where
   capturesEntry (S.ExprKey e) = capturesE e
   capturesEntry (S.VarKey _ _) = Set.empty
capturesE (S.Matrix _ body (x × y) source) =
   (capturesE body \\ (Set.singleton x ∪ Set.singleton y)) ∪ capturesE source
capturesE (S.Lambda (S.LambdaClause (ps × body))) =
   fv body \\ unions (bv <$> ps)
capturesE (S.Project e _) = capturesE e
capturesE (S.DProject e e') = capturesE e ∪ capturesE e'
capturesE (S.App e e') = capturesE e ∪ capturesE e'
capturesE (S.BinaryApp e _ e') = capturesE e ∪ capturesE e'
capturesE (S.UnaryPrefixApp _ e) = capturesE e
capturesE (S.Ternary cond e1 e2) = capturesE cond ∪ capturesE e1 ∪ capturesE e2
capturesE (S.Paragraph elems) = unions (capturesPe <$> elems)
   where
   capturesPe (S.Token _) = Set.empty
   capturesPe (S.Unquote e) = capturesE e
capturesE (S.ListEmpty _) = Set.empty
capturesE (S.ListNonEmpty _ e l) = capturesE e ∪ capturesEListRest l
   where
   capturesEListRest (S.End _) = Set.empty
   capturesEListRest (S.Next _ e' l') = capturesE e' ∪ capturesEListRest l'
capturesE (S.ListEnum e1 e2) = capturesE e1 ∪ capturesE e2
capturesE (S.ListComp _ e _) = capturesE e -- simplified; full qualifier handling later
capturesE (S.DocExpr e e') = capturesE e ∪ capturesE e'

-- ======================
-- Well-formedness judgement (PurePy spec §2.3)
-- ======================
--
-- checkDA Γ s computes Result for s under context Γ, and verifies every
-- variable reference is tt-bound. Returns | Assigns Δ.

checkDA :: forall m a. MonadError Error m => Ctx -> S.Stmt a -> m Result
checkDA _ S.Pass = pure (Assigns Map.empty)
checkDA γ (S.Return e) = do
   checkExprDA γ e
   pure Returns
checkDA γ (S.ExprStmt e) = do
   checkExprDA γ e
   pure (Assigns Map.empty)
checkDA γ (S.Assert cond msg) = do
   checkExprDA γ cond
   case msg of
      Just m -> checkExprDA γ m
      Nothing -> pure unit
   pure (Assigns Map.empty)
checkDA γ (S.Def (S.VarDef p e)) = do
   checkExprDA γ e
   pure (Assigns (mapFromSet true (bv p)))
checkDA γ (S.DefRec rs) = do
   let regionNames = unions (Set.singleton <<< fst <$> rs)
   let γ' = γ `overrideCtx` mapFromSet true regionNames
   for_ rs \(_ × S.Clause (ps × body)) -> do
      let params = unions (bv <$> ps)
      let γ'' = γ' `overrideCtx` mapFromSet true params
      void $ checkDA γ'' body
   pure (Assigns (mapFromSet true regionNames))
checkDA γ (S.Seq s1 s2) = do
   r1 <- checkDA γ s1
   case r1 of
      Returns -> throw "Unreachable statement"
      Assigns δ -> do
         r2 <- checkDA (γ `overrideCtx` δ) s2
         pure (overrideRes r1 r2)
checkDA γ (S.If clauses elseBody) = do
   clauseRs <- traverse
      ( \(cond × body) -> do
           checkExprDA γ cond
           checkDA γ body
      )
      clauses
   elseR <- checkDA γ elseBody
   pure $ foldl mergeRes elseR clauseRs
checkDA γ (S.Match scrut branches) = do
   checkExprDA γ scrut
   branchRs <- traverse
      ( \(p × body) -> do
           let pBindings = bv p
           let γ' = γ `overrideCtx` mapFromSet true pBindings
           r <- checkDA γ' body
           -- pattern bindings are branch-local; strip from result
           pure $ stripVars pBindings r
      )
      branches
   pure $ foldl mergeRes (head branchRs) (tail branchRs)

-- Check every variable reference in an expression is tt-bound in Γ.
checkExprDA :: forall m a. MonadError Error m => Ctx -> S.Expr a -> m Unit
checkExprDA γ e = do
   let refs = fv e
   for_ refs \v -> case Map.lookup v γ of
      Just true -> pure unit
      Just false -> throw $ "Not definitely assigned: " <> v
      Nothing -> throw $ "Unbound name: " <> v

mapFromSet :: forall k v. Ord k => v -> Set k -> Map k v
mapFromSet v = foldl (\acc k -> Map.insert k v acc) Map.empty

stripVars :: Set Var -> Result -> Result
stripVars _ Returns = Returns
stripVars vars (Assigns δ) = Assigns (foldl (flip Map.delete) δ vars)
