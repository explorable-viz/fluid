module WellFormed where

import Prelude

import Bind (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.List (List(..))
import Data.List.NonEmpty (head, tail)
import DataType (cNone)
import Data.Traversable (traverse)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set, unions)
import Data.Set as Set
import Data.Tuple (fst, snd)
import DefiniteAssignment (Ctx, TyResult(..), assignsEmpty, mergeRes, overrideCtx, overrideRes)
import Effect.Exception (Error)
import Expr (bv, fv)
import Expr (Module(..), RecDefs(..)) as E
import Lattice (Raw)
import SExpr (Clause(..), DictEntry(..), Expr(..), LambdaClause(..), ListRest(..), Module, ParagraphElem(..), Stmt(..), VarDef(..)) as S
import Util (type (×), throw, (×))
import Util.Map (keys)
import Util.Set ((\\), (∪))
import Val (Env)

-- ======================
-- Entry points
-- ======================

checkProgram :: forall m. MonadError Error m => Set Var -> Raw S.Stmt -> m (S.Stmt TyResult)
checkProgram initialScope s = do
   _ × s' <- checkDA (mapFromSet true initialScope) s
   pure (implicitNone s')

-- Post-validation normalisation: function bodies whose TyResult is Assigns
-- (fall-through possible) get an implicit `return None` appended.
implicitNone :: S.Stmt TyResult -> S.Stmt TyResult
implicitNone S.Pass = S.Pass
implicitNone (S.Return e) = S.Return e
implicitNone (S.ExprStmt e) = S.ExprStmt e
implicitNone (S.Assert cond msg) = S.Assert cond msg
implicitNone (S.Def vd) = S.Def vd
implicitNone (S.Seq s1 s2) = S.Seq (implicitNone s1) (implicitNone s2)
implicitNone (S.If clauses elseBody) =
   S.If ((\(c × b) -> c × implicitNone b) <$> clauses) (implicitNone elseBody)
implicitNone (S.Match scrut branches) =
   S.Match scrut ((\(p × b) -> p × implicitNone b) <$> branches)
implicitNone (S.DefRec rs) =
   S.DefRec
      ( ( \(name × S.Clause r (ps × body)) ->
             name × S.Clause r (ps × addReturnNone r (implicitNone body))
        ) <$> rs
      )
   where
   addReturnNone :: TyResult -> S.Stmt TyResult -> S.Stmt TyResult
   addReturnNone Returns body = body
   addReturnNone (Assigns _) body = S.Seq body (S.Return (S.Constr Returns cNone Nil))

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
   branchCaptures (_ × S.Clause _ (ps × body)) =
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
-- checkDA Γ s computes TyResult for s under context Γ, and verifies every
-- variable reference is tt-bound. Returns | Assigns Δ.

-- checkDA validates and produces an annotated AST. The annotation type
-- TyResult is meaningful at Clause sites (each carries its body's TyResult);
-- elsewhere it's a sentinel.
checkDA :: forall m a. MonadError Error m => Ctx -> S.Stmt a -> m (TyResult × S.Stmt TyResult)
checkDA _ S.Pass = pure (Assigns Map.empty × S.Pass)
checkDA γ (S.Return e) = do
   checkExprDA γ e
   pure (Returns × S.Return (assignsEmpty <$ e))
checkDA γ (S.ExprStmt e) = do
   checkExprDA γ e
   pure (Assigns Map.empty × S.ExprStmt (assignsEmpty <$ e))
checkDA γ (S.Assert cond msg) = do
   checkExprDA γ cond
   case msg of
      Just m -> checkExprDA γ m
      Nothing -> pure unit
   pure (Assigns Map.empty × S.Assert (assignsEmpty <$ cond) ((assignsEmpty <$ _) <$> msg))
checkDA γ (S.Def (S.VarDef p e)) = do
   let assigned = bv p
   let bad = assigned `Set.intersection` capturesE e
   for_ (Set.toUnfoldable bad :: Array Var) \x ->
      throw $ "Variable captured by its own definition: " <> x
   checkExprDA γ e
   pure (Assigns (mapFromSet true assigned) × S.Def (S.VarDef p (assignsEmpty <$ e)))
checkDA γ (S.DefRec rs) = do
   let regionNames = unions (Set.singleton <<< fst <$> rs)
   let γ' = γ `overrideCtx` mapFromSet true regionNames
   rs' <- traverse
      ( \(name × S.Clause _ (ps × body)) -> do
           let params = unions (bv <$> ps)
           let locals_ff = assigns body \\ params
           let γ'' = γ' `overrideCtx` mapFromSet true params `overrideCtx` mapFromSet false locals_ff
           rBody × body' <- checkDA γ'' body
           pure (name × S.Clause rBody (ps × body'))
      )
      rs
   pure (Assigns (mapFromSet true regionNames) × S.DefRec rs')
checkDA γ (S.Seq s1 s2) = do
   r1 × s1' <- checkDA γ s1
   case r1 of
      Returns -> throw "Unreachable statement"
      Assigns δ -> do
         let bad = captures s1 `Set.intersection` assigns s2
         for_ (Set.toUnfoldable bad :: Array Var) \x ->
            throw $ "Captured variable reassigned: " <> x
         r2 × s2' <- checkDA (γ `overrideCtx` δ) s2
         pure (overrideRes r1 r2 × S.Seq s1' s2')
checkDA γ (S.If clauses elseBody) = do
   clauses' <- traverse
      ( \(cond × body) -> do
           checkExprDA γ cond
           r × body' <- checkDA γ body
           pure (r × ((assignsEmpty <$ cond) × body'))
      )
      clauses
   elseR × elseBody' <- checkDA γ elseBody
   let merged = foldl mergeRes elseR (fst <$> clauses')
   pure (merged × S.If (snd <$> clauses') elseBody')
checkDA γ (S.Match scrut branches) = do
   checkExprDA γ scrut
   branches' <- traverse
      ( \(p × body) -> do
           let pBindings = bv p
           let γ' = γ `overrideCtx` mapFromSet true pBindings
           r × body' <- checkDA γ' body
           -- pattern bindings are branch-local; strip from result
           pure (stripVars pBindings r × (p × body'))
      )
      branches
   -- Merge with Assigns ∅ representing the implicit no-matching-case fall-
   -- through, mirroring how if-no-else merges with an implicit Pass else.
   -- DA does not reason about exhaustiveness; a static totality check belongs
   -- in PurePy once it gains match.
   let merged = mergeRes (foldl mergeRes (head (fst <$> branches')) (tail (fst <$> branches'))) (Assigns Map.empty)
   pure (merged × S.Match (assignsEmpty <$ scrut) (snd <$> branches'))

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

stripVars :: Set Var -> TyResult -> TyResult
stripVars _ Returns = Returns
stripVars vars (Assigns δ) = Assigns (foldl (flip Map.delete) δ vars)
