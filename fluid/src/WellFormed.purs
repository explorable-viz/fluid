module WellFormed where

import Prelude

import Bind (Var)
import Control.Monad.Error.Class (class MonadError)
import Data.Either (Either(..))
import Data.Foldable (foldl, for_)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.List.NonEmpty as NEL
import Data.Semigroup.Foldable (foldl1)
import Data.Set (Set, unions)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple (fst, snd)
import DataType (cNone)
import DefiniteAssignment (Ctx, TyResult(..), assignsEmpty, fromSet, mergeRes, overrideCtx, overrideRes)
import Effect.Exception (Error)
import Expr (bv, fv)
import Expr (Module(..), RecDefs(..)) as E
import Lattice (Raw)
import SExpr (Clause(..), DictEntry(..), Expr(..), LambdaClause(..), ListRest(..), Module, ParagraphElem(..), Stmt(..), VarDef(..)) as S
import Util (type (×), throw, (×))
import Util.Map (keys)
import Util.Set ((\\), (∪))
import Val (Env)

checkProgram :: forall m. MonadError Error m => Set Var -> Raw S.Stmt -> m (S.Stmt TyResult)
checkProgram γ0 s = do
   _ × s' <- checkDA (fromSet true γ0) s
   pure (implicitNone s')

implicitNone :: S.Stmt TyResult -> S.Stmt TyResult
implicitNone S.Pass = S.Pass
implicitNone (S.Return e) = S.Return e
implicitNone (S.ExprStmt e) = S.ExprStmt e
implicitNone (S.Assert e e') = S.Assert e e'
implicitNone (S.Def d) = S.Def d
implicitNone (S.Seq s1 s2) = S.Seq (implicitNone s1) (implicitNone s2)
implicitNone (S.If es s) = S.If ((\(e × s') -> e × implicitNone s') <$> es) (implicitNone s)
implicitNone (S.Match e ps) = S.Match e ((\(p × s) -> p × implicitNone s) <$> ps)
implicitNone (S.DefRec ds) =
   S.DefRec ((\(x × S.Clause r (ps × s)) -> x × S.Clause r (ps × close r (implicitNone s))) <$> ds)
   where
   close Returns s = s
   close (Assigns _) s = S.Seq s (S.Return (S.Constr Returns cNone Nil))

checkModule :: forall m. MonadError Error m => Raw S.Module -> m Unit
checkModule _ = pure unit

moduleExports :: forall a. E.Module a -> Set Var
moduleExports (E.Module ds) = unions (defNames <$> ds)
   where
   defNames (Left d) = bv d
   defNames (Right (E.RecDefs _ ρ)) = keys ρ

envNames :: forall a. Env a -> Set Var
envNames = keys

assigns :: forall a. S.Stmt a -> Set Var
assigns S.Pass = Set.empty
assigns (S.Def (S.VarDef p _)) = bv p
assigns (S.ExprStmt _) = Set.empty
assigns (S.Assert _ _) = Set.empty
assigns (S.Return _) = Set.empty
assigns (S.If es s) = unions (assigns <$> (snd <$> es)) ∪ assigns s
assigns (S.Match _ ps) = unions (assigns <$> (snd <$> ps))
assigns (S.DefRec ds) = unions (Set.singleton <<< fst <$> ds)
assigns (S.Seq s1 s2) = assigns s1 ∪ assigns s2

captures :: forall a. S.Stmt a -> Set Var
captures S.Pass = Set.empty
captures (S.Def (S.VarDef _ e)) = capturesE e
captures (S.ExprStmt e) = capturesE e
captures (S.Assert e e') = capturesE e ∪ maybe Set.empty capturesE e'
captures (S.Return e) = capturesE e
captures (S.If es s) =
   unions ((\(e × s') -> capturesE e ∪ captures s') <$> es) ∪ captures s
captures (S.Match e ps) =
   capturesE e ∪ unions ((\(_ × s) -> captures s) <$> ps)
captures (S.DefRec ds) =
   (unions (clauseCaptures <$> ds)) \\ unions (Set.singleton <<< fst <$> ds)
   where
   clauseCaptures (_ × S.Clause _ (ps × s)) =
      (fv s \\ unions (bv <$> ps)) \\ assigns s
captures (S.Seq s1 s2) = captures s1 ∪ captures s2

capturesE :: forall a. S.Expr a -> Set Var
capturesE (S.Var _) = Set.empty
capturesE (S.Op _) = Set.empty
capturesE (S.Int _ _) = Set.empty
capturesE (S.Float _ _) = Set.empty
capturesE (S.Str _ _) = Set.empty
capturesE (S.Constr _ _ es) = unions (capturesE <$> es)
capturesE (S.Dictionary _ es) =
   unions ((\(k × v) -> capturesEntry k ∪ capturesE v) <$> es)
   where
   capturesEntry (S.ExprKey e) = capturesE e
   capturesEntry (S.VarKey _ _) = Set.empty
capturesE (S.Matrix _ e (x × y) e') =
   (capturesE e \\ (Set.singleton x ∪ Set.singleton y)) ∪ capturesE e'
capturesE (S.Lambda (S.LambdaClause (ps × e))) =
   fv e \\ unions (bv <$> ps)
capturesE (S.Project e _) = capturesE e
capturesE (S.DProject e e') = capturesE e ∪ capturesE e'
capturesE (S.App e e') = capturesE e ∪ capturesE e'
capturesE (S.BinaryApp e _ e') = capturesE e ∪ capturesE e'
capturesE (S.UnaryPrefixApp _ e) = capturesE e
capturesE (S.Ternary e e1 e2) = capturesE e ∪ capturesE e1 ∪ capturesE e2
capturesE (S.Paragraph es) = unions (capturesPe <$> es)
   where
   capturesPe (S.Token _) = Set.empty
   capturesPe (S.Unquote e) = capturesE e
capturesE (S.ListEmpty _) = Set.empty
capturesE (S.ListNonEmpty _ e l) = capturesE e ∪ capturesEListRest l
   where
   capturesEListRest (S.End _) = Set.empty
   capturesEListRest (S.Next _ e' l') = capturesE e' ∪ capturesEListRest l'
capturesE (S.ListEnum e1 e2) = capturesE e1 ∪ capturesE e2
capturesE (S.ListComp _ e _) = capturesE e
capturesE (S.DocExpr e e') = capturesE e ∪ capturesE e'

checkDA :: forall m a. MonadError Error m => Ctx -> S.Stmt a -> m (TyResult × S.Stmt TyResult)
checkDA _ S.Pass = pure (assignsEmpty × S.Pass)
checkDA γ (S.Return e) = do
   checkExprDA γ e
   pure (Returns × S.Return (assignsEmpty <$ e))
checkDA γ (S.ExprStmt e) = do
   checkExprDA γ e
   pure (assignsEmpty × S.ExprStmt (assignsEmpty <$ e))
checkDA γ (S.Assert e e') = do
   checkExprDA γ e
   for_ e' (checkExprDA γ)
   pure (assignsEmpty × S.Assert (assignsEmpty <$ e) ((assignsEmpty <$ _) <$> e'))
checkDA γ (S.Def (S.VarDef p e)) = do
   let xs = bv p
   for_ (Set.toUnfoldable (xs `Set.intersection` capturesE e) :: Array Var) \x ->
      throw $ "Variable captured by its own definition: " <> x
   checkExprDA γ e
   pure (Assigns (fromSet true xs) × S.Def (S.VarDef p (assignsEmpty <$ e)))
checkDA γ (S.DefRec ds) = do
   let fs = unions (Set.singleton <<< fst <$> ds)
   let γ' = γ `overrideCtx` fromSet true fs
   ds' <- traverse
      ( \(x × S.Clause _ (ps × s)) -> do
           let xs = unions (bv <$> ps)
           let ys = assigns s \\ xs
           let γ'' = γ' `overrideCtx` fromSet true xs `overrideCtx` fromSet false ys
           r × s' <- checkDA γ'' s
           pure (x × S.Clause r (ps × s'))
      )
      ds
   pure (Assigns (fromSet true fs) × S.DefRec ds')
checkDA γ (S.Seq s1 s2) = do
   r1 × s1' <- checkDA γ s1
   case r1 of
      Returns -> throw "Unreachable statement"
      Assigns δ -> do
         for_ (Set.toUnfoldable (captures s1 `Set.intersection` assigns s2) :: Array Var) \x ->
            throw $ "Captured variable reassigned: " <> x
         r2 × s2' <- checkDA (γ `overrideCtx` δ) s2
         pure (overrideRes r1 r2 × S.Seq s1' s2')
checkDA γ (S.If es s) = do
   es' <- traverse
      ( \(e × s') -> do
           checkExprDA γ e
           r × s'' <- checkDA γ s'
           pure (r × ((assignsEmpty <$ e) × s''))
      )
      es
   r × s' <- checkDA γ s
   pure (foldl1 mergeRes (NEL.cons r (fst <$> es')) × S.If (snd <$> es') s')
checkDA γ (S.Match e ps) = do
   checkExprDA γ e
   ps' <- traverse
      ( \(p × s) -> do
           let xs = bv p
           r × s' <- checkDA (γ `overrideCtx` fromSet true xs) s
           pure (stripVars xs r × (p × s'))
      )
      ps
   pure (mergeRes (foldl1 mergeRes (fst <$> ps')) assignsEmpty × S.Match (assignsEmpty <$ e) (snd <$> ps'))

checkExprDA :: forall m a. MonadError Error m => Ctx -> S.Expr a -> m Unit
checkExprDA γ e =
   for_ (fv e) \x -> case Map.lookup x γ of
      Just true -> pure unit
      Just false -> throw $ "Not definitely assigned: " <> x
      Nothing -> throw $ "Unbound name: " <> x

stripVars :: Set Var -> TyResult -> TyResult
stripVars _ Returns = Returns
stripVars xs (Assigns δ) = Assigns (foldl (flip Map.delete) δ xs)
