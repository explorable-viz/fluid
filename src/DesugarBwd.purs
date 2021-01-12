module DesugarBwd where

import Prelude hiding (absurd)
import Data.Function (on)
import Data.List (List(..), (:), zip)
import Data.List.NonEmpty (NonEmptyList(..), groupBy, toList, appendFoldable, reverse)
import Data.Map (fromFoldable)
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Data.Tuple (uncurry, fst, snd)
import Bindings (Binding, Bindings(..), (↦), (:+:))
import DataType (cPair, cCons, cNil, cTrue, cFalse)
import Expr (Cont(..), Elim(..))
import Expr (Expr(..), RawExpr(..), VarDef(..)) as E
import Pretty (render, pretty)
import SExpr (Expr(..), ListPatternRest(..), ListRest(..), Pattern(..), Qualifier(..), RawExpr(..), VarDef(..))
import Lattice (𝔹, (∧))
import Util (MayFail, type (×), (×), (≞), (≜), absurd, mustLookup, lookupE, error)

qualTrue :: 𝔹 -> Qualifier 𝔹
qualTrue α = (Guard α (Expr α (Constr cTrue Nil)))

snil :: 𝔹 -> Expr 𝔹
snil α = Expr α $ Constr cNil Nil

class DesugarBwd a b where
   desugarBwd :: a -> b -> MayFail b

-- data VarDef a = VarDef (Elim a) (Expr a)
instance desugarBwdVarDef  :: DesugarBwd (E.VarDef Boolean) (VarDef Boolean) where
   desugarBwd (E.VarDef σ e) (VarDef π s) = VarDef π <$> desugarBwd e s

instance desugarBwdVarDefs :: DesugarBwd (E.Expr Boolean)
                                         (Boolean × (NonEmptyList (VarDef Boolean) × Expr Boolean)) where
   desugarBwd (E.Expr α1 (E.Let (E.VarDef σ e1@(E.Expr α2 _)) e2@(E.Expr α3 _)))
              (_ × (NonEmptyList (VarDef π s1 :| Nil) × s2)) = do
              s1' <- desugarBwd e1 s1
              s2' <- desugarBwd e2 s2
              pure $ (α1 ∧ α2 ∧ α3) × (NonEmptyList (VarDef π s1' :| Nil) × s2')
   desugarBwd (E.Expr α1 (E.Let (E.VarDef σ e1@(E.Expr α2 _)) e2@(E.Expr α3 _)))
              (_ × (NonEmptyList (VarDef π s1 :| d : ds) × s2)) = do
              s1' <- desugarBwd e1 s1
              α4 × (NonEmptyList (d' :| ds') × s2') <- desugarBwd e2 ((α1 ∧ α2 ∧ α3) × (NonEmptyList (d :| ds) × s2))
              pure $ (α1 ∧ α2 ∧ α3 ∧ α4) × (NonEmptyList (VarDef π s1' :| d' : ds') × s2')
   desugarBwd e (_ × (vardefs × s_body)) =
      error $ "Desugar var defs (e, vardefs × s_body) match not found: \n" <>
              render (pretty e) <> "\n" <>
              render (pretty vardefs) <> " × " <> render (pretty s_body)

concatNonEmpty :: forall a. NonEmptyList (NonEmptyList a) -> NonEmptyList a
concatNonEmpty (NonEmptyList (x :| x' : xs)) = appendFoldable x (concatNonEmpty (NonEmptyList (x' :| xs)))
concatNonEmpty (NonEmptyList (x :| Nil))     = x


{-                 →                        →           -}
{-                                            →         -}
{- let [f ↦ σ] ↘ (f c)         [f ↦ σ] ↘ (f, (p, s))    -}
instance desugarBwdRecDefs :: DesugarBwd (Bindings Elim Boolean)
                                         (NonEmptyList (String × ((NonEmptyList Pattern) × (Expr Boolean)))) where
   desugarBwd fσs fπes = concatNonEmpty <$> zipRecDefs fσs fπess --error "Desugar bwd for RecDefs not implemented"

      where

         fπess  = reverse $ (groupBy (eq `on` fst) fπes :: NonEmptyList (NonEmptyList (String × ((NonEmptyList Pattern) × (Expr Boolean)))))

         -- f a -> g b -> (a -> b -> b) -> (g b)
         zipRecDefs :: Bindings Elim 𝔹
                    -> NonEmptyList (NonEmptyList (String × ((NonEmptyList Pattern) × (Expr 𝔹))))
                    -> MayFail (NonEmptyList (NonEmptyList (String × ((NonEmptyList Pattern) × (Expr 𝔹)))))
         zipRecDefs (ρ :+: f ↦ σ) (NonEmptyList (fπes1 :| fπes2 : fπes_rest)) = do
            fπes1' <- fromRecDef (f ↦ σ) fπes1
            fπess' <- toList <$> zipRecDefs ρ (NonEmptyList (fπes2 :| fπes_rest))
            pure $ NonEmptyList (fπes1' :| fπess')
         zipRecDefs (Empty :+: f ↦ σ) (NonEmptyList (fπes1 :| Nil)) = do
            fπes1'  <- fromRecDef (f ↦ σ) fπes1
            pure $ NonEmptyList (fπes1' :| Nil)
         zipRecDefs ρ fπs = error $ "zipRecDefs error: recdefs (ρ, fπs) have different sizes \n" <>
                                    render (pretty ρ) <> "\n" <>
                                    render (pretty fπs)
         -- backward slice the eliminator (containing different possible pattern matches of the f)
         -- and the set of branches (for each pattern match of f)
         --          →       →
         -- f ↦ σ, (f c) ↘ (f c)
         fromRecDef :: Binding Elim 𝔹
                    -> NonEmptyList (String × (NonEmptyList Pattern × Expr 𝔹))
                    -> MayFail (NonEmptyList (String × (NonEmptyList Pattern × Expr 𝔹)))
         fromRecDef (f ↦ σ) fπs@(NonEmptyList ((f' × (πs × e)) :| fπs')) =
            map ((×) f) <$> desugarBwd σ (snd <$> fπs)

instance desugarBwdExpr :: DesugarBwd (E.Expr Boolean) (Expr Boolean) where
   desugarBwd (E.Expr α (E.Var x))   (Expr _ (Var x'))      = pure $ Expr α (Var (x ≜ x'))
   desugarBwd (E.Expr α (E.Op op))   (Expr _ (Op op'))      = pure $ Expr α (Op (op ≜ op'))
   desugarBwd (E.Expr α (E.Int n))   (Expr _ (Int n'))      = pure $ Expr α (Int (n ≜ n'))
   desugarBwd (E.Expr α (E.Float n)) (Expr _ (Float n'))    = pure $ Expr α (Float (n ≜ n'))
   desugarBwd (E.Expr α (E.Str s))   (Expr _ (Str s'))      = pure $ Expr α (Str (s ≜ s'))
   -- | Constr (this covers Cons)
   desugarBwd (E.Expr α (E.Constr ctr args)) (Expr _ (Constr ctr' args')) =
      Expr α <$> (Constr ctr <$> traverse (uncurry desugarBwd) (zip args args'))
   -- | Lambda
   desugarBwd (E.Expr α (E.Lambda σ)) (Expr _ (Lambda bs))= Expr α <$> (Lambda <$> desugarBwd σ bs)
   -- | Application
   desugarBwd (E.Expr α (E.App e1 e2)) (Expr _ (App s1 s2)) =
      Expr α <$> (App <$> desugarBwd e1 s1 <*> desugarBwd e2 s2)
   desugarBwd (E.Expr α (E.BinaryApp e1 x e2)) (Expr _ (BinaryApp s1 x' s2)) =
      Expr α <$> (BinaryApp <$> desugarBwd e1 s1 <@> x ≜ x' <*> desugarBwd e2 s2)
   -- | Match-as
   desugarBwd (E.Expr α2 (E.App (E.Expr α1 (E.Lambda σ)) e))
              (Expr _ (MatchAs s bs)) =
      Expr (α1 ∧ α2) <$> (MatchAs <$> desugarBwd e s <*> desugarBwd σ bs)
   -- | If-then-else
   desugarBwd (E.Expr α2 (E.App (E.Expr α1 (E.Lambda (ElimConstr m))) e1))
              (Expr _ (IfElse s1 s2 s3)) = do
      e2 <- liftM1 asExpr $ lookupE cTrue m
      e3 <- liftM1 asExpr $ lookupE cFalse m
      Expr (α1 ∧ α2) <$> (IfElse <$> desugarBwd e1 s1 <*> desugarBwd e2 s2 <*> desugarBwd e3 s3)
   -- | Empty-list
   desugarBwd (E.Expr α (E.Constr c Nil)) (Expr _ ListEmpty) | c == cNil =
      pure $ Expr α ListEmpty
   -- | Non-empty-list
   desugarBwd (E.Expr α (E.Constr c (e : e' : Nil))) (Expr _ (ListNonEmpty s l)) | c == cCons =
      Expr α <$> (ListNonEmpty <$> desugarBwd e s <*> desugarBwd e' l)
   -- | List-enum
   desugarBwd (E.Expr α2 (E.App (E.Expr α1 (E.App (E.Expr _ (E.Var "enumFromTo")) e1)) e2))
              (Expr α (ListEnum s1 s2)) =
      Expr (α1 ∧ α2) <$> (ListEnum <$> desugarBwd e1 s1 <*> desugarBwd e2 s2)
   -- | List-comp-done
   desugarBwd (E.Expr α2 (E.Constr c (e : (E.Expr α1 (E.Constr c' Nil)) : Nil)))
              (Expr _ (ListComp s_body (NonEmptyList (Guard _ (Expr _ (Constr c'' Nil)) :| Nil))))
      | c == cCons , c' == cNil, c'' == cTrue =
      Expr (α1 ∧ α2) <$> (ListComp <$> desugarBwd e s_body <*> (pure $ NonEmptyList (Guard (α1 ∧ α2) (Expr (α1 ∧ α2) (Constr cTrue Nil)) :| Nil)))
   -- | List-comp-qual
   desugarBwd e
              (Expr α (ListComp s_body (NonEmptyList (q :| Nil)))) = do
      sListComp <- desugarBwd e (Expr α (ListComp s_body (NonEmptyList (q :| (qualTrue true) : Nil))))
      case sListComp of
         Expr α2 (ListComp s_body' (NonEmptyList (q' :| (Guard α1 (Expr _ (Constr c Nil))) : Nil)))
         | c == cTrue
            -> pure $ Expr (α1 ∧ α2) (ListComp s_body' (NonEmptyList (q' :| Nil)))
         sListComp'
            -> error $ "desugarBwd for List-comp-qual failed: \n" <>
                       render (pretty sListComp')
   -- | List-comp-guard
   desugarBwd (E.Expr α2 (E.App (E.Expr α1 (E.Lambda (ElimConstr m))) e1))
              (Expr _ (ListComp s1 (NonEmptyList ((Guard _ s2) :| q : qs)))) = do
      e2          <- liftM1 asExpr $ lookupE cTrue  m
      e3          <- liftM1 asExpr $ lookupE cFalse m
      s2'         <- desugarBwd e1 s2
      sListComp   <- desugarBwd e2 (Expr true (ListComp s1 (NonEmptyList (q :| qs))))
      sNil        <- desugarBwd e3 (snil true)
      case sListComp, sNil of
         Expr α3 (ListComp s1' (NonEmptyList (q' :| qs'))), Expr α4 (Constr c Nil)
         | c == cNil
               -> pure $ Expr (α1 ∧ α2 ∧ α3 ∧ α4)
                              (ListComp s1' (NonEmptyList ((Guard (α1 ∧ α2 ∧ α3 ∧ α4) s2') :| q' : qs')))
         sListComp', sNil'  -> error $ "desugarBwd for List-comp-guard failed: " <>
                                       render (pretty sListComp') <> "\n" <> render (pretty sNil')
   -- | List-comp-decl
   desugarBwd (E.Expr α1 (E.App (E.Expr α2 (E.Lambda σ)) e))
              (Expr _ (ListComp s2 (NonEmptyList ((Declaration _ (VarDef π s1)) :| q : qs)))) = do
      (_ × s1') <- desugarBwd σ (NonEmptyList (π :| Nil) × s1)
      sListComp  <- desugarBwd e (Expr true (ListComp s2 (NonEmptyList (q :| qs))))
      case sListComp of
         Expr α3 (ListComp s2' (NonEmptyList (q' :| qs')))
            -> pure $ Expr (α1 ∧ α2 ∧ α3)
                           (ListComp s2' (NonEmptyList ((Declaration (α1 ∧ α2 ∧ α3) (VarDef π s1')) :| q' : qs')))
         sListComp'
            -> error $ "desugarBwd for List-comp-decl failed: \n" <>
                       render (pretty sListComp')
   -- | List-comp-gen
   desugarBwd (E.Expr α3 (E.App (E.Expr α2 (E.App (E.Expr _  (E.Var "concatMap"))
                                                  (E.Expr α1 (E.Lambda σ))))
                                 e1))
              (Expr _ (ListComp s2 (NonEmptyList ((Generator _ p s1) :| q : qs)))) = do
      s1'        <- desugarBwd e1 s1
      σ'         <- pure $ asElim $ untotalisePatt (Arg σ) p
      e2         <- liftM1 asExpr (desugarPatternBwd σ' p)
      sListComp  <- desugarBwd e2 (Expr true (ListComp s2 (NonEmptyList (q :| qs))))
      case sListComp of
         Expr α4 (ListComp s2' (NonEmptyList (q' :| qs'))) ->
            pure $ Expr (α1 ∧ α2 ∧ α3 ∧ α4)
                        (ListComp s2' (NonEmptyList ((Generator (α1 ∧ α2 ∧ α3) p s1) :| q' : qs')))
         sListComp'
            -> error $ "desugarBwd for List-comp-gen failed: \n" <>
                       render (pretty sListComp')
   -- | Let
   desugarBwd (E.Expr α (E.Let d e))
              (Expr _ (Let ds s)) = do
      α' × (ds' × s') <- desugarBwd (E.Expr α (E.Let d e)) (true × (ds × s))
      pure $ Expr α' (Let ds' s')
   -- | LetRec (recursive function)
   desugarBwd (E.Expr α (E.LetRec fπs e))
              (Expr _ (LetRec fπs' s)) =
      Expr α <$> (LetRec <$> desugarBwd fπs fπs' <*> desugarBwd e s)

   desugarBwd e s = error $ "desugarBwd match not found: " <> render (pretty e) <> "\n" <> render (pretty s)

asElim :: Cont 𝔹 -> Elim 𝔹
asElim (Arg σ) =  σ
asElim κ = error $ "Couldn't infer Elim from Cont: \n" <>
                   render (pretty κ)

asExpr :: Cont 𝔹 -> E.Expr 𝔹
asExpr (Body e) =  e
asExpr κ = error $ "Couldn't infer Expr from Cont: \n" <>
                   render (pretty κ)

{- e, l ↘ l -}
instance desugarBwdListRest :: DesugarBwd (E.Expr Boolean) (ListRest Boolean) where
   desugarBwd (E.Expr α (E.Constr c Nil)) (End _) | c == cNil =
      pure $ End α
   desugarBwd (E.Expr α (E.Constr c (e : e' : Nil))) (Next _ s l) | c == cCons =
      Next α <$> desugarBwd e s <*> desugarBwd e' l
   desugarBwd e l = error $ "desugarBwdListRest (e, l) match not found: \n" <>
                            render (pretty e) <> "\n" <>
                            render (pretty l)

class DesugarPatternBwd a where
   desugarPatternBwd :: Elim Boolean -> a -> MayFail (Cont Boolean)

{-    →     -}
{- σ, p ↘ κ -}
instance desugarPatternBwdPatterns :: DesugarPatternBwd (NonEmptyList Pattern) where
   desugarPatternBwd σ (NonEmptyList (π :| Nil)) = desugarPatternBwd σ π
   desugarPatternBwd σ (NonEmptyList (π :| π' : πs)) = do
      σ' <- liftM1 asElim $ desugarPatternBwd σ π
      desugarPatternBwd σ' (NonEmptyList (π' :| πs))

{- σ, p ↘ κ -}
instance desugarPatternBwdPattern :: DesugarPatternBwd Pattern where
   -- | Var
   desugarPatternBwd (ElimVar x κ)  (PVar x') = (x ≞ x') *> pure κ
   -- | Nil
   desugarPatternBwd (ElimConstr m) (PConstr c Nil) | c == cNil = lookupE cNil m
   -- | Cons, Pair
   desugarPatternBwd (ElimConstr m) (PConstr ctr (π:π':_))
      | ctr == cNil || ctr == cPair = do
          σ  <- liftM1 asElim $ lookupE ctr m
          σ' <- liftM1 asElim $ desugarPatternBwd σ π
          desugarPatternBwd σ' π'
   -- | Empty-list
   desugarPatternBwd (ElimConstr m) (PListEmpty) = lookupE cNil m
   -- | Non-empty-list
   desugarPatternBwd σ (PListNonEmpty π o)  = do
      σ' <- liftM1 asElim $ desugarPatternBwd σ π
      desugarPatternBwd σ' o
   desugarPatternBwd σ π = error $ "desugarPatternBwdPattern (σ, π) match not found: \n" <>
                                   render (pretty σ) <> "\n" <>
                                   render (pretty π)

{- σ, o ↘ κ -}
instance desugarPatternBwdListPatternRest :: DesugarPatternBwd ListPatternRest where
   desugarPatternBwd (ElimConstr m) PEnd        = lookupE cCons m
   desugarPatternBwd (ElimConstr m) (PNext π o) = do
      σ  <- liftM1 asElim $ lookupE cCons m
      σ' <- liftM1 asElim $ desugarPatternBwd σ π
      desugarPatternBwd σ' o
   desugarPatternBwd σ l = error $ "desugarPatternBwdListPatternRest (σ, l) match not found: \n" <>
                                   render (pretty σ) <> "\n" <>
                                   render (pretty l)
{- σ, c ↘ c -}
instance desugarBwdBranch :: DesugarBwd (Elim Boolean) (NonEmptyList Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) = do
      e  <- liftM1 asExpr $ desugarPatternBwd σ πs
      s' <- desugarBwd e s
      pure $ πs × s'

instance desugarBwdBranchUncurried :: DesugarBwd (Elim Boolean) (Pattern × Expr Boolean) where
   desugarBwd σ (πs × s) = do
      e  <- liftM1 asExpr $ desugarPatternBwd σ πs
      s' <- desugarBwd e s
      pure $ πs × s'

{-    →   → -}
{- σ, c ↘ c -}
instance desugarBwdBranches :: DesugarBwd (Elim Boolean) (NonEmptyList (NonEmptyList Pattern × Expr Boolean)) where
   desugarBwd σ (NonEmptyList (b1 :| b2 : bs)) = do
      b'  <- desugarBwd σ b1
      bs' <- desugarBwd σ (NonEmptyList (b2 :| bs))
      pure $ NonEmptyList (b' :| (toList bs'))
   desugarBwd σ (NonEmptyList (b :| Nil)) = do
      b' <- desugarBwd σ b
      pure $ NonEmptyList (b' :| Nil)

instance desugarBwdBranchesUncurried :: DesugarBwd (Elim Boolean) (NonEmptyList (Pattern × Expr Boolean)) where
   desugarBwd σ (NonEmptyList (b1 :| b2 : bs)) = do
      b'  <- desugarBwd σ b1
      bs' <- desugarBwd σ (NonEmptyList (b2 :| bs))
      pure $ NonEmptyList (b' :| (toList bs'))
   desugarBwd σ (NonEmptyList (b :| Nil)) = do
      b' <- desugarBwd σ b
      pure $ NonEmptyList (b' :| Nil)

{- untotalise κ p ↗ κ' -}
untotalisePatt :: Cont 𝔹 -> Pattern -> Cont 𝔹
untotalisePatt (Arg σ) p =
   case σ, p of
      -- | var
      ElimVar x κ, PVar x'            ->
         if x == x' then Arg (ElimVar x κ) else error absurd
      -- | true, false, pair, nil, cons
      ElimConstr m, PConstr ctr ps    ->
         let κ = mustLookup ctr m
         in  Arg $ ElimConstr (fromFoldable [ctr × untotaliseListPatt κ ps])
      -- | patt-list-empty
      ElimConstr m, PListEmpty        ->
         let κ = mustLookup cNil m
         in  Arg $ ElimConstr (fromFoldable [cNil × κ])
      -- | patt-list-non-empty
      ElimConstr m, PListNonEmpty p' o ->
         let κ = mustLookup cCons m
         in  Arg $ ElimConstr (fromFoldable [cCons × untotaliseListPattRest (untotalisePatt κ p') o])
      σ', p' -> error $ "untotalisePatt (σ, π) match not found: \n" <>
                      render (pretty σ') <> "\n" <>
                      render (pretty p')
untotalisePatt κ π = error $ "untotalisePatt (κ, π) match not found: \n" <>
                             render (pretty κ) <> "\n" <>
                             render (pretty π)

untotaliseListPatt :: Cont 𝔹 -> List Pattern -> Cont 𝔹
untotaliseListPatt κ Nil = κ
untotaliseListPatt κ (p:ps) =
   untotaliseListPatt (untotalisePatt κ p) ps

{- untotalise κ o ↗ κ' -}
untotaliseListPattRest :: Cont 𝔹 -> ListPatternRest -> Cont 𝔹
untotaliseListPattRest (Arg (ElimConstr m)) PEnd =
   let κ = mustLookup cNil m
   in  Arg $ ElimConstr (fromFoldable [cNil × κ])
untotaliseListPattRest (Arg (ElimConstr m)) (PNext p o) =
   let κ = mustLookup cCons m
   in  Arg $ ElimConstr (fromFoldable [cCons × untotaliseListPattRest (untotalisePatt κ p) o])
untotaliseListPattRest κ o = error $ "untotaliseListPattRest (κ, o) not found: \n" <>
                                     render (pretty κ) <> "\n" <>
                                     render (pretty o)