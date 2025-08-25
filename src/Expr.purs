module Expr where

import Prelude hiding (absurd, top)

import Bind (Var)
import Control.Apply (lift2)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl, foldrDefault, foldMapDefaultL)
import Data.List (List(..), (:), zipWith)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set, empty, unions)
import Data.Set (fromFoldable) as S
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (snd)
import DataType (Ctr)
import Dict (Dict)
import Doc (DocOpt(..), DocCommentElem(..)) as Doc
import Graph (class TypeName, class Vertices, DVertex'(..), Vertex, pack, vertices)
import Lattice (class BoundedJoinSemilattice, class Expandable, class JoinSemilattice, class MeetSemilattice, Raw, expand, (∧), (∨))
import Util (type (+), type (×), error, shapeMismatch, singleton, (×), (≜))
import Util.Map (keys, asMaplet)
import Util.Pair (Pair(..))
import Util.Set ((\\), (∪))

-- Deviate from POPL paper by having closures depend on originating lambda or letrec
data Expr a
   = Var Var
   | Op Var
   | Int a Int
   | Float a Number
   | Str a String
   | Dictionary a (DocOpt a) (List (Pair (Expr a))) -- constructor name Dict borks (import of same name)
   | Constr a (DocOpt a) Ctr (List (Expr a))
   | Matrix a (DocOpt a) (Expr a) (Var × Var) (Expr a)
   | Lambda a (Elim a)
   | Project (DocOpt a) (Expr a) Var
   | DProject (DocOpt a) (Expr a) (Expr a)
   | App (DocOpt a) (Expr a) (Expr a)
   | Let (VarDef a) (Expr a)
   | LetRec (RecDefs a) (Expr a)
   | DocExpr (DocOpt a) (Expr a)

-- eliminator here is a singleton with null terminal continuation
data VarDef a = VarDef (Elim a) (Expr a)
data RecDefs a = RecDefs a (Dict (Elim a))

data Elim a
   = ElimVar Var (Cont a)
   | ElimConstr (Dict (Cont a))
   | ElimDict (Set Var) (Cont a)

-- Continuation of an eliminator branch.
data Cont a
   = ContExpr (Expr a)
   | ContElim (Elim a)

type DocOpt a = Doc.DocOpt Expr a
type DocCommentElem a = Doc.DocCommentElem Expr a

asElim :: forall a. Cont a -> Elim a
asElim (ContElim σ) = σ
asElim _ = error "Eliminator expected"

asExpr :: forall a. Cont a -> Expr a
asExpr (ContExpr e) = e
asExpr _ = error "Expression expected"

newtype Module a = Module (List (VarDef a + RecDefs a))

class FV a where
   fv :: a -> Set Var

instance FV (Doc.DocOpt Expr a) where
   fv Doc.None = empty
   fv (Doc.Doc doc) = unions (fv <$> doc)

instance FV (Expr a) where
   fv (Var x) = singleton x
   fv (Op op) = singleton op
   fv (Int _ _) = empty
   fv (Float _ _) = empty
   fv (Str _ _) = empty
   fv (Dictionary _ doc ees) = fv doc ∪ unions ((\(Pair e e') -> fv e ∪ fv e') <$> ees)
   fv (Constr _ doc _ es) = fv doc ∪ unions (fv <$> es)
   fv (Matrix _ doc e1 _ e2) = fv doc ∪ fv e1 ∪ fv e2
   fv (Lambda _ σ) = fv σ
   fv (Project doc e _) = fv doc ∪ fv e
   fv (DProject doc e x) = fv doc ∪ fv e ∪ fv x
   fv (App doc e1 e2) = fv doc ∪ fv e1 ∪ fv e2
   fv (Let def e) = fv def ∪ (fv e \\ bv def)
   fv (LetRec ρ e) = fv ρ ∪ fv e
   fv (DocExpr doc e') = fv doc ∪ fv e'

instance FV (Elim a) where
   fv (ElimVar x κ) = fv κ \\ singleton x
   fv (ElimConstr m) = unions (fv <$> m)
   fv (ElimDict _ κ) = fv κ

instance FV (Cont a) where
   fv (ContElim σ) = fv σ
   fv (ContExpr e) = fv e

instance FV (VarDef a) where
   fv (VarDef _ e) = fv e

instance FV (RecDefs a) where
   fv (RecDefs _ ρ) = fv ρ

instance FV a => FV (Dict a) where
   fv ρ = unions (fv <$> ρ) \\ S.fromFoldable (keys ρ)

instance (FV a, FV b) => FV (a × b) where
   fv (x × y) = fv x ∪ fv y

instance FV a => FV (Maybe a) where
   fv Nothing = empty
   fv (Just x) = fv x

instance (FV a) => FV (List a) where
   fv xs = unions (fv <$> xs)

instance FV (DocCommentElem a) where
   fv (Doc.Token _) = empty
   fv (Doc.Unquote e) = fv e

class BV a where
   bv :: a -> Set Var

-- Bound variables, defined only for singleton eliminators.
instance BV (Elim a) where
   bv (ElimVar x κ) = singleton x ∪ bv κ
   bv (ElimConstr m) = bv (snd (asMaplet m))
   bv (ElimDict _ κ) = bv κ

instance BV (VarDef a) where
   bv (VarDef σ _) = bv σ

instance BV (Cont a) where
   bv (ContElim σ) = bv σ
   bv (ContExpr _) = empty

instance JoinSemilattice a => JoinSemilattice (Elim a) where
   join (ElimVar x κ) (ElimVar x' κ') = ElimVar (x ≜ x') (κ ∨ κ')
   join (ElimConstr cκs) (ElimConstr cκs') = ElimConstr (cκs ∨ cκs')
   join (ElimDict xs κ) (ElimDict ys κ') = ElimDict (xs ≜ ys) (κ ∨ κ')
   join _ _ = shapeMismatch unit

instance BoundedJoinSemilattice a => Expandable (Elim a) (Raw Elim) where
   expand (ElimVar x κ) (ElimVar x' κ') = ElimVar (x ≜ x') (expand κ κ')
   expand (ElimConstr cκs) (ElimConstr cκs') = ElimConstr (expand cκs cκs')
   expand (ElimDict xs κ) (ElimDict ys κ') = ElimDict (xs ≜ ys) (expand κ κ')
   expand _ _ = shapeMismatch unit

instance JoinSemilattice a => JoinSemilattice (Cont a) where
   join (ContExpr e) (ContExpr e') = ContExpr (e ∨ e')
   join (ContElim σ) (ContElim σ') = ContElim (σ ∨ σ')
   join _ _ = shapeMismatch unit

instance BoundedJoinSemilattice a => Expandable (Cont a) (Raw Cont) where
   expand (ContExpr e) (ContExpr e') = ContExpr (expand e e')
   expand (ContElim σ) (ContElim σ') = ContElim (expand σ σ')
   expand _ _ = shapeMismatch unit

instance JoinSemilattice a => JoinSemilattice (VarDef a) where
   join (VarDef σ e) (VarDef σ' e') = VarDef (σ ∨ σ') (e ∨ e')

instance BoundedJoinSemilattice a => Expandable (VarDef a) (Raw VarDef) where
   expand (VarDef σ e) (VarDef σ' e') = VarDef (expand σ σ') (expand e e')

instance JoinSemilattice a => JoinSemilattice (RecDefs a) where
   join (RecDefs α ρ) (RecDefs α' ρ') = RecDefs (α ∨ α') (ρ ∨ ρ')

instance BoundedJoinSemilattice a => Expandable (RecDefs a) (Raw RecDefs) where
   expand (RecDefs α ρ) (RecDefs _ ρ') = RecDefs α (expand ρ ρ')

instance JoinSemilattice a => JoinSemilattice (Expr a) where
   join (Var x) (Var x') = Var (x ≜ x')
   join (Op op) (Op op') = Op (op ≜ op')
   join (Int α n) (Int α' n') = Int (α ∨ α') (n ≜ n')
   join (Str α str) (Str α' str') = Str (α ∨ α') (str ≜ str')
   join (Float α n) (Float α' n') = Float (α ∨ α') (n ≜ n')
   join (Dictionary α doc ees) (Dictionary α' doc' ees') = Dictionary (α ∨ α') (doc ∨ doc') (ees ∨ ees')
   join (Constr α doc c es) (Constr α' doc' c' es') = Constr (α ∨ α') (doc ∨ doc') (c ≜ c') (es ∨ es') -- TODO: assert consistentWith
   join (Matrix α doc e1 (x × y) e2) (Matrix α' doc' e1' (x' × y') e2') =
      Matrix (α ∨ α') (doc ∨ doc') (e1 ∨ e1') ((x ≜ x') × (y ≜ y')) (e2 ∨ e2')
   join (Lambda α σ) (Lambda α' σ') = Lambda (α ∨ α') (σ ∨ σ')
   join (Project doc e x) (Project doc' e' x') = Project (doc ∨ doc') (e ∨ e') (x ≜ x')
   join (DProject doc e x) (DProject doc' e' x') = DProject (doc ∨ doc') (e ∨ e') (x ∨ x')
   join (App doc e1 e2) (App doc' e1' e2') = App (doc ∨ doc') (e1 ∨ e1') (e2 ∨ e2')
   join (Let def e) (Let def' e') = Let (def ∨ def') (e ∨ e')
   join (LetRec ρ e) (LetRec ρ' e') = LetRec (ρ ∨ ρ') (e ∨ e')
   join (DocExpr doc e) (DocExpr doc' e') = DocExpr (doc ∨ doc') (e ∨ e')
   join _ _ = shapeMismatch unit

instance BoundedJoinSemilattice a => Expandable (Expr a) (Raw Expr) where
   expand (Var x) (Var x') = Var (x ≜ x')
   expand (Op op) (Op op') = Op (op ≜ op')
   expand (Int α n) (Int _ n') = Int α (n ≜ n')
   expand (Str α str) (Str _ str') = Str α (str ≜ str')
   expand (Float α n) (Float _ n') = Float α (n ≜ n')
   expand (Dictionary α doc ees) (Dictionary _ doc' ees') = Dictionary α (expand doc doc') (expand ees ees')
   expand (Constr α doc c es) (Constr _ doc' c' es') = Constr α (expand doc doc') (c ≜ c') (expand es es')
   expand (Matrix α doc e1 (x × y) e2) (Matrix _ doc' e1' (x' × y') e2') =
      Matrix α (expand doc doc') (expand e1 e1') ((x ≜ x') × (y ≜ y')) (expand e2 e2')
   expand (Lambda α σ) (Lambda _ σ') = Lambda α (expand σ σ')
   expand (Project doc e x) (Project doc' e' x') = Project (expand doc doc') (expand e e') (x ≜ x')
   expand (DProject doc e x) (DProject doc' e' x') = DProject (expand doc doc') (expand e e') (expand x x')
   expand (App doc e1 e2) (App doc' e1' e2') = App (expand doc doc') (expand e1 e1') (expand e2 e2')
   expand (Let def e) (Let def' e') = Let (expand def def') (expand e e')
   expand (LetRec ρ e) (LetRec ρ' e') = LetRec (expand ρ ρ') (expand e e')
   expand (DocExpr doc e) (DocExpr doc' e') = DocExpr (expand doc doc') (expand e e')
   expand _ _ = shapeMismatch unit

instance MeetSemilattice a => MeetSemilattice (Expr a) where
   meet = lift2 (∧)

instance Vertices (Expr Vertex) where
   vertices (Var _) = empty
   vertices (Op _) = empty
   vertices e@(Int α _) = singleton (DVertex (α × pack e))
   vertices e@(Float α _) = singleton (DVertex (α × pack e))
   vertices e@(Str α _) = singleton (DVertex (α × pack e))
   vertices d@(Dictionary α doc ees) = singleton (DVertex (α × pack d)) ∪ unions (go <$> ees) ∪ vertices doc
      where
      go (Pair e e') = vertices e ∪ vertices e'
   vertices e@(Constr α doc _ es) = singleton (DVertex (α × pack e)) ∪ unions (vertices <$> es) ∪ vertices doc
   vertices e@(Matrix α doc e1 _ e2) = singleton (DVertex (α × pack e)) ∪ vertices e1 ∪ vertices e2 ∪ vertices doc
   vertices e@(Lambda α σ) = singleton (DVertex (α × pack e)) ∪ vertices σ
   vertices (Project doc e _) = vertices doc ∪ vertices e
   vertices (DProject doc e x) = vertices e ∪ vertices x ∪ vertices doc
   vertices (App doc e1 e2) = vertices e1 ∪ vertices e2 ∪ vertices doc
   vertices (Let def e) = vertices def ∪ vertices e
   vertices (LetRec ρ e) = vertices ρ ∪ vertices e
   vertices (DocExpr doc e') = vertices doc ∪ vertices e'

instance Vertices (Elim Vertex) where
   vertices (ElimVar _ κ) = vertices κ
   vertices (ElimConstr m) = vertices m
   vertices (ElimDict _ κ) = vertices κ

instance Vertices (VarDef Vertex) where
   vertices (VarDef σ e) = vertices σ ∪ vertices e

instance Vertices (Cont Vertex) where
   vertices (ContExpr e) = vertices e
   vertices (ContElim σ) = vertices σ

instance Vertices (RecDefs Vertex) where
   vertices defs@(RecDefs α ρ) = singleton (DVertex (α × pack defs)) ∪ vertices ρ

instance Vertices (Module Vertex) where
   vertices (Module defs) = unions (go <$> defs)
      where
      go (Left vardef) = vertices vardef
      go (Right recdefs) = vertices recdefs

-- ======================
-- boilerplate
-- ======================
derive instance Functor VarDef
derive instance Foldable VarDef
derive instance Traversable VarDef
derive instance Functor Cont
derive instance Foldable Cont
derive instance Traversable Cont
derive instance Functor Elim
derive instance Foldable Elim
derive instance Traversable Elim
derive instance Functor Expr
derive instance Foldable Expr
derive instance Traversable Expr
derive instance Functor RecDefs
derive instance Foldable RecDefs
derive instance Traversable RecDefs
derive instance Newtype (Module a) _
derive instance Functor Module

-- For terms of a fixed shape.
instance Apply Expr where
   apply (Var x) (Var x') = Var (x ≜ x')
   apply (Op op) (Op _) = Op op
   apply (Int fα n) (Int α n') = Int (fα α) (n ≜ n')
   apply (Float fα n) (Float α n') = Float (fα α) (n ≜ n')
   apply (Str fα s) (Str α s') = Str (fα α) (s ≜ s')
   apply (Dictionary fα fdoc fxes) (Dictionary α doc xes) = Dictionary (fα α) (fdoc <*> doc) (zipWith (lift2 (<*>)) fxes xes)
   apply (Constr fα fdoc c fes) (Constr α doc c' es) = Constr (fα α) (fdoc <*> doc) (c ≜ c') (zipWith (<*>) fes es)
   apply (Matrix fα fdoc fe1 (x × y) fe2) (Matrix α doc e1 (x' × y') e2) =
      Matrix (fα α) (fdoc <*> doc) (fe1 <*> e1) ((x ≜ x') × (y ≜ y')) (fe2 <*> e2)
   apply (Lambda fα fσ) (Lambda α σ) = Lambda (fα α) (fσ <*> σ)
   apply (Project fdoc fe x) (Project doc e _) = Project (fdoc <*> doc) (fe <*> e) x
   apply (App fdoc fe1 fe2) (App doc e1 e2) = App (fdoc <*> doc) (fe1 <*> e1) (fe2 <*> e2)
   apply (Let (VarDef fσ fe1) fe2) (Let (VarDef σ e1) e2) = Let (VarDef (fσ <*> σ) (fe1 <*> e1)) (fe2 <*> e2)
   apply (LetRec fρ fe) (LetRec ρ e) = LetRec (fρ <*> ρ) (fe <*> e)
   apply (DProject fdoc fd fk) (DProject doc d k) = DProject (fdoc <*> doc) (fd <*> d) (fk <*> k)
   apply (DocExpr fdoc fe) (DocExpr doc e) = DocExpr (fdoc <*> doc) (fe <*> e)
   apply _ _ = shapeMismatch unit

instance Apply Elim where
   apply (ElimVar x fk) (ElimVar _ k) = ElimVar x (fk <*> k)
   apply (ElimConstr fk) (ElimConstr k) = ElimConstr (((<*>) <$> fk) <*> k)
   apply (ElimDict xs fk) (ElimDict _ k) = ElimDict xs (fk <*> k)
   apply _ _ = shapeMismatch unit

instance Apply Cont where
   apply (ContExpr f) (ContExpr e) = ContExpr (f <*> e)
   apply (ContElim fσ) (ContElim σ) = ContElim (fσ <*> σ)
   apply _ _ = shapeMismatch unit

instance Apply VarDef where
   apply (VarDef fσ fe) (VarDef σ e) = VarDef (fσ <*> σ) (fe <*> e)

instance Apply RecDefs where
   apply (RecDefs fα fρ) (RecDefs α ρ) = RecDefs (fα α) (((<*>) <$> fρ) <*> ρ)

-- Apply instance for Either no good here as doesn't assume fixed shape.
instance Apply Module where
   apply (Module Nil) (Module Nil) = Module Nil
   apply (Module (Left fdef : fdefs)) (Module (Left def : defs)) =
      Module (Left (fdef <*> def) : unwrap (apply (Module fdefs) (Module defs)))
   apply (Module (Right fdef : fdefs)) (Module (Right def : defs)) =
      Module (Right (fdef <*> def) : unwrap (apply (Module fdefs) (Module defs)))
   apply _ _ = shapeMismatch unit

-- Foldable instance for Either only considers Right case.
foldlModuleDef :: forall a b. (b -> a -> b) -> b -> VarDef a + RecDefs a -> b
foldlModuleDef f acc (Left def) = foldl f acc def
foldlModuleDef f acc (Right def) = foldl f acc def

instance Foldable Module where
   foldl _ acc (Module Nil) = acc
   foldl f acc (Module (Left def : defs)) =
      foldl (foldlModuleDef f) (foldl f acc def) defs
   foldl f acc (Module (Right def : defs)) =
      foldl (foldlModuleDef f) (foldl f acc def) defs

   foldr f = foldrDefault f
   foldMap f = foldMapDefaultL f

instance Traversable Module where
   traverse _ (Module Nil) = pure (Module Nil)
   traverse f (Module (Left def : ds)) =
      Module <$> ((Left <$> traverse f def) `lift2 (:)` (unwrap <$> traverse f (Module ds)))
   traverse f (Module (Right def : ds)) =
      Module <$> ((Right <$> traverse f def) `lift2 (:)` (unwrap <$> traverse f (Module ds)))

   sequence = sequenceDefault

derive instance Eq a => Eq (Expr a)
derive instance Eq a => Eq (Elim a)
derive instance Eq a => Eq (Cont a)
derive instance Eq a => Eq (VarDef a)
derive instance Eq a => Eq (RecDefs a)

derive instance Ord a => Ord (Expr a)
derive instance Ord a => Ord (Elim a)
derive instance Ord a => Ord (Cont a)
derive instance Ord a => Ord (VarDef a)
derive instance Ord a => Ord (RecDefs a)

instance TypeName (RecDefs a) where
   typeName _ = "RecDefs"

instance TypeName (Expr a) where
   typeName _ = "Expr"
