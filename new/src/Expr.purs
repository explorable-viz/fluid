module Expr where

import Prelude hiding (top)
import Data.List (List(..), (:))
import Data.List (concat) as L
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import DataType (Ctr(..))
import Lattice (class MaybeZippable, Selected, maybeZipWith, maybeZipWithList, maybeZipWithMap)
import Util (type (+), (≟), error)

type Var = String

varAnon = "_" :: Var

data VarDef' a = VarDef (Elim' a) (Expr' a) -- elim has codomain unit
type VarDef = VarDef' Selected
type VarDefs a = List (VarDef' a)

data RecDef' a = RecDef Var (Elim' a)
type RecDef = RecDef' Selected
type RecDefs' a = List (RecDef' a)
type RecDefs = RecDefs' Selected

data RawExpr' a =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List (Expr' a)) |
   Lambda (Elim' a) |
   App (Expr' a) (Expr' a) |
   BinaryApp (Expr' a) Var (Expr' a) |
   MatchAs (Expr' a) (Elim' a) |
   Let (VarDef' a) (Expr' a) |
   LetRec (RecDefs' a) (Expr' a)

data Expr' a = Expr a (RawExpr' a)

type Expr = Expr' Selected
type RawExpr = RawExpr' Selected

derive instance functorVarDef :: Functor VarDef'
derive instance functorRecDef :: Functor RecDef'
derive instance functorRawExpr :: Functor RawExpr'
derive instance functorExpr :: Functor Expr'

expr :: RawExpr -> Expr
expr = Expr false

-- Continuation of an eliminator. None form only used in structured let.
data Cont' a = None | Body (Expr' a) | Arg (Elim' a)
type Cont = Cont' Selected

body :: Cont -> Expr
body (Body e) = e
body _ = error "Expression expected"

instance selectableCont :: MaybeZippable Cont' where
   maybeZipWith f (Body e) (Body e')        = Body <$> maybeZipWith f e e'
   maybeZipWith f (Arg σ) (Arg σ')          = Arg <$> maybeZipWith f σ σ'
   maybeZipWith _ _ _                       = Nothing

data Elim' a =
   ElimVar Var (Cont' a) |
   ElimConstr (Map Ctr (Cont' a))

type Elim = Elim' Selected

derive instance functorCont :: Functor Cont'
derive instance functorElim :: Functor Elim'

instance maybeZippableElim :: MaybeZippable Elim' where
   maybeZipWith f (ElimVar x κ) (ElimVar x' κ')
      = ElimVar <$> x ≟ x' <*> maybeZipWith f κ κ'
   maybeZipWith f (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeZipWithMap f κs κs'
   maybeZipWith _ _ _                                = Nothing

data Module' a = Module (List (VarDef' a + RecDefs' a))
type Module = Module' Selected

class ToList a where
   toList :: a -> List a

class FromList a where
   fromList :: List a -> a

instance exprToList :: ToList (Expr' Boolean) where
   toList (Expr a (Constr (Ctr "Cons") (e : es : Nil))) = (e:toList es)
   toList (Expr a (Constr (Ctr "Nil") Nil)) = Nil
   toList _ = error "expected list expression"

instance exprFromList :: FromList (Expr' Boolean) where
   fromList (x : xs) = expr $ Constr (Ctr "Cons") (x: fromList xs : Nil)
   fromList Nil      = expr $ Constr (Ctr "Nil") Nil

appendE :: Expr -> Expr -> Expr
appendE e1 e2 = fromList $ (toList e1) <> (toList e2)

concatE :: Expr -> Expr
concatE e1 = fromList $ L.concat $ map toList (toList e1)

mapE :: Elim -> Expr -> Expr
mapE σ e = fromList $ map (applyσ σ) (toList e)
   where
   applyσ :: Elim -> Expr -> Expr
   applyσ σ' e' = expr $ MatchAs e' σ'

concatMapE :: Elim -> Expr -> Expr
concatMapE = mapE

instance maybeZippableDef :: MaybeZippable VarDef' where
   maybeZipWith f (VarDef σ e) (VarDef σ' e') = VarDef <$> maybeZipWith f σ σ' <*> maybeZipWith f e e'

instance maybeZippableRecDef :: MaybeZippable RecDef' where
   maybeZipWith f (RecDef x σ) (RecDef x' σ') = RecDef <$> x ≟ x' <*> maybeZipWith f σ σ'

instance maybeZippableExpr :: MaybeZippable Expr' where
   maybeZipWith f (Expr α r) (Expr α' r') = Expr <$> pure (f α α') <*> maybeZipWith f r r'

instance maybeZippableRawExpr :: MaybeZippable RawExpr' where
   maybeZipWith _ (Var x) (Var x')                = Var <$> x ≟ x'
   maybeZipWith _ (Op op) (Op op')                = Op <$> op ≟ op'
   maybeZipWith _ (Int n) (Int n')                = Int <$> n ≟ n'
   maybeZipWith _ (Str s) (Var s')                = Str <$> s ≟ s'
   maybeZipWith f (Constr c es) (Constr c' es')
      = Constr <$> c ≟ c' <*> maybeZipWithList f es es'
   maybeZipWith f (App e1 e2) (App e1' e2')
      = App <$> maybeZipWith f e1 e1' <*> maybeZipWith f e2 e2'
   maybeZipWith f (BinaryApp e1 op e2) (BinaryApp e1' op' e2')
      = BinaryApp <$> maybeZipWith f e1 e1' <*> op ≟ op' <*> maybeZipWith f e2 e2'
   maybeZipWith f (Lambda σ) (Lambda σ')
      = Lambda <$> maybeZipWith f σ σ'
   maybeZipWith f (MatchAs e σ) (MatchAs e' σ')
      = MatchAs <$> maybeZipWith f e e' <*> maybeZipWith f σ σ'
   maybeZipWith f (Let def e) (Let def' e')
      = Let <$> maybeZipWith f def def' <*> maybeZipWith f e e'
   maybeZipWith f (LetRec δ e) (LetRec δ' e')
      = LetRec <$> maybeZipWithList f δ δ' <*> maybeZipWith f e e'
   maybeZipWith _ _ _                             = Nothing
