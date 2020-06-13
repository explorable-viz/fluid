module Expr where

import Prelude hiding (top)
import Bindings (Var)
import Data.List (List, zipWith)
import Data.Map (Map)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import DataType (Ctr)
import Lattice (class Selectable, Selected, mapα, maybeZipWithα)
import Util ((≟), error)

data Def = Def Elim2 Expr -- elim has codomain unit
data RecDef = RecDef Var Elim2
type RecDefs = List RecDef

data RawExpr =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List Expr) |
   True | False |
   Pair Expr Expr |
   Nil | Cons Expr Expr |
   Lambda Elim2 |
   App Expr Expr |
   BinaryApp Expr Var Expr |
   MatchAs Expr Elim2 |
   Let Def Expr |
   LetRec RecDefs Expr

data Expr = Expr Selected RawExpr

expr :: RawExpr -> Expr
expr = Expr false

data Cont = CExpr Expr | CElim Elim2

asExpr :: Cont -> Expr
asExpr (CExpr e) = e
asExpr (CElim _) = error "Expression expected"

instance selectableCont :: Selectable Cont where
   mapα f (CExpr e)  = CExpr $ mapα f e
   mapα f (CElim σ)  = CElim $ mapα f σ

   maybeZipWithα f (CExpr e) (CExpr e')   = CExpr <$> maybeZipWithα f e e'
   maybeZipWithα f (CElim σ) (CElim σ')   = CElim <$> maybeZipWithα f σ σ'
   maybeZipWithα _ _ _                    = Nothing

data Elim2 =
   ElimVar2 Var Cont |
   ElimConstr (Map Ctr Cont)

instance elim2Selectable :: Selectable Elim2 where
   mapα f (ElimVar2 x κ)   = ElimVar2 x $ mapα f κ
   mapα f (ElimConstr κs)  = ElimConstr $ map (mapα f) κs

   maybeZipWithα f (ElimVar2 x κ) (ElimVar2 x' κ')    = ElimVar2 <$> x ≟ x' <*> maybeZipWithα f κ κ'
   maybeZipWithα f (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeZipWithα f κs κs'
   maybeZipWithα _ _ _                                = Nothing

data Module = Module (List (Either Def RecDefs))

instance defSelectable :: Selectable Def where
   mapα f (Def σ e)                       = Def (mapα f σ) (mapα f e)
   maybeZipWithα f (Def σ e) (Def σ' e')  = Def <$> maybeZipWithα f σ σ' <*> maybeZipWithα f e e'

instance recDefSelectable :: Selectable RecDef where
   mapα f (RecDef x σ)                          = RecDef x (mapα f σ)
   maybeZipWithα f (RecDef x σ) (RecDef x' σ')  = RecDef <$> x ≟ x' <*> maybeZipWithα f σ σ'

instance exprSelectable :: Selectable Expr where
   mapα f (Expr α r)                         = Expr (f α) $ mapα f r
   maybeZipWithα f (Expr α r) (Expr α' r')   = Expr <$> pure (f α α') <*> maybeZipWithα f r r'

instance rawExprSelectable :: Selectable RawExpr where
   mapα _ (Var x)             = Var x
   mapα _ (Op φ)              = Op φ
   mapα _ (Int n)             = Int n
   mapα _ (Str str)           = Str str
   mapα f (Constr c es)       = Constr c $ map (mapα f) es
   mapα _ True                = True
   mapα _ False               = False
   mapα f (Pair e e')         = Pair (mapα f e) (mapα f e')
   mapα _ Nil                 = Nil
   mapα f (Cons e e')         = Cons (mapα f e) (mapα f e')
   mapα f (Lambda σ)          = Lambda (mapα f σ)
   mapα f (App e e')          = App (mapα f e) (mapα f e')
   mapα f (BinaryApp e op e') = BinaryApp (mapα f e) op (mapα f e')
   mapα f (MatchAs e σ)       = MatchAs (mapα f e) (mapα f σ)
   mapα f (Let def e)         = Let (mapα f def) (mapα f e)
   mapα f (LetRec δ e)        = LetRec (map (mapα f) δ) (mapα f e)

   maybeZipWithα _ (Var x) (Var x')                = Var <$> x ≟ x'
   maybeZipWithα _ (Op op) (Op op')                = Op <$> op ≟ op'
   maybeZipWithα _ (Int n) (Int n')                = Int <$> n ≟ n'
   maybeZipWithα _ (Str s) (Var s')                = Str <$> s ≟ s'
   maybeZipWithα f (Constr c es) (Constr c' es') =
      Constr <$> c ≟ c' <*> sequence (zipWith (maybeZipWithα f) es' es')
   maybeZipWithα _ False False                     = pure False
   maybeZipWithα _ True True                       = pure True
   maybeZipWithα f (Pair e1 e2) (Pair e1' e2')     = Pair <$> maybeZipWithα f e1 e1' <*> maybeZipWithα f e2 e2'
   maybeZipWithα _ Nil Nil                         = pure Nil
   maybeZipWithα f (Cons e1 e2) (Cons e1' e2')     = Cons <$> maybeZipWithα f e1 e1' <*> maybeZipWithα f e2 e2'
   maybeZipWithα _ _ _                             = Nothing
