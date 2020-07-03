module Expr where

import Prelude hiding (top)
import Control.Apply (lift2)
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Bindings (Var)
import DataType (Ctr)
import Lattice (class Selectable, class Selectable2, Selected, mapα, maybeZipWith, maybeZipWithα)
import Util (type (+), (≟), error)

data VarDef = VarDef Elim Expr -- elim has codomain unit
data VarDef2 a = VarDef2 (Elim2 a) (Expr2 a) -- elim has codomain unit
type VarDefs = List VarDef
data RecDef = RecDef Var Elim
data RecDef2 a = RecDef2 Var (Elim2 a)
type RecDefs = List RecDef
type RecDefs2 a = List (RecDef2 a)

data RawExpr =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   Constr Ctr (List Expr) |
   Lambda Elim |
   App Expr Expr |
   BinaryApp Expr Var Expr |
   MatchAs Expr Elim |
   Let VarDef Expr |
   LetRec RecDefs Expr

data RawExpr2 a =
   Var2 Var |
   Op2 Var |
   Int2 Int |
   Str2 String |
   Constr2 Ctr (List (Expr2 a)) |
   Lambda2 Elim |
   App2 (Expr2 a) (Expr2 a) |
   BinaryApp2 (Expr2 a) Var (Expr2 a) |
   MatchAs2 (Expr2 a) (Elim2 a) |
   Let2 (VarDef2 a) (Expr2 a) |
   LetRec2 (RecDefs2 a) (Expr2 a)

data Expr = Expr Selected RawExpr

data Expr2 a = Expr2 a (RawExpr2 a)

derive instance functorVarDef :: Functor VarDef2
derive instance functorRecDef :: Functor RecDef2
derive instance functorRawExpr :: Functor RawExpr2
derive instance functorExpr :: Functor Expr2

expr :: RawExpr -> Expr
expr = Expr false

-- Continuation of an eliminator. None form only used in structured let.
data Cont = None | Body Expr | Arg Elim
data Cont2 a = None2 | Body2 (Expr2 a) | Arg2 (Elim2 a)

body :: Cont -> Expr
body (Body e) = e
body _ = error "Expression expected"

instance selectableCont :: Selectable Cont where
   mapα f None          = None
   mapα f (Body e)      = Body $ mapα f e
   mapα f (Arg σ)       = Arg $ mapα f σ

   maybeZipWithα f (Body e) (Body e')        = Body <$> maybeZipWithα f e e'
   maybeZipWithα f (Arg σ) (Arg σ')          = Arg <$> maybeZipWithα f σ σ'
   maybeZipWithα _ _ _                       = Nothing

instance selectable2Cont :: Selectable2 Cont2 where
   maybeZipWith f (Body2 e) (Body2 e')        = Body2 <$> maybeZipWith f e e'
   maybeZipWith f (Arg2 σ) (Arg2 σ')          = Arg2 <$> maybeZipWith f σ σ'
   maybeZipWith _ _ _                       = Nothing

data Elim =
   ElimVar Var Cont |
   ElimConstr (Map Ctr Cont)

data Elim2 a =
   ElimVar2 Var (Cont2 a) |
   ElimConstr2 (Map Ctr (Cont2 a))

derive instance functorCont :: Functor Cont2
derive instance functorElim :: Functor Elim2

instance elimSelectable :: Selectable Elim where
   mapα f (ElimVar x κ)    = ElimVar x $ mapα f κ
   mapα f (ElimConstr κs)  = ElimConstr $ map (mapα f) κs

   maybeZipWithα f (ElimVar x κ) (ElimVar x' κ')
      = ElimVar <$> x ≟ x' <*> maybeZipWithα f κ κ'
   maybeZipWithα f (ElimConstr κs) (ElimConstr κs')   = ElimConstr <$> maybeZipWithα f κs κs'
   maybeZipWithα _ _ _                                = Nothing

instance selectable2Elim :: Selectable2 Elim2 where
   maybeZipWith f (ElimVar2 x κ) (ElimVar2 x' κ')
      = ElimVar2 <$> x ≟ x' <*> maybeZipWith (error "todo") κ κ'
   maybeZipWith f (ElimConstr2 κs) (ElimConstr2 κs')   = ElimConstr2 <$> maybeZipWith (error "todo") κs κs'
   maybeZipWith _ _ _                                = Nothing

data Module = Module (List (VarDef + RecDefs))

instance defSelectable :: Selectable VarDef where
   mapα f (VarDef σ e)                          = VarDef (mapα f σ) (mapα f e)
   maybeZipWithα f (VarDef σ e) (VarDef σ' e')  = VarDef <$> maybeZipWithα f σ σ' <*> maybeZipWithα f e e'

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
   maybeZipWithα f (Constr c es) (Constr c' es')
      = Constr <$> c ≟ c' <*> maybeZipWithα f es' es'
   maybeZipWithα f (App e1 e2) (App e1' e2')
      = App <$>  maybeZipWithα f e1 e1' <*> maybeZipWithα f e2 e2'
   maybeZipWithα f (BinaryApp e1 op e2) (BinaryApp e1' op' e2')
      = BinaryApp <$> maybeZipWithα f e1 e1' <*> op ≟ op' <*> maybeZipWithα f e2 e2'
   maybeZipWithα f (Lambda σ) (Lambda σ')
      = Lambda <$> maybeZipWithα f σ σ'
   maybeZipWithα f (MatchAs e σ) (MatchAs e' σ')
      = MatchAs <$> maybeZipWithα f e e' <*> maybeZipWithα f σ σ'
   maybeZipWithα f (Let def e) (Let def' e')
      = Let <$> maybeZipWithα f def def' <*> maybeZipWithα f e e'
   maybeZipWithα f (LetRec δ e) (LetRec δ' e')
      = LetRec <$> maybeZipWithα f δ δ' <*>  maybeZipWithα f e e'
   maybeZipWithα _ _ _                             = Nothing

instance selectable2Def :: Selectable2 VarDef2 where
   maybeZipWith f (VarDef2 σ e) (VarDef2 σ' e')  = VarDef2 <$> maybeZipWith (error "todo") σ σ' <*> maybeZipWith f e e'

instance selectable2RecDef :: Selectable2 RecDef2 where
   maybeZipWith f (RecDef2 x σ) (RecDef2 x' σ')  = RecDef2 <$> x ≟ x' <*> maybeZipWith f σ σ'

instance selectable2Expr :: Selectable2 Expr2 where
   maybeZipWith f (Expr2 α r) (Expr2 α' r')   = Expr2 <$> pure (f α α') <*> maybeZipWith f r r'

instance selectable2RawExpr :: Selectable2 RawExpr2 where
   maybeZipWith _ (Var2 x) (Var2 x')                = Var2 <$> x ≟ x'
   maybeZipWith _ (Op2 op) (Op2 op')                = Op2 <$> op ≟ op'
   maybeZipWith _ (Int2 n) (Int2 n')                = Int2 <$> n ≟ n'
   maybeZipWith _ (Str2 s) (Var2 s')                = Str2 <$> s ≟ s'
   maybeZipWith f (Constr2 c es) (Constr2 c' es')
      = Constr2 <$> c ≟ c' <*> maybeZipWith (error "todo") es es'
   maybeZipWith f (App2 e1 e2) (App2 e1' e2')
      = App2 <$> maybeZipWith f e1 e1' <*> maybeZipWith f e2 e2'
   maybeZipWith f (BinaryApp2 e1 op e2) (BinaryApp2 e1' op' e2')
      = BinaryApp2 <$> maybeZipWith (error "todo") e1 e1' <*> op ≟ op' <*> maybeZipWith f e2 e2'
   maybeZipWith f (Lambda2 σ) (Lambda2 σ')
      = Lambda2 <$> (error "todo") -- maybeZipWith f σ σ'
   maybeZipWith f (MatchAs2 e σ) (MatchAs2 e' σ')
      = MatchAs2 <$> maybeZipWith (error "todo") e e' <*> maybeZipWith f σ σ'
   maybeZipWith f (Let2 def e) (Let2 def' e')
      = Let2 <$> maybeZipWith f def def' <*> maybeZipWith f e e'
   maybeZipWith f (LetRec2 δ e) (LetRec2 δ' e')
      = LetRec2 <$> maybeZipWith (error "todo") δ δ' <*> maybeZipWith f e e'
   maybeZipWith _ _ _                             = Nothing
