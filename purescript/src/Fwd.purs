module Fwd where

-- import Prelude ((<>), ($))
-- import Data.Maybe (Maybe(..))
-- import Data.Semiring ((+))
-- import Expr


-- match :: Val -> Elim -> Trace -> Maybe (T3 Env Expr Availability)
-- match val σ t
--  = case  val, σ of 
--     _, ElimVar x t expr 
--         ->  Just $ T3 (EnvNil :∈: T2 x val) expr Top
--     ValTrue, ElimBool (BranchTrue expr1) (BranchFalse expr2)
--         ->  Just $ T3 EnvNil expr1 Top
--     ValFalse, ElimBool (BranchTrue expr1) (BranchFalse expr2)
--         ->  Just $ T3 EnvNil expr2 Top
--     ValPair x' y', ElimPair x _ y _ expr
--         ->  let ρ' = (EnvNil :∈: T2 y y' :∈: T2 x x')
--             in  Just $ T3 ρ' expr Top
--     ValPair_Del x' y', ElimPair x _ y _ expr
--         ->  let ρ' = (EnvNil :∈: T2 y y' :∈: T2 x x')
--             in  Just $ T3 ρ' expr Bottom
--     ValNil, ElimList (BranchNil _ expr2) (BranchCons x xs _ expr1) 
--         ->  Just $ T3 EnvNil expr2 Top
--     ValCons v vs, ElimList (BranchNil _ expr2) (BranchCons x xs _ expr1) 
--         ->  let ρ' = (EnvNil :∈: T2 xs vs :∈: T2 x v)
--             in  Just $ T3 ρ' expr1 Top
--     ValCons_Del v vs, ElimList (BranchNil _ expr2) (BranchCons x xs _ expr1) 
--         ->  let ρ' = (EnvNil :∈: T2 xs vs :∈: T2 x v)
--             in  Just $ T3 ρ' expr1 Bottom
--     _, _ ->  Nothing




-- fwd :: Expr -> Availability -> Trace -> Env -> Val
-- fwd ExprBottom α t ρ             = ValBottom
-- fwd (ExprVar x) α t ρ
--  = case findVarVal x t ρ of
--     Just val -> val
--     _        -> ValFailure ("variable " <> x <> " not found")
-- fwd ExprTrue Top t ρ             = ValTrue
-- fwd ExprTrue Bottom t ρ          = ValBottom
-- fwd ExprFalse Top t ρ            = ValFalse
-- fwd ExprFalse Bottom t ρ         = ValBottom
-- fwd (ExprNum n) Top t ρ          = ValNum n
-- fwd (ExprNum n) Bottom t ρ       = ValBottom
-- fwd (ExprPair e1 e2) Top t ρ     = ValPair (fwd e1 Top ρ) (fwd e2 Top ρ)
-- fwd (ExprPair e1 e2) Bottom t ρ  = ValPair_Del (fwd e1 Bottom ρ) (fwd e2 Bottom ρ)
-- fwd (ExprPair_Del e1 e2) α t ρ   = ValPair_Del (fwd e1 α ρ) (fwd e2 α ρ)
-- fwd (ExprLetrec fun σ e) α t ρ   = fwd e α (ρ :∈: T2 fun (ValClosure ρ fun σ))
-- fwd (ExprApp e e') α t ρ
--  = case fwd e α t ρ  of
--      ValClosure ρ' fun σ
--         -> case match (fwd e' α ρ) σ of
--                 Just (T3 ρ'' e''  α') -> fwd e'' α' (concEnv ρ' ρ'' :∈: T2 fun (ValClosure ρ' fun σ))
--                 Nothing               -> ValFailure "Match not found"
--      _  -> ValFailure "Applied expression e in e e' does not fwduate to closure"
-- fwd (ExprAdd e1 e2) Bottom t ρ   = ValBottom
-- fwd (ExprAdd e1 e2) Top t ρ
--  = let v1 = fwd e1 Top t ρ
--        v2 = fwd e2 Top t ρ
--    in  case v1, v2 of
--           (ValNum n1), (ValNum n2) -> ValNum (n1 + n2)
--           ValBottom,  _            -> ValBottom 
--           _,          ValBottom    -> ValBottom
--           _,          _            -> ValFailure "Arithemetic type error: e1 or/and e2 do not fwduate to ints"
-- fwd (ExprLet x e1 e2) α t ρ
--  = let v1  = fwd e1 α ρ
--        ρ'  = (ρ :∈: T2 x v1)
--    in  fwd e2 α ρ'
-- fwd (ExprLet_Body x e1 e2) α t ρ = fwd e2 α (ρ :∈: T2 x ValBottom)
-- fwd ExprNil α t ρ                = ValNil
-- fwd (ExprCons e es) Top t ρ      = ValCons (fwd e Top ρ) (fwd es Top ρ)
-- fwd (ExprCons e es) Bottom t ρ   = ValCons_Del (fwd e Bottom ρ) (fwd es Bottom ρ)
-- fwd (ExprCons_Del e es) α t ρ    = ValCons_Del (fwd e α ρ) (fwd es α ρ)
-- fwd (ExprMatch e σ) α t ρ
--  = case match (fwd e α t ρ) σ of
--     Nothing            -> ValFailure "Match not found"
--     Just (T3 ρ' e' α') -> fwd e' α' (concEnv ρ ρ')



