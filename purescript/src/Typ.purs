module Typ where

import Expr
import Prelude ((==), (<>))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

class HasTyp a where 
    getTyp :: a -> Ctx -> Typ


instance hasTypBranchNil :: HasTyp BranchNil where 
    getTyp (BranchNil _ e) ctx = getTyp e ctx

instance hasTypBranchCons :: HasTyp BranchCons where
    getTyp (BranchCons x xs tx e ) ctx   = let txs = TypList tx
                                               te  = getTyp e (ctx:>(Tuple x tx):>(Tuple xs txs)) 
                                           in  TypFun txs te
    getTyp (BranchCons_Head x tx e) ctx  = let te = getTyp e (ctx:>(Tuple x tx)) 
                                           in  TypFun tx te
    getTyp (BranchCons_Tail xs tx e) ctx = let txs = TypList tx
                                               te  = getTyp e (ctx:>(Tuple xs txs)) 
                                           in  TypFun txs te

instance hasTypBranchTrue :: HasTyp BranchTrue where
    getTyp (BranchTrue e) ctx = TypFun TypBool (getTyp e ctx)

instance hasTypBranchFalse :: HasTyp BranchFalse where
    getTyp (BranchFalse e) ctx = TypFun TypBool (getTyp e ctx)


instance hasTypElim :: HasTyp Elim where
    getTyp (ElimVar x tx e) ctx        = TypFun tx (getTyp e (ctx:>(Tuple x tx)))
    getTyp (ElimPair x tx y ty e) ctx  = let te = getTyp e (ctx:>(Tuple x tx):>(Tuple y ty))
                                         in  TypFun (TypPair tx ty) te 
    getTyp (ElimPair_Fst x tx e) ctx   = let te = getTyp e (ctx:>(Tuple x tx))
                                         in  TypFun (TypPair_Fst tx) te 
    getTyp (ElimPair_Snd y ty e) ctx   = let te = getTyp e (ctx:>(Tuple y ty))
                                         in  TypFun (TypPair_Snd ty) te 
    getTyp (ElimList bNil bCons ) ctx  = getTyp bCons ctx 
    getTyp (ElimBool bTrue bFalse) ctx = getTyp bTrue ctx


instance hasTypExpr :: HasTyp Expr where
      getTyp (ExprVar x) ctx        = case findVarTyp x ctx of 
                                                Just t -> t
                                                _      -> TypFailure ("variable " <> x <> " not found")
      getTyp (ExprPair e1 e2) ctx   = TypPair (getTyp e1 ctx) (getTyp e2 ctx)
      getTyp (ExprPair_Fst e1) ctx  = TypPair_Fst (getTyp e1 ctx)
      getTyp (ExprPair_Snd e2) ctx  = TypPair_Snd (getTyp e2 ctx)
      getTyp (ExprLet x e1 e2) ctx  = let v1    = (getTyp e1 ctx)
                                          ctx'  = (ctx:>(Tuple x v1))
                                      in  getTyp e2 ctx'
      getTyp (ExprLet_Body e2) ctx  = getTyp e2 ctx
      getTyp (ExprNum n) ctx        = TypNum 
      getTyp ExprTrue ctx           = TypBool
      getTyp ExprFalse ctx          = TypBool
      getTyp ExprNil ctx            = TypList TypVar
      getTyp (ExprCons e es) ctx    = TypList (getTyp e ctx)
      getTyp (ExprCons_Head e) ctx  = getTyp e ctx
      getTyp (ExprCons_Tail es) ctx = getTyp es ctx
      -- needs revising after adding type annotation
      getTyp (ExprMatch e elim) ctx = let t2 = getTyp elim ctx
                                          t1 = getTyp e ctx
                                      in case Tuple t1 t2 of 
                                                Tuple (TypFun a b) t  -> if t == a 
                                                                         then b 
                                                                         else TypFailure "Match type error"
                                                _ -> TypFailure "Match type error"
      getTyp (ExprFun elim) ctx     = getTyp elim ctx
      getTyp (ExprApp e e') ctx     = let t1 = getTyp e ctx
                                          t2 = getTyp e' ctx 
                                      in case Tuple t1 t2 of 
                                                Tuple (TypFun a b) t -> if t == a 
                                                                        then b 
                                                                        else TypFailure "Application type error: applied expression not compatible to argument expression"
                                                _               -> TypFailure "Application type error"
      getTyp (ExprApp_Fun e) ctx    = getTyp e ctx
      getTyp (ExprAdd e1 e2) ctx    = let v1 = getTyp e1 ctx 
                                          v2 = getTyp e2 ctx 
                                      in  case Tuple v1 v2 of 
                                           Tuple TypNum TypNum -> TypNum 
                                           _  -> TypFailure "Arithemetic type error: e1 or/and e2 do not typecheck as ints"

