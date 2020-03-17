module Typ where

import Expr
import Prelude ((==), (<>))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

class Typed a where 
      typeOf :: a -> Ctx -> Typ


instance hasTypBranchNil :: Typed BranchNil where 
      typeOf (BranchNil _ e) ctx = typeOf e ctx

instance hasTypBranchCons :: Typed BranchCons where
      typeOf (BranchCons x xs tx e ) ctx   = let txs = TypList tx
                                                 te  = typeOf e (ctx:>(Tuple x tx):>(Tuple xs txs)) 
                                             in  TypFun txs te
      typeOf (BranchCons_Head x tx e) ctx  = let te = typeOf e (ctx:>(Tuple x tx)) 
                                             in  TypFun tx te
      typeOf (BranchCons_Tail xs tx e) ctx = let txs = TypList tx
                                                 te  = typeOf e (ctx:>(Tuple xs txs)) 
                                             in  TypFun txs te

instance hasTypBranchTrue :: Typed BranchTrue where
      typeOf (BranchTrue e) ctx = TypFun TypBool (typeOf e ctx)

instance hasTypBranchFalse :: Typed BranchFalse where
      typeOf (BranchFalse e) ctx = TypFun TypBool (typeOf e ctx)


instance hasTypElim :: Typed Elim where
      typeOf (ElimVar x tx e) ctx        = TypFun tx (typeOf e (ctx:>(Tuple x tx)))
      typeOf (ElimPair x tx y ty e) ctx  = let te = typeOf e (ctx:>(Tuple x tx):>(Tuple y ty))
                                           in  TypFun (TypPair tx ty) te 
      typeOf (ElimPair_Fst x tx e) ctx   = let te = typeOf e (ctx:>(Tuple x tx))
                                           in  TypFun (TypPair_Fst tx) te 
      typeOf (ElimPair_Snd y ty e) ctx   = let te = typeOf e (ctx:>(Tuple y ty))
                                           in  TypFun (TypPair_Snd ty) te 
      typeOf (ElimList bNil bCons ) ctx  = typeOf bCons ctx 
      typeOf (ElimBool bTrue bFalse) ctx = typeOf bTrue ctx


instance hasTypExpr :: Typed Expr where
      typeOf (ExprVar x) ctx        = case findVarTyp x ctx of 
                                                Just t -> t
                                                _      -> TypFailure ("variable " <> x <> " not found")
      typeOf (ExprPair e1 e2) ctx   = TypPair (typeOf e1 ctx) (typeOf e2 ctx)
      typeOf (ExprPair_Fst e1) ctx  = TypPair_Fst (typeOf e1 ctx)
      typeOf (ExprPair_Snd e2) ctx  = TypPair_Snd (typeOf e2 ctx)
      typeOf (ExprLet x e1 e2) ctx  = let v1    = (typeOf e1 ctx)
                                          ctx'  = (ctx:>(Tuple x v1))
                                      in  typeOf e2 ctx'
      typeOf (ExprLet_Body e2) ctx  = typeOf e2 ctx
      typeOf (ExprNum n) ctx        = TypNum 
      typeOf ExprTrue ctx           = TypBool
      typeOf ExprFalse ctx          = TypBool
      typeOf ExprNil ctx            = TypList TypPoly
      typeOf (ExprCons e es) ctx    = TypList (typeOf e ctx)
      typeOf (ExprCons_Head e) ctx  = TypList_Head (typeOf e ctx)
      typeOf (ExprCons_Tail es) ctx = TypList_Tail (typeOf es ctx)
      typeOf (ExprMatch e elim) ctx = let t2 = typeOf elim ctx
                                          t1 = typeOf e ctx
                                      in case Tuple t1 t2 of 
                                                Tuple (TypFun a b) t  -> if t == a 
                                                                         then b 
                                                                         else TypFailure "Match type error"
                                                _ -> TypFailure "Match type error"
      typeOf (ExprFun elim) ctx     = typeOf elim ctx
      typeOf (ExprApp e e') ctx     = let t1 = typeOf e ctx
                                          t2 = typeOf e' ctx 
                                      in case Tuple t1 t2 of 
                                                Tuple (TypFun a b) t -> if t == a 
                                                                        then b 
                                                                        else TypFailure "Application type error: applied expression not compatible to argument expression"
                                                _               -> TypFailure "Application type error"
      typeOf (ExprApp_Fun e) ctx    = typeOf e ctx
      typeOf (ExprAdd e1 e2) ctx    = let v1 = typeOf e1 ctx 
                                          v2 = typeOf e2 ctx 
                                      in  case Tuple v1 v2 of 
                                           Tuple TypNum TypNum -> TypNum 
                                           _  -> TypFailure "Arithemetic type error: e1 or/and e2 do not typecheck as ints"

instance hasTypVal :: Typed Val where 
      typeOf ValTrue ctx               = TypBool
      typeOf ValFalse ctx              = TypBool
      typeOf (ValNum n) ctx            = TypNum
      typeOf (ValPair x y) ctx         = TypPair (typeOf x ctx) (typeOf y ctx) 
      typeOf (ValPair_Fst x) ctx       = TypPair_Fst (typeOf x ctx)  
      typeOf (ValPair_Snd y) ctx       = TypPair_Snd (typeOf y ctx) 
      typeOf ValNil ctx                = TypList TypPoly
      typeOf (ValCons x xs) ctx        = TypList (typeOf x ctx)
      typeOf (ValCons_Head x) ctx      = TypList_Head (typeOf x ctx)
      typeOf (ValCons_Tail xs) ctx     = TypList_Tail (typeOf xs ctx)
      typeOf (ValClosure env elim) ctx = typeOf elim ctx
      typeOf (ValFailure s) ctx        = TypFailure s
