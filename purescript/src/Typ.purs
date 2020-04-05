module Typ where

import Expr
import Prelude ((==), (<>))
import Data.Maybe (Maybe(..))

class Typed a where
      typeOf :: a -> Ctx -> Typ


instance hasTypBranchNil :: Typed BranchNil where
      typeOf (BranchNil _ e) ctx = typeOf e ctx

instance hasTypBranchCons :: Typed BranchCons where
      typeOf (BranchCons x xs tx e ) ctx   = let txs = TypList tx
                                                 te  = typeOf e (ctx :∁: (Bind x tx) :∁: (Bind xs txs))
                                             in  TypFun txs te

instance hasTypBranchTrue :: Typed BranchTrue where
      typeOf (BranchTrue e) ctx = TypFun TypBool (typeOf e ctx)

instance hasTypBranchFalse :: Typed BranchFalse where
      typeOf (BranchFalse e) ctx = TypFun TypBool (typeOf e ctx)


instance hasTypElim :: Typed Elim where
      typeOf (ElimVar x tx e) ctx        = TypFun tx (typeOf e (ctx :∁: (Bind x tx)))
      typeOf (ElimPair x tx y ty e) ctx  = let te = typeOf e (ctx :∁: (Bind x tx) :∁: (Bind y ty))
                                           in  TypFun (TypPair tx ty) te
      typeOf (ElimList bNil bCons ) ctx  = typeOf bCons ctx
      typeOf (ElimBool bTrue bFalse) ctx = typeOf bTrue ctx


instance hasTypExpr :: Typed Expr where
      typeOf ExprBottom ctx         = TypBottom
      typeOf (ExprVar x) ctx        = case findVarTyp x ctx of
                                                Just t -> t
                                                _      -> TypFailure ("variable " <> x <> " not found")
      typeOf (ExprPair e1 e2) ctx   = TypPair (typeOf e1 ctx) (typeOf e2 ctx)
      typeOf (ExprPair_Del e1 e2) ctx = TypPair (typeOf e1 ctx) (typeOf e2 ctx)
      typeOf (ExprLet x e1 e2) ctx  = let v1    = (typeOf e1 ctx)
                                          ctx'  = (ctx :∁: (Bind x v1))
                                      in  typeOf e2 ctx'
      typeOf (ExprLet_Body x e1 e2) ctx  = typeOf e2 ctx
      typeOf (ExprNum n) ctx        = TypNum
      typeOf ExprTrue ctx           = TypBool
      typeOf ExprFalse ctx          = TypBool
      typeOf ExprNil ctx            = TypList TypNum
      typeOf (ExprCons e es) ctx    = TypList (typeOf e ctx)
      typeOf (ExprCons_Del e es) ctx    = TypList (typeOf e ctx)
      typeOf (ExprMatch e elim) ctx = let t2 = typeOf elim ctx
                                          t1 = typeOf e ctx
                                      in case Bind t1 t2 of
                                                Bind (TypFun a b) t  -> if t == a
                                                                         then b
                                                                         else TypFailure "Match type error"
                                                _ -> TypFailure "Match type error"
      typeOf (ExprLetrec fun elim e) ctx = let ctx' = (ctx :∁: Bind fun (typeOf elim ctx))
                                           in  typeOf e ctx'
      typeOf (ExprApp e e') ctx          = let t1 = typeOf e ctx
                                               t2 = typeOf e' ctx
                                           in case Bind t1 t2 of
                                                Bind (TypFun a b) t -> if t == a
                                                                        then b
                                                                        else TypFailure "Application type error: applied expression not compatible to argument expression"
                                                _               -> TypFailure "Application type error"
      typeOf (ExprAdd e1 e2) ctx    = let v1 = typeOf e1 ctx
                                          v2 = typeOf e2 ctx
                                      in  case Bind v1 v2 of
                                           Bind TypNum TypNum -> TypNum
                                           _  -> TypFailure "Arithemetic type error: e1 or/and e2 do not typecheck as ints"

instance hasTypVal :: Typed Val where
      typeOf ValTrue ctx               = TypBool
      typeOf ValFalse ctx              = TypBool
      typeOf (ValNum n) ctx            = TypNum
      typeOf (ValPair x y) ctx         = TypPair (typeOf x ctx) (typeOf y ctx)
      typeOf (ValPair_Del x y) ctx     = TypPair (typeOf x ctx) (typeOf y ctx)
      typeOf ValNil ctx                = TypList TypNum
      typeOf (ValCons x xs) ctx        = TypList (typeOf x ctx)
      typeOf (ValCons_Del x xs) ctx    = TypList (typeOf x ctx)
      typeOf (ValClosure env fun elim) ctx = typeOf elim ctx
      typeOf (ValFailure s) ctx        = TypFailure s
      typeOf ValBottom ctx             = TypBottom