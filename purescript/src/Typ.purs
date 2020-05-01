module Typ where

import Expr
import Prelude ((==), (<>))
import Data.Maybe (Maybe(..))


class Typed a where
      typeOf :: a -> Ctx -> Typ


instance hasTypElim :: Typed Elim where
      typeOf (ElimVar { x, tx, e }) ctx
            = TypFun tx (typeOf e (ctx :+: (Bind x tx)))
      typeOf (ElimPair { x, tx, y, ty, e }) ctx
            = let te = typeOf e (ctx :+: (Bind x tx) :+: (Bind y ty))
              in  TypFun (TypPair tx ty) te
      typeOf (ElimList { bnil: e, bcons: { x, y, e: e' } } ) ctx
            = let tnil  = typeOf e ctx
                  tx    = TypInt
                  ty    = TypList TypInt
                  tcons = typeOf e' (ctx :+: (Bind x tx) :+: (Bind y ty))
              in  if   tnil == tcons
                  then TypFun (TypList TypInt) tnil
                  else TypFailure "Elim branches have different types"
      typeOf (ElimBool { btrue: e, bfalse: e' }) ctx
            = if   typeOf e ctx == typeOf e' ctx
              then TypFun TypBool (typeOf e ctx)
              else TypFailure "Elim branches have different types"

instance hasTypExpr :: Typed Expr where
      typeOf (ExprVar x) ctx        = case find x ctx of
                                                Just t -> t
                                                _      -> TypFailure ("variable " <> x <> " not found")
      typeOf (ExprPair e1 e2) ctx   = TypPair (typeOf e1 ctx) (typeOf e2 ctx)
      typeOf (ExprPairSel e1 e2) ctx   = TypPair (typeOf e1 ctx) (typeOf e2 ctx)
      typeOf (ExprLet x e1 e2) ctx  = let v1    = (typeOf e1 ctx)
                                          ctx'  = (ctx :+: (Bind x v1))
                                      in  typeOf e2 ctx'
      typeOf (ExprInt n) ctx           = TypInt
      typeOf (ExprIntSel n) ctx           = TypInt
      typeOf ExprTrue ctx              = TypBool
      typeOf ExprTrueSel ctx              = TypBool
      typeOf ExprFalse ctx             = TypBool
      typeOf ExprFalseSel ctx             = TypBool
      typeOf ExprNil ctx               = TypList TypInt
      typeOf ExprNilSel ctx               = TypList TypInt
      typeOf (ExprCons e es) ctx       = TypList (typeOf e ctx)
      typeOf (ExprConsSel e es) ctx       = TypList (typeOf e ctx)
      typeOf (ExprMatch e elim) ctx    = let t2 = typeOf elim ctx
                                             t1 = typeOf e ctx
                                      in case t1, t2 of
                                                TypFun a b, t  -> if t == a
                                                                         then b
                                                                         else TypFailure "Match type error"
                                                _, _ -> TypFailure "Match type error"
      typeOf (ExprLetrec fun elim e) ctx = let ctx' = (ctx :+: Bind fun (typeOf elim ctx))
                                           in  typeOf e ctx'
      typeOf (ExprApp e e') ctx          = let t1 = typeOf e ctx
                                               t2 = typeOf e' ctx
                                           in case t1, t2 of
                                                TypFun a b, t -> if t == a
                                                                        then b
                                                                        else TypFailure "Application type error: applied expression not compatible to argument expression"
                                                _, _               -> TypFailure "Application type error"
      typeOf (ExprAdd e1 e2) ctx    = let v1 = typeOf e1 ctx
                                          v2 = typeOf e2 ctx
                                      in  case v1, v2 of
                                           TypInt, TypInt -> TypInt
                                           _, _  -> TypFailure "Arithemetic type error: e1 or/and e2 do not typecheck as ints"

instance hasTypVal :: Typed Val where
      typeOf ValTrue ctx                   = TypBool
      typeOf ValTrueSel ctx                   = TypBool
      typeOf ValFalse ctx                  = TypBool
      typeOf ValFalseSel ctx                  = TypBool
      typeOf (ValInt n) ctx                = TypInt
      typeOf (ValIntSel n) ctx                = TypInt
      typeOf (ValPair x y) ctx             = TypPair (typeOf x ctx) (typeOf y ctx)
      typeOf (ValPairSel x y) ctx             = TypPair (typeOf x ctx) (typeOf y ctx)
      typeOf ValNil ctx                    = TypList TypInt
      typeOf ValNilSel ctx                    = TypList TypInt
      typeOf (ValCons x xs) ctx            = TypList (typeOf x ctx)
      typeOf (ValConsSel x xs) ctx            = TypList (typeOf x ctx)
      typeOf (ValClosure env fun elim) ctx = typeOf elim ctx
      typeOf (ValFailure s) ctx            = TypFailure s
      typeOf ValBottom ctx                 = TypBottom
