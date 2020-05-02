module Typed where

import Bindings (Bind(..), find, (:+:))
import Expr (Ctx, Elim, Expr, Typ)
import Expr (Expr(..), Elim(..)) as E
import Expr (Typ(..)) as T
import Val (Val)
import Val (Val(..)) as V
import Prelude ((==), (<>))
import Data.Maybe (Maybe(..))


class Typed a where
      typeOf :: a -> Ctx -> Typ


instance hasTypElim :: Typed Elim where
      typeOf (E.ElimVar { x, tx, e }) ctx
            = T.TypFun tx (typeOf e (ctx :+: (Bind x tx)))
      typeOf (E.ElimPair { x, tx, y, ty, e }) ctx
            = let te = typeOf e (ctx :+: (Bind x tx) :+: (Bind y ty))
              in  T.TypFun (T.TypPair tx ty) te
      typeOf (E.ElimList { bnil: e, bcons: { x, y, e: e' } } ) ctx
            = let tnil  = typeOf e ctx
                  tx    = T.TypInt
                  ty    = T.TypList T.TypInt
                  tcons = typeOf e' (ctx :+: (Bind x tx) :+: (Bind y ty))
              in  if   tnil == tcons
                  then T.TypFun (T.TypList T.TypInt) tnil
                  else T.TypFailure "Elim branches have different types"
      typeOf (E.ElimBool { btrue: e, bfalse: e' }) ctx
            = if   typeOf e ctx == typeOf e' ctx
              then T.TypFun T.TypBool (typeOf e ctx)
              else T.TypFailure "Elim branches have different types"

instance hasTypExpr :: Typed Expr where
      typeOf (E.ExprVar x) ctx        = case find x ctx of
                                                Just t -> t
                                                _      -> T.TypFailure ("variable " <> x <> " not found")
      typeOf (E.ExprPair e1 e2) ctx   = T.TypPair (typeOf e1 ctx) (typeOf e2 ctx)
      typeOf (E.ExprPairSel e1 e2) ctx   = T.TypPair (typeOf e1 ctx) (typeOf e2 ctx)
      typeOf (E.ExprLet x e1 e2) ctx  = let v1    = (typeOf e1 ctx)
                                            ctx'  = (ctx :+: (Bind x v1))
                                      in  typeOf e2 ctx'
      typeOf (E.ExprInt n) ctx           = T.TypInt
      typeOf (E.ExprIntSel n) ctx        = T.TypInt
      typeOf E.ExprTrue ctx              = T.TypBool
      typeOf E.ExprTrueSel ctx           = T.TypBool
      typeOf E.ExprFalse ctx             = T.TypBool
      typeOf E.ExprFalseSel ctx          = T.TypBool
      typeOf E.ExprNil ctx               = T.TypList T.TypInt
      typeOf E.ExprNilSel ctx            = T.TypList T.TypInt
      typeOf (E.ExprCons e es) ctx       = T.TypList (typeOf e ctx)
      typeOf (E.ExprConsSel e es) ctx    = T.TypList (typeOf e ctx)
      typeOf (E.ExprMatch e elim) ctx    = let t2 = typeOf elim ctx
                                               t1 = typeOf e ctx
                                      in case t1, t2 of
                                                T.TypFun a b, t  -> if t == a
                                                                         then b
                                                                         else T.TypFailure "Match type error"
                                                _, _ -> T.TypFailure "Match type error"
      typeOf (E.ExprLetrec fun elim e) ctx = let ctx' = (ctx :+: Bind fun (typeOf elim ctx))
                                           in  typeOf e ctx'
      typeOf (E.ExprApp e e') ctx          = let t1 = typeOf e ctx
                                                 t2 = typeOf e' ctx
                                           in case t1, t2 of
                                                T.TypFun a b, t -> if t == a
                                                                        then b
                                                                        else T.TypFailure "Application type error: applied expression not compatible to argument expression"
                                                _, _               -> T.TypFailure "Application type error"
      typeOf (E.ExprAdd e1 e2) ctx    = let v1 = typeOf e1 ctx
                                            v2 = typeOf e2 ctx
                                      in  case v1, v2 of
                                           T.TypInt, T.TypInt -> T.TypInt
                                           _, _  -> T.TypFailure "Arithemetic type error: e1 or/and e2 do not typecheck as ints"

instance hasTypVal :: Typed Val where
      typeOf V.ValTrue ctx                   = T.TypBool
      typeOf V.ValTrueSel ctx                = T.TypBool
      typeOf V.ValFalse ctx                  = T.TypBool
      typeOf V.ValFalseSel ctx               = T.TypBool
      typeOf (V.ValInt n) ctx                = T.TypInt
      typeOf (V.ValIntSel n) ctx             = T.TypInt
      typeOf (V.ValPair x y) ctx             = T.TypPair (typeOf x ctx) (typeOf y ctx)
      typeOf (V.ValPairSel x y) ctx          = T.TypPair (typeOf x ctx) (typeOf y ctx)
      typeOf V.ValNil ctx                    = T.TypList T.TypInt
      typeOf V.ValNilSel ctx                 = T.TypList T.TypInt
      typeOf (V.ValCons x xs) ctx            = T.TypList (typeOf x ctx)
      typeOf (V.ValConsSel x xs) ctx         = T.TypList (typeOf x ctx)
      typeOf (V.ValClosure env fun elim) ctx = typeOf elim ctx
      typeOf (V.ValFailure s) ctx            = T.TypFailure s
      typeOf V.ValBottom ctx                 = T.TypBottom
