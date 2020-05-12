module Typed where

import Prelude ((==), (<>), unit)
import Data.Maybe (Maybe(..))
import Bindings (Bind(..), find, (:+:))
import Expr (Ctx, Elim, Expr(..), Typ)
import Expr (Elim(..)) as E
import Expr (Typ(..)) as T
import Val (Val)
import Val (Val(..)) as V
import Util (__todo)


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
      typeOf (Var x) ctx        = case find x ctx of
                                                Just t -> t
                                                _      -> T.TypFailure ("variable " <> x <> " not found")
      typeOf (Pair e1 e2) ctx   = T.TypPair (typeOf e1 ctx) (typeOf e2 ctx)
      typeOf (PairSel e1 e2) ctx   = T.TypPair (typeOf e1 ctx) (typeOf e2 ctx)
      typeOf (Let x e1 e2) ctx  = let v1    = (typeOf e1 ctx)
                                      ctx'  = (ctx :+: (Bind x v1))
                                      in  typeOf e2 ctx'
      typeOf (Int n) ctx           = T.TypInt
      typeOf (IntSel n) ctx        = T.TypInt
      typeOf True ctx              = T.TypBool
      typeOf TrueSel ctx           = T.TypBool
      typeOf False ctx             = T.TypBool
      typeOf FalseSel ctx          = T.TypBool
      typeOf Nil ctx               = T.TypList T.TypInt
      typeOf NilSel ctx            = T.TypList T.TypInt
      typeOf (Cons e es) ctx       = T.TypList (typeOf e ctx)
      typeOf (ConsSel e es) ctx    = T.TypList (typeOf e ctx)
      typeOf (Match e elim) ctx    = let t2 = typeOf elim ctx
                                         t1 = typeOf e ctx
                                      in case t1, t2 of
                                                T.TypFun a b, t  -> if t == a
                                                                         then b
                                                                         else T.TypFailure "Match type error"
                                                _, _ -> T.TypFailure "Match type error"
      typeOf (Letrec fun elim e) ctx = let ctx' = (ctx :+: Bind fun (typeOf elim ctx))
                                           in  typeOf e ctx'
      typeOf (App e e') ctx          = let t1 = typeOf e ctx
                                           t2 = typeOf e' ctx
                                           in case t1, t2 of
                                                T.TypFun a b, t -> if t == a
                                                                        then b
                                                                        else T.TypFailure "Application type error: applied expression not compatible to argument expression"
                                                _, _               -> T.TypFailure "Application type error"
      typeOf (Add e1 e2) ctx    = let v1 = typeOf e1 ctx
                                      v2 = typeOf e2 ctx
                                      in  case v1, v2 of
                                           T.TypInt, T.TypInt -> T.TypInt
                                           _, _  -> T.TypFailure "Arithemetic type error: e1 or/and e2 do not typecheck as ints"
      typeOf (BinaryApp _ _ _) ctx = __todo

instance hasTypVal :: Typed Val where
      typeOf V.True ctx                   = T.TypBool
      typeOf V.TrueSel ctx                = T.TypBool
      typeOf V.False ctx                  = T.TypBool
      typeOf V.FalseSel ctx               = T.TypBool
      typeOf (V.Int n) ctx                = T.TypInt
      typeOf (V.IntSel n) ctx             = T.TypInt
      typeOf (V.Pair x y) ctx             = T.TypPair (typeOf x ctx) (typeOf y ctx)
      typeOf (V.PairSel x y) ctx          = T.TypPair (typeOf x ctx) (typeOf y ctx)
      typeOf V.Nil ctx                    = T.TypList T.TypInt
      typeOf V.NilSel ctx                 = T.TypList T.TypInt
      typeOf (V.Cons x xs) ctx            = T.TypList (typeOf x ctx)
      typeOf (V.ConsSel x xs) ctx         = T.TypList (typeOf x ctx)
      typeOf (V.Closure env fun elim) ctx = typeOf elim ctx
      typeOf (V.Failure s) ctx            = T.TypFailure s
      typeOf V.Bot ctx                    = T.TypBottom
