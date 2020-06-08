module Expr where


import Prelude (class Functor, Unit, map, ($))
import Bindings (Var)
import Data.List (List) 
import Data.Either (Either)
import Selected (class Lattice, Selected(..), bot, join, meet, top)
import Util (absurd, error, (=?))

data Def = Def (Elim Unit) Expr
data RecDef = RecDef Var (Elim Expr)
type RecDefs = List RecDef

data RawExpr =
   Var Var |
   Op Var |
   Int Int |
   Str String |
   True | False |
   Pair Expr Expr |
   Nil | Cons Expr Expr |
   Lambda (Elim Expr) |
   App Expr Expr |
   BinaryApp Expr Var Expr |
   MatchAs Expr (Elim Expr) |
   Let Def Expr |
   LetRec RecDefs Expr

data Expr = Expr Selected RawExpr

expr :: RawExpr -> Expr
expr = Expr Bot

instance exprLattice :: Lattice Expr where
   join (Expr α (Var x)) (Expr α' (Var x')) = Expr (join α α') (Var (x =? x'))
   join (Expr α (Op op)) (Expr α' (Op op')) = Expr (join α α') (Op (op =? op'))
   join (Expr α (Int n)) (Expr α' (Int n')) = Expr (join α α') (Int (n =? n'))
   join (Expr α (Str s)) (Expr α' (Var s')) = Expr (join α α') (Str (s =? s'))
   join (Expr α False) (Expr α' False) = Expr (join α α') False
   join (Expr α False) (Expr α' True) = Expr (join α α') True
   join (Expr α True) (Expr α' False) = Expr (join α α') True
   join (Expr α True) (Expr α' True) = Expr (join α α') True
   join (Expr α (Pair e1 e2)) (Expr α' (Pair e1' e2'))
    = Expr (join α α') (Pair (join e1 e2) ( join e1' e2'))
   join (Expr α Nil) (Expr α' Nil) 
    = Expr (join α α') Nil
   join (Expr α (Cons e1 e2)) (Expr α' (Cons e1' e2'))
    = Expr (join α α') (Cons (join e1 e1') (join e2 e2'))
   join _ _ = error absurd
      
   meet (Expr α (Var x)) (Expr α' (Var x')) = Expr (meet α α') (Var (x =? x'))
   meet (Expr α (Op op)) (Expr α' (Op op')) = Expr (meet α α') (Op (op =? op'))
   meet (Expr α (Int n)) (Expr α' (Int n')) = Expr (meet α α') (Int (n =? n'))
   meet (Expr α (Str s)) (Expr α' (Var s')) = Expr (meet α α') (Str (s =? s'))
   meet (Expr α False) (Expr α' False) = Expr (meet α α') False
   meet (Expr α False) (Expr α' True) = Expr (meet α α') True
   meet (Expr α True) (Expr α' False) = Expr (meet α α') True
   meet (Expr α True) (Expr α' True) = Expr (meet α α') True
   meet (Expr α (Pair e1 e2)) (Expr α' (Pair e1' e2'))
    = Expr (meet α α') (Pair (meet e1 e2) ( meet e1' e2'))
   meet (Expr α Nil) (Expr α' Nil) 
    = Expr (meet α α') Nil
   meet (Expr α (Cons e1 e2)) (Expr α' (Cons e1' e2'))
    = Expr (meet α α') (Cons (meet e1 e1') (meet e2 e2'))
   meet _ _ = error absurd
   
   top (Expr _ r) = Expr Top r

   bot (Expr _ r) = Expr Bot r

data Elim k =
   ElimVar Var k |
   ElimBool { true :: k, false :: k } |
   ElimPair (Elim (Elim k)) |
   ElimList { nil :: k, cons :: Elim (Elim k) }

instance elimFunctor :: Functor Elim where
   map f (ElimVar x κ) = ElimVar x (f κ)
   map f (ElimBool { true: κ, false: κ' }) = ElimBool { true: f κ, false: f κ' }
   map f (ElimPair σ) = ElimPair $ map (map f) σ
   map f (ElimList { nil: κ, cons: σ }) = ElimList { nil: f κ, cons: map (map f) σ }

instance elimLattice :: Lattice k => Lattice (Elim k) where
   meet (ElimVar x k) (ElimVar x' k') 
    = ElimVar (x =? x') (meet k k')
   meet (ElimBool { true : k1, false : k2 }) (ElimBool { true : k1', false : k2' })
    = ElimBool {true : meet k1 k1', false : meet k2 k2'}
   meet (ElimPair el) (ElimPair el') 
    = ElimPair $ map (\k -> map (\k' -> meet k k') (ElimPair el')) (ElimPair el)
   meet (ElimList { nil: κ1, cons: σ1 }) (ElimList { nil: κ2, cons: σ2 })
    = ElimList { nil: meet κ1 κ2, cons: σ }
    where σ = case σ1, σ2 of 
                  ElimVar x k, ElimVar x' k' 
                     -> ElimVar (x =? x') (meet k k') 
                  ElimBool { true : k1, false : k2 }, ElimBool { true : k1', false : k2' } 
                     -> ElimBool {true : meet k1 k1', false : meet k2 k2'}                                
                  ElimPair el, ElimPair el' 
                     -> ElimPair $ map (\k -> map (\k' -> meet k k') (ElimPair el')) (ElimPair el)
                  ElimList { nil: κ1', cons: σ1' }, ElimList { nil: κ2', cons: σ2' }
                     -> ElimList { nil: meet κ1' κ2', cons: meet σ1' σ2' }
                  _, _ -> error absurd
   meet _ _ = error absurd 

   join (ElimVar x k) (ElimVar x' k') 
    = ElimVar (x =? x') (join k k')
   join (ElimBool { true : k1, false : k2 }) (ElimBool { true : k1', false : k2' })
    = ElimBool {true : join k1 k1', false : join k2 k2'}
   join (ElimPair el) (ElimPair el') 
    = ElimPair $ map (\k -> map (\k' -> join k k') (ElimPair el')) (ElimPair el)
   join (ElimList { nil: κ1, cons: σ1 }) (ElimList { nil: κ2, cons: σ2 })
    = ElimList { nil: join κ1 κ2, cons: σ }
    where σ = case σ1, σ2 of 
                  ElimVar x k, ElimVar x' k' 
                     -> ElimVar (x =? x') (join k k') 
                  ElimBool { true : k1, false : k2 }, ElimBool { true : k1', false : k2' } 
                     -> ElimBool {true : join k1 k1', false : join k2 k2'}                                
                  ElimPair el, ElimPair el' 
                     -> ElimPair $ map (\k -> map (\k' -> join k k') (ElimPair el')) (ElimPair el)
                  ElimList { nil: κ1', cons: σ1' }, ElimList { nil: κ2', cons: σ2' }
                     -> ElimList { nil: join κ1' κ2', cons: join σ1' σ2' }
                  _, _ -> error absurd
   join _ _ = error absurd 

   bot (ElimVar x k) 
    = ElimVar x (bot k)
   bot (ElimBool { true : k1, false : k2 }) 
    = (ElimBool { true : bot k1, false : bot k2 })
   bot (ElimPair el) 
    = ElimPair (map bot el)
   bot (ElimList { nil: κ, cons: σ }) 
    = ElimList { nil: bot κ, cons: map bot σ}

   top (ElimVar x k) 
    = ElimVar x (top k)
   top (ElimBool { true : k1, false : k2 }) 
    = (ElimBool { true : top k1, false : top k2 })
   top (ElimPair el) 
    = ElimPair (map top el)
   top (ElimList { nil: κ, cons: σ }) 
    = ElimList { nil: top κ, cons: map top σ}

data Module = Module (List (Either Def RecDefs))
