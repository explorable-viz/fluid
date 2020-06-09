module Expr where


import Prelude (class Functor, Unit, map, ($), (<$>), (<<<), bind, pure)
import Bindings (Var)
import Data.List (List) 
import Data.Either (Either)
import Control.Bind ((>>=))
import Selected (class Lattice, Selected(..), bot, join, meet, top, maybeJoin, maybeMeet)
import Util (absurd, error, (=?), isEq)
import Data.Maybe (Maybe(..))

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
   maybeJoin (Expr α (Var x)) (Expr α' (Var x')) 
    = do 
    α'' <- maybeJoin α α' 
    x'' <- isEq x x' 
    pure (Expr α'' (Var x''))
   maybeJoin (Expr α (Op op)) (Expr α' (Op op')) 
    = do 
    α''  <- maybeJoin α α' 
    op'' <- isEq op op' 
    pure (Expr α'' (Op op''))
   maybeJoin (Expr α (Int n)) (Expr α' (Int n')) 
    = do 
    α'' <- maybeJoin α   α' 
    n'' <- isEq n n' 
    pure (Expr α'' (Int n'')) 
   maybeJoin (Expr α (Str s)) (Expr α' (Var s'))
    = do 
    α'' <- maybeJoin α   α' 
    s'' <- isEq s s' 
    pure (Expr α'' (Str s'')) 
   maybeJoin (Expr α False) (Expr α' False) 
    = do 
    α'' <- maybeJoin α   α' 
    pure (Expr α'' False) 
   maybeJoin (Expr α False) (Expr α' True) 
    = do 
    α'' <- maybeJoin α α' 
    pure (Expr α'' True) 
   maybeJoin (Expr α True) (Expr α' False) 
    = do 
    α'' <- maybeJoin α α' 
    pure (Expr α'' True) 
   maybeJoin (Expr α True) (Expr α' True) 
    = do 
    α'' <- maybeJoin α α' 
    pure (Expr α'' True) 
   maybeJoin (Expr α (Pair e1 e1')) (Expr α' (Pair e2 e2'))
    = do 
    α'' <- maybeJoin α   α' 
    e   <- maybeJoin e1  e2
    e'  <- maybeJoin e1' e2'
    pure (Expr α'' (Pair e e'))
   maybeJoin (Expr α Nil) (Expr α' Nil) 
    = do 
    α'' <- (maybeJoin α α') 
    pure (Expr α'' Nil)
   maybeJoin (Expr α (Cons e1 e1')) (Expr α' (Cons e2 e2'))
    = do 
    α'' <- maybeJoin α   α' 
    e   <- maybeJoin e1  e2
    e'  <- maybeJoin e1' e2'
    pure (Expr α'' (Cons e e'))
   maybeJoin _ _ = Nothing

   join e e' = case maybeJoin e e' of Just e'' -> e''
                                      Nothing  -> error absurd
      
   maybeMeet (Expr α (Var x)) (Expr α' (Var x')) 
    = do 
    α'' <- maybeMeet α α' 
    x'' <- isEq x x' 
    pure (Expr α'' (Var x''))
   maybeMeet (Expr α (Op op)) (Expr α' (Op op')) 
    = do 
    α''  <- maybeMeet α α' 
    op'' <- isEq op op' 
    pure (Expr α'' (Op op''))
   maybeMeet (Expr α (Int n)) (Expr α' (Int n')) 
    = do 
    α'' <- maybeMeet α   α' 
    n'' <- isEq n n' 
    pure (Expr α'' (Int n'')) 
   maybeMeet (Expr α (Str s)) (Expr α' (Var s'))
    = do 
    α'' <- maybeMeet α   α' 
    s'' <- isEq s s' 
    pure (Expr α'' (Str s'')) 
   maybeMeet (Expr α False) (Expr α' False) 
    = do 
    α'' <- maybeMeet α   α' 
    pure (Expr α'' False) 
   maybeMeet (Expr α False) (Expr α' True) 
    = do 
    α'' <- maybeMeet α α' 
    pure (Expr α'' True) 
   maybeMeet (Expr α True) (Expr α' False) 
    = do 
    α'' <- maybeMeet α α' 
    pure (Expr α'' True) 
   maybeMeet (Expr α True) (Expr α' True) 
    = do 
    α'' <- maybeMeet α α' 
    pure (Expr α'' True) 
   maybeMeet (Expr α (Pair e1 e1')) (Expr α' (Pair e2 e2'))
    = do 
    α'' <- maybeMeet α   α' 
    e   <- maybeMeet e1  e2
    e'  <- maybeMeet e1' e2'
    pure (Expr α'' (Pair e e'))
   maybeMeet (Expr α Nil) (Expr α' Nil) 
    = do 
    α'' <- (maybeMeet α α') 
    pure (Expr α'' Nil)
   maybeMeet (Expr α (Cons e1 e1')) (Expr α' (Cons e2 e2'))
    = do 
    α'' <- maybeMeet α   α' 
    e   <- maybeMeet e1  e2
    e'  <- maybeMeet e1' e2'
    pure (Expr α'' (Cons e e'))
   maybeMeet _ _ = error absurd

   meet e e' = case maybeMeet e e' of Just e'' -> e''
                                      Nothing  -> error absurd
      
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
   maybeMeet (ElimVar x k) (ElimVar x' k') 
    = do 
    x'' <- isEq x x' 
    k'' <- maybeMeet k k'
    pure (ElimVar x'' k'')
   maybeMeet (ElimBool { true : k1, false : k2 }) (ElimBool { true : k1', false : k2' })
    = do 
    k1'' <- maybeMeet k1 k1'
    k2'' <- maybeMeet k2 k2'
    pure (ElimBool {true : k1'', false : k2''})
   maybeMeet (ElimPair el) (ElimPair el') 
    = hoistMaybe $ ElimPair (map (\k -> map (\k' -> maybeMeet k k') (ElimPair el')) (ElimPair el))
   maybeMeet (ElimList { nil: κ1, cons: σ1 }) (ElimList { nil: κ2, cons: σ2 })
    = do 
    κ <- maybeMeet κ1 κ2 
    σ <- maybeσ
    pure $ ElimList { nil: κ, cons: σ }
    where
     maybeσ = case σ1, σ2 of 
                  ElimVar x k, ElimVar x' k' 
                     -> do 
                     x'' <- isEq x x'
                     k'' <- maybeMeet k k' 
                     pure (ElimVar x'' k'') 
                  ElimBool { true : k1, false : k2 }, ElimBool { true : k1', false : k2' } 
                     -> do 
                     k1'' <- maybeMeet k1 k1'
                     k2'' <- maybeMeet k2 k2'
                     pure (ElimBool {true : k1'', false : k2''})                        
                  ElimPair el, ElimPair el' 
                     -> hoistMaybe $ ElimPair (map (\k -> map (\k' -> maybeMeet k k') (ElimPair el')) (ElimPair el))
                  ElimList { nil: κ1', cons: σ1' }, ElimList { nil: κ2', cons: σ2' }
                     -> do 
                     κ  <- maybeMeet κ1' κ2'
                     σ' <- maybeMeet σ1' σ2' 
                     pure $ ElimList { nil: κ, cons: σ' }
                  _, _ -> Nothing
   maybeMeet _ _ = Nothing

   meet σ σ' = case maybeMeet σ σ' of Just σ'' -> σ''
                                      Nothing  -> error absurd


   maybeJoin (ElimVar x k) (ElimVar x' k') 
    = do 
    x'' <- isEq x x' 
    k'' <- maybeJoin k k'
    pure (ElimVar x'' k'')
   maybeJoin (ElimBool { true : k1, false : k2 }) (ElimBool { true : k1', false : k2' })
    = do 
    k1'' <- maybeJoin k1 k1'
    k2'' <- maybeJoin k2 k2'
    pure (ElimBool {true : k1'', false : k2''})
   maybeJoin (ElimPair el) (ElimPair el') 
    = hoistMaybe $ ElimPair (map (\k -> map (\k' -> maybeJoin k k') (ElimPair el')) (ElimPair el))
   maybeJoin (ElimList { nil: κ1, cons: σ1 }) (ElimList { nil: κ2, cons: σ2 })
    = do 
    κ <- maybeJoin κ1 κ2 
    σ <- maybeσ
    pure $ ElimList { nil: κ, cons: σ }
    where
     maybeσ = case σ1, σ2 of 
                  ElimVar x k, ElimVar x' k' 
                     -> do 
                     x'' <- isEq x x'
                     k'' <- maybeJoin k k' 
                     pure (ElimVar x'' k'') 
                  ElimBool { true : k1, false : k2 }, ElimBool { true : k1', false : k2' } 
                     -> do 
                     k1'' <- maybeJoin k1 k1'
                     k2'' <- maybeJoin k2 k2'
                     pure (ElimBool {true : k1'', false : k2''})                        
                  ElimPair el, ElimPair el' 
                     -> hoistMaybe $ ElimPair (map (\k -> map (\k' -> maybeJoin k k') (ElimPair el')) (ElimPair el))
                  ElimList { nil: κ1', cons: σ1' }, ElimList { nil: κ2', cons: σ2' }
                     -> do 
                     κ  <- maybeJoin κ1' κ2'
                     σ' <- maybeJoin σ1' σ2' 
                     pure $ ElimList { nil: κ, cons: σ' }
                  _, _ -> Nothing
   maybeJoin _ _ = Nothing

   join σ σ' = case maybeJoin σ σ' of Just σ'' -> σ''
                                      Nothing  -> error absurd

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

hoistMaybe :: forall k . Elim (Maybe k) -> Maybe (Elim k)
hoistMaybe (ElimVar x (Just κ)) = Just $ ElimVar x κ
hoistMaybe (ElimBool { true: Just κ, false: Just κ' }) = Just $ ElimBool { true: κ, false: κ' }
hoistMaybe (ElimPair σ) = hoistMaybe (hoistMaybe <$> σ) >>= Just <<< ElimPair
hoistMaybe (ElimList { nil: Just κ, cons: σ }) = do
   σ' <- hoistMaybe (hoistMaybe <$> σ)
   pure $ ElimList { nil: κ, cons: σ' }
hoistMaybe _ = Nothing

data Module = Module (List (Either Def RecDefs))
