
data Elim k =
   ElimVar Var k |
   ElimBool { true :: k, false :: k } |
   ElimPair (Elim (Elim k)) |
   ElimList { nil :: k, cons :: Elim (Elim k) }

instance elimFunctor :: Functor Elim where
   map f (ElimVar x κ)                       = ElimVar x (f κ)
   map f (ElimBool { true: κ, false: κ' })   = ElimBool { true: f κ, false: f κ' }
   map f (ElimPair σ)                        = ElimPair $ map (map f) σ
   map f (ElimList { nil: κ, cons: σ })      = ElimList { nil: f κ, cons: map (map f) σ }

instance elimLattice :: Lattice k => Lattice (Elim k) where
   maybeMeet (ElimVar x κ) (ElimVar x' κ') = do
      x'' <- x ≟ x'
      κ'' <- κ ∧? κ'
      pure (ElimVar x'' κ'')
   maybeMeet (ElimBool { true : κ1, false : κ2 }) (ElimBool { true : κ1', false : κ2' }) = do
      κ1'' <- κ1 ∧? κ1'
      κ2'' <- κ2 ∧? κ2'
      pure (ElimBool {true : κ1'', false : κ2''})
   maybeMeet (ElimPair σ) (ElimPair σ') = do
      σ'' <- σ ∧? σ'
      pure (ElimPair σ'')
   maybeMeet (ElimList { nil: κ1, cons: σ1 }) (ElimList { nil: κ2, cons: σ2 }) = do
      κ <- κ1 ∧? κ2
      σ <- σ1 ∧? σ2
      pure (ElimList { nil: κ, cons: σ })
   maybeMeet _ _ = Nothing

   maybeJoin (ElimVar x κ) (ElimVar x' κ') = do
      x'' <- x ≟ x'
      κ'' <- κ ∨? κ'
      pure (ElimVar x'' κ'')
   maybeJoin (ElimBool { true : κ1, false : κ2 }) (ElimBool { true : κ1', false : κ2' }) = do
      κ1'' <- κ1 ∨? κ1'
      κ2'' <- κ2 ∨? κ2'
      pure (ElimBool {true : κ1'', false : κ2''})
   maybeJoin (ElimPair σ) (ElimPair σ') = do
      σ'' <- σ ∨? σ'
      pure (ElimPair σ'')
   maybeJoin (ElimList { nil: κ1, cons: σ1 }) (ElimList { nil: κ2, cons: σ2 }) = do
      κ <- κ1 ∨? κ2
      σ <- σ1 ∨? σ2
      pure (ElimList { nil: κ, cons: σ })
   maybeJoin _ _ = Nothing

   bot (ElimVar x κ)
      = ElimVar x (bot κ)
   bot (ElimBool { true : κ1, false : κ2 })
      = (ElimBool { true : bot κ1, false : bot κ2 })
   bot (ElimPair σ)
      = ElimPair (bot σ)
   bot (ElimList { nil: κ, cons: σ })
      = ElimList { nil: bot κ, cons: bot σ}

   top (ElimVar x κ)
      = ElimVar x (top κ)
   top (ElimBool { true : κ1, false : κ2 })
      = (ElimBool { true : top κ1, false : top κ2 })
   top (ElimPair σ)
      = ElimPair (top σ)
   top (ElimList { nil: κ, cons: σ })
      = ElimList { nil: top κ, cons: top σ}
