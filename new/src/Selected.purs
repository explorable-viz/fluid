module Selected where

import Prelude hiding (join)

class Lattice a where
    meet :: a -> a -> a
    join :: a -> a -> a
    top  :: a -> a
    bot  :: a -> a

data Selected = Top | Bot -- maybe tt, ff would be better

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨

derive instance eqSelected :: Eq Selected

instance latticeSelected :: Lattice Selected where
    meet Top Top = Top
    meet _ _ = Bot
    join Bot Bot = Bot
    join _ _ = Top
    top _ = Top
    bot _ = Bot 
