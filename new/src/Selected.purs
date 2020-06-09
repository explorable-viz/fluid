module Selected where

import Prelude hiding (join)
import Util (error, absurd)
import Data.Maybe (Maybe(..))

class Lattice a where
    maybeMeet   :: a -> a -> Maybe a
    meet        :: a -> a -> a
    maybeJoin   :: a -> a -> Maybe a
    join        :: a -> a -> a
    top         :: a -> a
    bot         :: a -> a

data Selected = Top | Bot -- maybe tt, ff would be better

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨
infix 7 maybeMeet as ∧?
infix 6 maybeJoin as ∨?

derive instance eqSelected :: Eq Selected

instance latticeSelected :: Lattice Selected where
    maybeMeet Top Top = Just Top
    maybeMeet _ _     = Just Bot
    meet α α' = case α ∧? α' of Just α'' -> α''
                                Nothing  -> error absurd
    maybeJoin Bot Bot = Just Bot
    maybeJoin _ _     = Just Top
    join α α' = case α ∨? α' of Just α'' -> α''
                                Nothing  -> error absurd
    top _ = Top
    bot _ = Bot 
