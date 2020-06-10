module Selected where

import Prelude hiding (absurd, join)
import Data.Maybe (Maybe(..))
import Util (fromJust)

class Lattice a where
    maybeMeet   :: a -> a -> Maybe a
    meet        :: a -> a -> a
    maybeJoin   :: a -> a -> Maybe a
    join        :: a -> a -> a
    top         :: a -> a
    bot         :: a -> a

meetDefault :: forall a . Lattice a => a -> a -> a
meetDefault a a' = fromJust $ maybeMeet a a'

joinDefault :: forall a . Lattice a => a -> a -> a
joinDefault a a' = fromJust $ maybeJoin a a'

data Selected = Top | Bot -- maybe tt, ff would be better

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨
infix 7 maybeMeet as ∧?
infix 6 maybeJoin as ∨?

derive instance eqSelected :: Eq Selected

instance latticeSelected :: Lattice Selected where
    meet Top Top = Top
    meet _ _     = Bot

    maybeMeet α α' = Just $ α ∧ α'

    join Bot Bot = Bot
    join _ _     = Top

    maybeJoin α α' = Just $ α ∨ α'

    top _ = Top
    bot _ = Bot
