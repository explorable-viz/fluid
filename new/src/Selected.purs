module Selected where

import Prelude hiding (join)
import Util (error, absurd)
import Data.Maybe (Maybe(..))

class Lattice a where
   maybeMeet   :: a -> a -> Maybe a
   maybeJoin   :: a -> a -> Maybe a
   top         :: a -> a
   bot         :: a -> a

join :: forall a. Lattice a => a -> a -> a
join p q = case p ∨? q of Just r   -> r
                          Nothing  -> error absurd

meet :: forall a. Lattice a => a -> a -> a
meet p q = case p ∧? q of Just r   -> r
                          Nothing  -> error absurd

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨
infix 7 maybeMeet as ∧?
infix 6 maybeJoin as ∨?

data Selected = Top | Bot -- maybe tt, ff would be better

derive instance eqSelected :: Eq Selected

instance latticeSelected :: Lattice Selected where
   maybeMeet Top Top = Just Top
   maybeMeet _ _     = Just Bot
   maybeJoin Bot Bot = Just Bot
   maybeJoin _ _     = Just Top
   top _ = Top
   bot _ = Bot
