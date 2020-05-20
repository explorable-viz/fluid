module Selected where

import Prelude hiding (join)

data Selected = Top | Bot -- maybe tt, ff would be better

meet :: Selected -> Selected -> Selected
meet Top Top = Top
meet _ _ = Bot

join :: Selected -> Selected -> Selected
join Bot Bot = Bot
join _ _ = Top

-- Give ∧ and ∨ same associativity and precedence as * and +
infixl 7 meet as ∧
infixl 6 join as ∨

derive instance eqSelected :: Eq Selected
