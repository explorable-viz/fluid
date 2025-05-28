module Link where

import Prelude hiding (join)

import App.Util (SelStates, 𝕊, Selectable)
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import DataType (cLink)
import Lattice (bot, join)
import Primitive (typeError)
import Util ((×))
import Val (BaseVal(..), Val(..))

data Link a = Link (Val a) (Selectable String)

getText :: Link (SelStates 𝕊) -> Selectable String
getText (Link v (s × _)) = s × (foldr join bot v)

linkContents :: ∀ a. Link a -> String
linkContents (Link _ (s × _)) = s

unpackLink :: BaseVal (SelStates 𝕊) -> Link (SelStates 𝕊)
unpackLink (Constr c (Val α doc v : (Val α' _ (Str s) : Nil))) | c == cLink = Link (Val α doc v) (s × α')
unpackLink v = typeError v "Link"
