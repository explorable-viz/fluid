module Bindings2 where

import Data.Tuple (Tuple(..))
import Lattice (class Expandable, expand)
import Util (type (×), (≜))
import Util.SnocList (SnocList)

type Var = String -- newtype?

newtype Binding2 a = Binding2 (Var × a)
type Bindings2 a = SnocList (Binding2 a)

infix 6 Tuple as ↦

instance expandableBinding :: Expandable a => Expandable (Binding2 a) where
   expand (Binding2 (x ↦ v)) (Binding2 (x' ↦ v')) = Binding2 ((x ≜ x') ↦ expand v v')
