module Val where

import Prelude hiding (absurd, top)
import Data.List (List, zipWith)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Bindings (Bindings)
import DataType (Ctr)
import Expr (Elim, RecDefs)
import Lattice (class Selectable, Selected, mapα, maybeZipWithα)
import Util ((≟))

data Primitive =
   IntOp (Int -> Val) -- one constructor for each primitive type we care about

data RawVal =
   Int Int |
   Str String |
   Constr Ctr (List Val) |
   Closure Env RecDefs Elim |
   Primitive Primitive

data Val = Val Selected RawVal

val :: RawVal -> Val
val = Val false

type Env = Bindings Val

instance selectablePrimitive :: Selectable Primitive where
   mapα _ = identity
   maybeZipWithα f (IntOp op) (IntOp op') = pure $ IntOp op'

instance selectableVal :: Selectable Val where
   mapα f (Val α u)                       = Val (f α) (mapα f u)
   maybeZipWithα f (Val α r) (Val α' r')  = Val <$> pure (α `f` α') <*> maybeZipWithα f r r'

instance selectableRawVal :: Selectable RawVal where
   mapα _ (Int x)          = Int x
   mapα _ (Str s)          = Str s
   mapα f (Constr c es)    = Constr c $ map (mapα f) es
   mapα f (Closure ρ δ σ)  = Closure (mapα f ρ) (map (mapα f) δ) $ mapα f σ
   mapα f (Primitive φ)    = Primitive $ mapα f φ

   maybeZipWithα f (Int x) (Int x')                   = Int <$> x ≟ x'
   maybeZipWithα f (Str s) (Str s')                   = Str <$> s ≟ s'
   maybeZipWithα f (Constr c es) (Constr c' es') =
      Constr <$> c ≟ c' <*> sequence (zipWith (maybeZipWithα f) es es')
   maybeZipWithα f (Closure ρ δ σ) (Closure ρ' δ' σ') =
      Closure <$> maybeZipWithα f ρ ρ'
              <*> sequence (zipWith (maybeZipWithα f) δ δ') <*> maybeZipWithα f σ σ'
   maybeZipWithα f (Primitive φ) (Primitive φ')       = Primitive <$> maybeZipWithα f φ φ'
   maybeZipWithα _ _ _                                = Nothing
