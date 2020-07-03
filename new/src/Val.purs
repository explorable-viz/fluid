module Val where

import Prelude hiding (absurd, top)
import Control.Apply (lift2)
import Data.Either (note)
import Data.List (List)
import Data.Maybe (Maybe(..))
import DataType (Ctr)
import Expr (Elim', RecDefs', Var)
import Lattice (class Selectable2, Selected, maybeZipWith, maybeZipWithList)
import Util (MayFail, (≟), report)

data Primitive =
   IntOp (Int -> Val) -- one constructor for each primitive type we care about

data RawVal' a =
   Int Int |
   Str String |
   Constr Ctr (List (Val' a)) |
   Closure (Env' a) (RecDefs' a) (Elim' a) |
   Primitive Primitive

data Val' a = Val a (RawVal' a)

derive instance functorRawVal :: Functor RawVal'
derive instance functorVal :: Functor Val'

type Val = Val' Selected
type RawVal = RawVal' Selected

val :: RawVal -> Val
val = Val false

instance selectable2Val :: Selectable2 Val' where
   maybeZipWith f (Val α r) (Val α' r') = Val <$> pure (α `f` α') <*> maybeZipWith f r r'

instance selectable2RawVal :: Selectable2 RawVal' where
   maybeZipWith f (Int x) (Int x')                   = Int <$> x ≟ x'
   maybeZipWith f (Str s) (Str s')                   = Str <$> s ≟ s'
   maybeZipWith f (Constr c vs) (Constr c' vs') =
      Constr <$> c ≟ c' <*> maybeZipWithList f vs vs'
   maybeZipWith f (Closure ρ δ σ) (Closure ρ' δ' σ') =
      Closure <$> maybeZipWith f ρ ρ' <*> maybeZipWithList f δ δ' <*> maybeZipWith f σ σ'
   maybeZipWith f (Primitive φ) (Primitive φ')       = pure $ Primitive φ -- should require φ == φ'
   maybeZipWith _ _ _                                = Nothing

data Bind' a = Bind Var (Maybe (Val' a))
type Bind = Bind' Selected
data Env' a = Empty | Extend (Env' a) (Bind' a)
type Env = Env' Selected

infix 6 Bind as ↦
infixl 5 Extend as :+:
infixl 5 update as ◃

find :: Var -> Env -> MayFail Val
find x' Empty  = report $ "variable " <> x' <> " not found"
find x' (xs :+: x ↦ v)
   | x == x'   = note "TODO: should map to a bottom value" v
   | otherwise = find x' xs

foldEnv :: forall a . (Bind -> a -> a) -> a -> Env -> a
foldEnv f z (ρ :+: x ↦ v)   = f (x ↦ v) (foldEnv f z ρ)
foldEnv _ z Empty           = z

update :: Env -> Bind -> Env
update Empty _ = Empty
update (xs :+: x ↦ v) (x' ↦ v')
   | x == x'    = xs :+: x' ↦ v'
   | otherwise  = (update xs $ x' ↦ v') :+: x ↦ v

derive instance functorBind :: Functor Bind'
derive instance functorEnv :: Functor Env'

instance semigroupEnv :: Semigroup (Env' a) where
   append m Empty          = m
   append m (Extend m' kv) = Extend (append m m') kv

instance monoidEnv :: Monoid (Env' a) where
   mempty = Empty

instance selectableEnv :: Selectable2 Env' where
   maybeZipWith _ Empty Empty                              = pure Empty
   maybeZipWith f (Extend m (x ↦ v)) (Extend m' (y ↦ v'))
      = Extend <$> maybeZipWith f m m' <*> ((↦) <$> x ≟ y <*> lift2 (maybeZipWith f) v v')
   maybeZipWith _ _ _                                      = Nothing
