module Val where

import Prelude hiding (absurd, top)
import Data.List (List)
import Data.Maybe (Maybe(..))
import DataType (Ctr)
import Expr (Elim', RecDefs', Var)
import Lattice (class MaybeZippable, Selected, maybeZipWith, maybeZipWithList)
import Util (MayFail, type (×), (×), (≟), report)

data Primitive =
   IntOp (Int -> Val) -- one constructor for each primitive type we care about

data RawVal' a =
   Int Int |
   Str String |
   Constr Ctr (List (Val' a)) |
   Closure (Env' a) (RecDefs' a) (Elim' a) |
   Primitive Primitive

data Val' a = Val a (RawVal' a)

type Val = Val' Selected
type RawVal = RawVal' Selected

val :: RawVal -> Val
val = Val false

data Bind' a = Bind Var (Val' a)
type Bind = Bind' Selected
data Env' a = Empty | Extend (Env' a) (Bind' a)
type Env = Env' Selected

infix 6 Bind as ↦
infixl 5 Extend as :+:
infixl 5 update as ◃

find :: Var -> Env -> MayFail Val
find x Empty  = report $ "variable " <> x <> " not found"
find x (xs :+: x' ↦ v)
   | x == x'   = pure v
   | otherwise = find x xs

foldEnv :: forall a . (Bind -> a -> a) -> a -> Env -> a
foldEnv f z (ρ :+: x ↦ v)   = f (x ↦ v) (foldEnv f z ρ)
foldEnv _ z Empty           = z

update :: Env -> Bind -> Env
update Empty _ = Empty
update (xs :+: x ↦ v) (x' ↦ v')
   | x == x'    = xs :+: x' ↦ v'
   | otherwise  = (update xs $ x' ↦ v') :+: x ↦ v

splitAt :: Int -> Env -> Env × Env
splitAt n ρ
  | n <= 0     = ρ × Empty
  | otherwise  = splitAt' n ρ
    where
        splitAt' :: Int -> Env -> Env × Env
        splitAt' _  Empty        = Empty × Empty
        splitAt' 1  (ρ0 :+: xv)  = ρ0 × Extend Empty xv
        splitAt' m  (ρ0 :+: xv)  = ρ' × (ρ'' :+: xv)
         where
         ρ' × ρ'' = splitAt' (m - 1) ρ0

derive instance functorRawVal :: Functor RawVal'
derive instance functorVal :: Functor Val'

instance maybeZippableVal :: MaybeZippable Val' where
   maybeZipWith f (Val α r) (Val α' r') = Val <$> pure (α `f` α') <*> maybeZipWith f r r'

instance maybeZippableRawVal :: MaybeZippable RawVal' where
   maybeZipWith f (Int x) (Int x')                   = Int <$> x ≟ x'
   maybeZipWith f (Str s) (Str s')                   = Str <$> s ≟ s'
   maybeZipWith f (Constr c vs) (Constr c' vs') =
      Constr <$> c ≟ c' <*> maybeZipWithList f vs vs'
   maybeZipWith f (Closure ρ δ σ) (Closure ρ' δ' σ') =
      Closure <$> maybeZipWith f ρ ρ' <*> maybeZipWithList f δ δ' <*> maybeZipWith f σ σ'
   maybeZipWith f (Primitive φ) (Primitive φ')       = pure $ Primitive φ -- should require φ == φ'
   maybeZipWith _ _ _                                = Nothing

derive instance functorBind :: Functor Bind'
derive instance functorEnv :: Functor Env'

instance semigroupEnv :: Semigroup (Env' a) where
   append m Empty          = m
   append m (Extend m' kv) = Extend (append m m') kv

instance monoidEnv :: Monoid (Env' a) where
   mempty = Empty

instance maybeZippableEnv :: MaybeZippable Env' where
   maybeZipWith _ Empty Empty                              = pure Empty
   maybeZipWith f (Extend m (x ↦ v)) (Extend m' (y ↦ v'))
      = Extend <$> maybeZipWith f m m' <*> ((↦) <$> x ≟ y <*> maybeZipWith f v v')
   maybeZipWith _ _ _                                      = Nothing
