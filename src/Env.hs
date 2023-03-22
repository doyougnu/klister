{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DerivingStrategies #-}

module Env (Env, empty, insert, singleton, lookup, lookupIdent, lookupVal, toList, fromList, named) where

import Prelude hiding (lookup)

import Control.Lens
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)

import Syntax (Ident, Stx(..))

import Util.Key

-- | an environment is a mapping to a map of @Ident@s and some value. We keep
-- 'v' as a phantom to stay honest
newtype Env v a = Env (IntMap (Ident, a))
  deriving newtype (Eq, Monoid, Semigroup, Show)
  deriving stock Functor

empty :: Env v a
empty = Env mempty

toList :: HasKey v => Env v a -> [(v, Ident, a)]
toList (Env env) = [(fromKey x, n, v) | (x, (n, v)) <- IM.toList env]

fromList :: HasKey v => [(v, Ident, a)] -> Env v a
fromList vars = Env (IM.fromList [(getKey x, (n, v)) | (x, n, v) <- vars])

singleton :: HasKey v => v -> Ident -> a -> Env v a
singleton x n v = Env (IM.singleton (getKey x) (n, v))

insert :: HasKey v => v -> Ident -> a -> Env v a -> Env v a
insert x n v (Env env) = Env (IM.insert (getKey x) (n, v) env)

lookup :: HasKey v => v -> Env v a -> Maybe (Ident, a)
lookup x (Env env) = IM.lookup (getKey x) env

lookupVal :: HasKey v => v -> Env v a -> Maybe a
lookupVal x env = snd <$> lookup x env

lookupIdent :: HasKey v => v -> Env v a -> Maybe Ident
lookupIdent x env = fst <$> lookup x env

named :: HasKey v => Text -> Env v a -> [(v, a)]
named n (Env xs) = [(fromKey x, a) | (x, (Stx _ _ n', a)) <- IM.toList xs, n == n']

type instance Index (Env v a) = v
type instance IxValue (Env v a) = (Ident, a)

instance HasKey v => Ixed (Env v a) where
  ix var f (Env env) = Env <$> ix (getKey var) f env
  {-# INLINE ix #-}

instance HasKey v => At (Env v a) where
  at x f (Env env) = Env <$> at (getKey x) f env
  {-# INLINE at #-}
