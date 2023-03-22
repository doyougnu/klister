-- | Tiny module to wrap operations for IntMaps

module Util.Key
  (HasKey(..)
  ) where

class HasKey a where
  getKey :: a -> Int
