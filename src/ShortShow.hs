module ShortShow where

import Data.Text
import qualified Data.List as List
import qualified Data.Sequence as Seq
import Data.Foldable

import Unique


class ShortShow a where
  shortShow :: a -> String

instance ShortShow Text where
  shortShow = show

instance ShortShow Unique where
  shortShow = show . hashUnique

instance (ShortShow a, ShortShow b) => ShortShow (a, b) where
  shortShow (x, y)
    = "("
   ++ shortShow x
   ++ ", "
   ++ shortShow y
   ++ ")"
instance (ShortShow a, ShortShow b, ShortShow c) => ShortShow (a, b, c) where
  shortShow (x, y, z)
    = "("
   ++ shortShow x
   ++ ", "
   ++ shortShow y
   ++ ", "
   ++ shortShow z
   ++ ")"

instance ShortShow a => ShortShow [a] where
  shortShow xs
    = "["
   ++ List.intercalate ", " (fmap shortShow xs)
   ++ "]"

instance ShortShow a => ShortShow (Seq.Seq a) where
  shortShow xs
    = "["
   ++ List.intercalate ", " (toList $ fmap shortShow xs)
   ++ "]"
