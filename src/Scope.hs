{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scope where

import Data.Data (Data)
import Control.Lens

import Util.Key

-- Int should be enough for now - consider bumping to something like int64
newtype Scope = Scope { scopeNum :: Int }
  deriving newtype (Eq, Ord, Show, HasKey)
  deriving stock Data
makeLenses ''Scope

