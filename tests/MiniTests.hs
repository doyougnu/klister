{-# LANGUAGE OverloadedStrings #-}

-- | Tiny module to hold mini-test strings. Some mini-test strings are split
-- strings which conflict with the CPP pragma. See
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/phases.html#cpp-and-string-gaps

module MiniTests where

import Data.Text

trivialUserMacro :: Text
trivialUserMacro = "[let-syntax \n\
                   \  [m [lambda [_] \n\
                   \       [pure [quote [lambda [x] x]]]]] \n\
                   \  m]"

unboundVarLet :: Text
unboundVarLet = "[let-syntax \
                \  [m [lambda [_] \
                \       [pure [quote [lambda [x] x]]]]] \
                \  anyRandomWord]"
