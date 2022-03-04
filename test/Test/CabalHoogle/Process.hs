{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.CabalHoogle.Process where

import           CabalHoogle.Process

import           CabalHoogle.P

import           Test.QuickCheck

prop_clean xs =
  counterexample "contained \\b or \\r" $
  all (`notElem` dirtyChars) (cleanLines [] xs)

dirtyChars :: [Char]
dirtyChars = "\b\r"

pure []
tests = $quickCheckAll
