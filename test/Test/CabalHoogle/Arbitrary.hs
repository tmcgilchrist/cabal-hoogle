{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.CabalHoogle.Arbitrary where

import qualified Data.List as DL
import qualified Data.Text as T

import           Hedgehog.Corpus (muppets)

import           CabalHoogle.Cabal.Types
import           CabalHoogle.Package

import           CabalHoogle.P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


newtype EqCabalError =
  EqCabalError {
      unCabalError :: CabalError
    } deriving (Show)

instance Eq EqCabalError where
  (==) x y =
    show x == show y

instance Arbitrary PackageName where
  arbitrary = do
    name <- T.intercalate "-" <$> listOf1 (elements muppets)
    pure (mkPackageName name)

instance Arbitrary Version where
  arbitrary = do
    len <- choose (1, 4)
    makeVersion <$> vectorOf len (choose (0, 100))
  shrink ver =
     let
       xs = versionNumbers ver
       len = DL.length xs
     in
      if len > 1
        then [makeVersion $ DL.take (len - 1) xs]
         else []

instance Arbitrary PackageId where
  arbitrary =
    PackageId <$>
      arbitrary <*>
      arbitrary

