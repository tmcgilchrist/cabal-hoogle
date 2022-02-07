{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.CabalHoogle.Package where

import           Test.CabalHoogle.Tripping (tripping)

import           CabalHoogle.Package

import           CabalHoogle.P

import           System.IO (IO)

import           Test.CabalHoogle.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_roundtrip_PackageId :: PackageId -> Property
prop_roundtrip_PackageId =
  tripping renderPackageId parsePackageId

return []
tests :: IO Bool
tests =
  $quickCheckAll
