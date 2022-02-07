{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalHoogle.Error
  ( CabalHoogleError (..)
  , renderCabalHoogleError
  , liftCabal
  ) where

import           Control.Monad.Trans.Bifunctor (firstT)
import           Control.Monad.Trans.Either (EitherT)

import           CabalHoogle.Cabal
import           CabalHoogle.P
import           CabalHoogle.Process

-- FIX Leaving this to make code cleanup easier, but ideally is a union of
-- sub-exceptions rather than this module being the root of most dependencies
data CabalHoogleError =
    CabalHoogleProcessError ProcessError
  | CabalHoogleCabalError CabalError
  | CabalHoogleParseError Text
  deriving (Show)

renderCabalHoogleError :: CabalHoogleError -> Text
renderCabalHoogleError = \case
  CabalHoogleProcessError e ->
    renderProcessError e

  CabalHoogleCabalError e ->
    renderCabalError e

  CabalHoogleParseError msg ->
    "Parse failed: " <> msg

liftCabal :: Functor m => EitherT CabalError m a -> EitherT CabalHoogleError m a
liftCabal =
  firstT CabalHoogleCabalError
