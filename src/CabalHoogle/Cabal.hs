{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalHoogle.Cabal
  ( cabal
  , cabal_
  , cabalPlan
  , cabalAnnihilate
  , cabalFind

  , CabalError(..)
  , renderCabalError
  ) where

import qualified Data.Text as T

import           CabalHoogle.P
import           CabalHoogle.Path
import           CabalHoogle.Process
import           Control.Monad.Trans.Either (EitherT)

import           System.IO (IO)

------------------------------------------------------------------------

cabal :: ProcessResult a => Argument -> [Argument] -> EitherT CabalError IO a
cabal cmd args = call CabalProcessError "cabal" (cmd : args)

cabalPlan :: ProcessResult a => Argument -> [Argument] -> EitherT CabalError IO a
cabalPlan cmd args = call CabalProcessError "cabal-plan" (cmd : args)

cabalFind :: ProcessResult a => Argument -> [Argument] -> EitherT CabalError IO a
cabalFind cmd args = call CabalProcessError "find" (cmd : args)

cabal_ :: Argument -> [Argument] -> EitherT CabalError IO ()
cabal_ cmd args = do
  PassErr <- cabal cmd args
  pure ()

cabalAnnihilate :: Argument -> [Argument] -> EitherT CabalError IO ()
cabalAnnihilate cmd args = do
  PassErrAnnihilate <- cabal cmd args
  pure ()

------------------------------------------------------------------------

data CabalError =
    CabalProcessError ProcessError
  | CabalFileNotFound Directory
  | CabalMultipleFilesFound Directory [File]
  | CabalNotInstalled
    deriving (Show)

renderCabalError :: CabalError -> Text
renderCabalError = \case
  CabalProcessError e ->
    renderProcessError e

  CabalFileNotFound dir ->
    "Could not find .cabal file in: " <> dir

  CabalMultipleFilesFound dir files ->
    "Found multiple possible .cabal files (" <> T.intercalate ", " files <> ") in: " <> dir

  CabalNotInstalled ->
    mconcat
      [ "cabal-install is not installed."
      , "\nYou can download it from https://www.haskell.org/cabal/download.html" ]

