{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module CabalHoogle.Home
  ( getCabalHome
  , getCabalHoogleDir
  , ensureCabalDir
  ) where

import qualified Data.Text as T

import           CabalHoogle.IO
import           CabalHoogle.P
import           CabalHoogle.Path

getCabalHome :: MonadIO m => m Directory
getCabalHome = do
  mhome <- lookupEnv "CABAL_HOME"
  bind canonicalizePath $
    case mhome of
      Just home ->
        return home
      Nothing ->
        (</> T.pack ".cabal") `liftM` getHomeDirectory

getCabalHoogleDir :: MonadIO m => Directory -> m Directory
getCabalHoogleDir path = do
  home <- getCabalHome
  return (home </> path)

ensureCabalDir :: MonadIO m => Directory -> m Directory
ensureCabalDir path = do
  path' <- getCabalHoogleDir path
  createDirectoryIfMissing True path'
  return path'

