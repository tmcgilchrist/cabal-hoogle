{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import           BuildInfo_cabal_hoogle
import           DependencyInfo_cabal_hoogle

import           Control.Concurrent (setNumCapabilities)
import           Control.Monad.Trans.Either (EitherT)

import           Data.FileEmbed (dummySpaceWith)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           GHC.Conc (getNumProcessors)

import           CabalHoogle.Error (CabalHoogleError (..), renderCabalHoogleError)
import           CabalHoogle.Home (getCabalHome)
import           CabalHoogle.Hoogle (buildRootHoogleDirectory, hoogle)
import           CabalHoogle.IO (doesDirectoryExist, lookupEnv, removeDirectoryRecursive)
import           CabalHoogle.Options.Applicative (CommandFields, Mod, Parser, cli, command', help, metavar, orDie,
                                                  strArgument, subparser)
import           CabalHoogle.Options.Git
import           CabalHoogle.P
import           CabalHoogle.Path ((</>))
import           CabalHoogle.Process

import           System.IO (BufferMode (..), IO, hSetBuffering, stderr, stdout)

------------------------------------------------------------------------

main :: IO ()
main = do
  nprocs <- getNumProcessors
  setNumCapabilities nprocs
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli "cabal-hoogle" (T.unpack gitRev) cabalVersion dependencyInfo parser runOrDie

runOrDie :: CabalHoogleCommand -> IO ()
runOrDie =
  orDie renderCabalHoogleError . run

------------------------------------------------------------------------

data CabalHoogleCommand
  = CabalHoogleClean
  | CabalHoogle [Argument]
  deriving (Eq, Show)

run :: CabalHoogleCommand -> EitherT CabalHoogleError IO ()
run = \case
  CabalHoogleClean ->
    cabalHoogleClean
  CabalHoogle args -> do
    cabalHoogle args

parser :: Parser CabalHoogleCommand
parser =
  subparser (mconcat commands)

commands :: [Mod CommandFields CabalHoogleCommand]
commands =
  [ command'
      "clean"
      "Clean up after build (only cleans files created by this program)."
      (pure CabalHoogleClean),
    command'
      "hoogle"
      "Run a hoogle query across the local dependencies"
      (CabalHoogle <$> many pCabalArgs)
  ]

pCabalArgs :: Parser Argument
pCabalArgs =
  strArgument $
    metavar "CABAL_ARGUMENTS"
      <> help "Extra arguments to pass on to cabal."

------------------------------------------------------------------------

cabalHoogleClean :: EitherT CabalHoogleError IO ()
cabalHoogleClean = do
  -- Remove hoogle cache in ~/.cabal/hoogle
  cabalHome <- getCabalHome
  removeDirectoryRecursive (cabalHome </> "hoogle")

  -- Remove local hoogle directory in dist-newstyle
  whenM (doesDirectoryExist buildRootHoogleDirectory) $
    removeDirectoryRecursive buildRootHoogleDirectory

  pure ()

cabalHoogle :: [Argument] -> EitherT CabalHoogleError IO ()
cabalHoogle args = do
  hkg <- fromMaybe "https://hackage.haskell.org/package" <$> lookupEnv "HACKAGE"
  hoogle hkg args

-- | Provide the git revision at compilation time (if possible)
--
-- Used to provide a git revision in cli programs
gitRev :: Text
gitRev
    | gitRevEmbed /= zeroRev = gitRevEmbed
    | T.null fromGit = zeroRev
    | otherwise = fromGit
  where
    -- Git revision embedded after compilation using
    -- Data.FileEmbed.injectWith. If nothing has been injected,
    -- this will be filled with 0 characters.
    gitRevEmbed :: Text
    gitRevEmbed = T.decodeUtf8 $(dummySpaceWith "gitrev" 40)

    -- Git revision found during compilation by running git. If
    -- git could not be run, then this will be empty.
    fromGit = T.strip (T.pack $(gitRevFromGit))

    zeroRev :: Text
    zeroRev = "0000000000000000000000000000000000000000"

