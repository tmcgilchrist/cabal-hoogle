{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CabalHoogle.Hoogle
  ( HooglePackagesSandbox (..)
  , HooglePackagesCached (..)
  , hoogle
  , joinHooglePackages
  , hoogleSourcePackages

  , buildRoot
  , buildRootHoogleDirectory
  ) where

import           Control.Monad.Trans.Either (EitherT, hoistEither, left, runEitherT)

import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           CabalHoogle.Cabal (cabalFind, cabalPlan, cabal_)
import           CabalHoogle.Error (CabalHoogleError (..), liftCabal, renderCabalHoogleError)
import           CabalHoogle.Home (ensureCabalDir)
import           CabalHoogle.IO (ListingOptions (RecursiveDepth), canonicalizePath, createDirectoryIfMissing,
                                 createSymbolicLink, doesFileExist, getDirectoryListing)
import           CabalHoogle.P
import           CabalHoogle.Package (PackageId (..), packageIdTuple, parsePackageId, renderPackageId, unPackageName)
import           CabalHoogle.Path (Directory, File, takeFileName, (</>))
import           CabalHoogle.Process (Argument, Hush (Hush), Out (Out, unOut), call, call_)

import           System.IO (IO, stderr)

newtype HooglePackagesSandbox = HooglePackagesSandbox [PackageId]
newtype HooglePackagesCached = HooglePackagesCached [PackageId]

data Hoogle =
  Hoogle {
      hooglePath :: File
    , _hoogleVersion :: HoogleVersion
    }

data HoogleVersion = Hoogle5x

buildRoot :: Directory
buildRoot =
  "dist-newstyle"

buildRootHoogleDirectory :: Directory
buildRootHoogleDirectory = buildRoot </> "hoogle"

hoogle :: Text -> [Argument] -> EitherT CabalHoogleError IO ()
hoogle hackageRoot args = do
  hp <- hooglePackages hackageRoot
  hpc <- hooglePackagesCached
  hspc <- hoogleSourcePackages hackageRoot
  hoogleIndex args (joinHooglePackages hpc hp) hspc

hoogleSourcePackages :: Text -> EitherT CabalHoogleError IO [Text]
hoogleSourcePackages _hackageRoot = do
  -- TODO Only run this when nothing exists, always build local hoogle for haddock

  Out str <- liftCabal (cabalFind (buildRoot </> "build") ["-name", "*.txt"])
  let pkgs = T.lines str

  -- Heuristic, if no local .txt hoogle files exist, maybe re-run v2-haddock to generate them and
  -- retry again.
  if null pkgs then do
    liftIO . T.hPutStrLn stderr $ "No hoogle files found re-running haddock to generate them."
    liftCabal (cabal_ "v2-haddock" ["--haddock-hoogle", "all", "--verbose=0"])

    Out str' <- liftCabal (cabalFind (buildRoot </> "build") ["-name", "*.txt"])
    pure (T.lines str')
  else
    pure pkgs

-- | Download all packages installed in the local sandbox into a global location
hooglePackages :: Text -> EitherT CabalHoogleError IO HooglePackagesSandbox
hooglePackages hackageRoot = do
  -- firstT MafiaInitError $ initialize LatestSources Nothing Nothing
  db <- hoogleCacheDir
  hoogleExe <- findHoogle
  Out planPkgStr <- liftCabal $ cabalPlan "topo" ["--ascii", "--"]
  let planPkgs = T.splitOn "\n" . T.strip $ planPkgStr
  fmap (HooglePackagesSandbox . catMaybes) . for (L.tail planPkgs) $ \pkg -> do
    pkgId <- hoistEither . maybeToRight (CabalHoogleParseError $ mconcat ["Invalid package: ", pkg]) . parsePackageId $ pkg
    let name = unPackageName . pkgName $ pkgId
    let txt = db </> pkg <> ".txt"
    let hoo = hoogleDbFile' hoogleExe db pkgId
    let skip = db </> pkg <> ".skip"
    ifM (doesFileExist skip) (pure Nothing) $
      ifM (doesFileExist hoo) (pure $ Just pkgId) $ do
        liftIO . T.hPutStrLn stderr $ "Downloading: " <> pkg
        r <- runEitherT $ call CabalHoogleProcessError "curl" ["-f", "-s", hackageRoot </> pkg </> "docs" </> name <> ".txt", "-o", txt]
        case r of
          Left _ -> do
            liftIO . T.hPutStrLn stderr $ "Missing: " <> pkg
            -- Technically we can "convert" a broken txt file and no one is the wiser, but we're not going to do that
            liftIO $ T.writeFile (T.unpack skip) ""
            pure Nothing
          Right Hush -> do
            pure $ Just pkgId

hoogleIndex :: [Argument] -> [PackageId] -> [Text] -> EitherT CabalHoogleError IO ()
hoogleIndex args pkgs srcPkgs = do
  -- By default hoogle will expect a 'default.hoo' file to exist in the database directory
  -- If we want the search to just be for _this_ sandbox, we have two options
  -- 1. Create a unique directory based on all the current packages and ensure the default.hoo
  -- 2. Specify/append all the packages from the global database by using "+$name-$version"
  --    Unfortunately hoogle doesn't like the "-$version" part :(
  db <- hoogleCacheDir
  hoogleExe <- findHoogle
  let db' = buildRootHoogleDirectory
  case hoogleExe of
    Hoogle hoogleExe' Hoogle5x -> do
      unlessM (doesFileExist $ db' </> "default.hoo") $ do
        createDirectoryIfMissing True db'

        -- Link each hoogle file into `db'` directory
        forM_ pkgs $ \pkg -> do
          let src = db </> renderPackageId pkg <> ".txt"
          let dst = db' </> takeFileName src
          unlessM (doesFileExist dst) $ do
            createSymbolicLink src dst

        -- Link each source package hoogle file into `db'` directory
        forM_ srcPkgs $ \pkg -> do
          let src = pkg
          src' <- canonicalizePath pkg
          let dst = db' </> takeFileName src
          unlessM (doesFileExist dst) $ do
            createSymbolicLink src' dst

        let a = mconcat [
              ["generate", "--database", db' </> "default.hoo"
              , "--local=" <> db']
              ]
        call_ CabalHoogleProcessError hoogleExe' a
      call_ CabalHoogleProcessError (hooglePath hoogleExe) $ ["-d", db' </> "default.hoo"] <> args

hooglePackagesCached :: (Functor m, MonadIO m) => m HooglePackagesCached
hooglePackagesCached = do
  db <- hoogleCacheDir
  HooglePackagesCached . mapMaybe ((=<<) parsePackageId . T.stripSuffix ".hoo" . takeFileName) <$>
    getDirectoryListing (RecursiveDepth 1) db

-- | Keep everything from the current sandbox and append the latest of any remaining packages
joinHooglePackages :: HooglePackagesCached -> HooglePackagesSandbox -> [PackageId]
joinHooglePackages (HooglePackagesCached cached) (HooglePackagesSandbox current) =
  let index = mapFromListGrouped . fmap packageIdTuple
      extra = fmap (uncurry PackageId) . M.toList . M.mapMaybe (head . reverse . L.sort) $ M.difference (index cached) (index current)
  in current <> extra

hoogleCacheDir :: MonadIO m => m Directory
hoogleCacheDir =
  ensureCabalDir "hoogle"

-- | Find the 'hoogle' executable on $PATH, error if it isn't there.
--
-- TODO print out the instructions to cabal install hoogle
findHoogle :: EitherT CabalHoogleError IO Hoogle
findHoogle = do
  h <- findHoogleExe
  v <- detectHoogleVersion h
  pure $ Hoogle h v

findHoogleExe :: EitherT CabalHoogleError IO File
findHoogleExe = do
  res <- runEitherT $ T.init . unOut <$> call CabalHoogleProcessError "which" ["hoogle"]
  case res of
    Right path -> pure path
    Left x ->
      -- TODO More friendly error messages about expecting to find `hoogle` on $PATH
      left . CabalHoogleParseError $ ("Invalid hoogle version: " <> renderCabalHoogleError x)

detectHoogleVersion :: File -> EitherT CabalHoogleError IO HoogleVersion
detectHoogleVersion hf = do
  res <- T.init . unOut <$> call CabalHoogleProcessError hf ["--version"]
  if T.isPrefixOf "Hoogle 5." res then
    pure Hoogle5x
  else
    left . CabalHoogleParseError $ "Invalid hoogle version: " <> res

hoogleDbFile' :: Hoogle -> Directory -> PackageId -> File
hoogleDbFile' v db pkg = case v of
  Hoogle _ Hoogle5x ->
    db </> renderPackageId pkg <> ".txt"

mapFromListGrouped :: Ord a => [(a, b)] -> Map a [b]
mapFromListGrouped =
  foldr (\(k, v) -> M.insertWith (<>) k [v]) M.empty
