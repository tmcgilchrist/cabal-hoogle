{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

import           Data.Char (isDigit)
import           Distribution.InstalledPackageInfo
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.BuildPaths (autogenPackageModulesDir)
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PackageIndex
import           Distribution.Simple.Setup (BuildFlags (..), ReplFlags (..), TestFlags (..), fromFlag)
import           Distribution.Simple.Utils
import           Distribution.Verbosity

showVersion :: Version -> String
showVersion = intercalate "." . fmap show . versionNumbers

autogenModulesDirCompat :: LocalBuildInfo -> String
autogenModulesDirCompat = autogenPackageModulesDir

main :: IO ()
main =
  let hooks = simpleUserHooks
   in defaultMainWithHooks hooks {
     preConf = \args flags -> do
       createDirectoryIfMissingVerbose silent True "gen"
       (preConf hooks) args flags
   , buildHook = \pd lbi uh flags -> do
       genBuildInfo (fromFlag $ buildVerbosity flags) pd
       genDependencyInfo (fromFlag $ buildVerbosity flags) pd lbi
       (buildHook hooks) pd lbi uh flags
   , replHook = \pd lbi uh flags args -> do
       genBuildInfo (fromFlag $ replVerbosity flags) pd
       genDependencyInfo (fromFlag $ replVerbosity flags) pd lbi
       (replHook hooks) pd lbi uh flags args
   , testHook = \args pd lbi uh flags -> do
       genBuildInfo (fromFlag $ testVerbosity flags) pd
       genDependencyInfo (fromFlag $ testVerbosity flags) pd lbi
       (testHook hooks) args pd lbi uh flags
   }

genBuildInfo :: Verbosity -> PackageDescription -> IO ()
genBuildInfo verbosity pkg = do
  createDirectoryIfMissingVerbose verbosity True "gen"
  let pname = unPackageName . pkgName . package $ pkg
      version = pkgVersion . package $ pkg
      name = "BuildInfo_" ++ (map (\c -> if c == '-' then '_' else c) pname)
      targetHs = "gen/" ++ name ++ ".hs"
      targetText = "gen/version.txt"
  t <- timestamp verbosity
  
  let v = showVersion version
  let buildVersion = intercalate "-" [v, t]
  rewriteFileEx verbosity targetHs $ unlines [
      "module " ++ name ++ " where"
    , "import Prelude"
    , "data RuntimeBuildInfo = RuntimeBuildInfo { buildVersion :: String, timestamp :: String}"
    , "buildInfo :: RuntimeBuildInfo"
    , "buildInfo = RuntimeBuildInfo \"" ++ v ++ "\" \"" ++ t ++ "\""
    , "buildInfoVersion :: String"
    , "buildInfoVersion = \"" ++ buildVersion ++ "\""
    , "cabalVersion :: String"
    , "cabalVersion = \"" ++ VERSION_Cabal ++ "\""
    ]
  rewriteFileEx verbosity targetText buildVersion

genDependencyInfo :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
genDependencyInfo verbosity pkg info = do
  let
    pname = unPackageName . pkgName . package $ pkg
    name = "DependencyInfo_" ++ (map (\c -> if c == '-' then '_' else c) pname)
    targetHs = autogenModulesDirCompat info ++ "/" ++ name ++ ".hs"
    render p =
      let
        n = unPackageName $ pkgName p
        v = showVersion $ pkgVersion p
      in
       n ++ "-" ++ v
    deps = fmap (render . sourcePackageId) . allPackages $ installedPkgs info
    strs = flip fmap deps $ \d -> "\"" ++ d ++ "\""

  createDirectoryIfMissingVerbose verbosity True (autogenModulesDirCompat info)

  rewriteFileEx verbosity targetHs $ unlines [
      "module " ++ name ++ " where"
    , "import Prelude"
    , "dependencyInfo :: [String]"
    , "dependencyInfo = [\n    " ++ intercalate "\n  , " strs ++ "\n  ]"
    ]

timestamp :: Verbosity -> IO String
timestamp verbosity =
  rawSystemStdout verbosity "date" ["+%Y%m%d%H%M%S"] >>= \s ->
    case splitAt 14 s of
      (d, _n : []) ->
        if (length d == 14 && filter isDigit d == d)
          then return d
          else fail $ "date has failed to produce the correct format [" <> s <> "]."
      _ ->
        fail $ "date has failed to produce a date long enough [" <> s <> "]."
