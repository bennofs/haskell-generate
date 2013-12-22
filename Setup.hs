{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.IORef
import Data.List ( nub )
import Data.Version ( showVersion )
import Distribution.Package ( PackageName(PackageName), PackageId, InstalledPackageId, packageVersion, packageName )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..), hsSourceDirs, libBuildInfo, buildInfo)
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, withExeLBI, ComponentLocalBuildInfo(), LocalBuildInfo(), componentPackageDeps )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag, buildDistPref, defaultDistPref, fromFlagOrDefault )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Verbosity ( Verbosity )
import System.Directory ( canonicalizePath )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi flags
     buildHook simpleUserHooks pkg lbi hooks flags
  }

--  Very ad-hoc implementation of difference lists
singletonDL :: a -> [a] -> [a]
singletonDL = (:)

emptyDL :: [a] -> [a]
emptyDL = id

appendDL :: ([a] -> [a]) -> ([a] -> [a]) -> [a] -> [a]
appendDL x y = x . y

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> BuildFlags -> IO ()
generateBuildModule verbosity pkg lbi flags = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withTestLBI pkg lbi $ \suite suitelbi -> do
    srcDirs <- mapM canonicalizePath $ hsSourceDirs $ testBuildInfo suite
    distDir <- canonicalizePath $ fromFlagOrDefault defaultDistPref $ buildDistPref flags

    depsVar <- newIORef emptyDL
    withLibLBI pkg lbi $ \lib liblbi ->
      modifyIORef depsVar $ appendDL . singletonDL $ depsEntry (libBuildInfo lib) liblbi suitelbi
    withExeLBI pkg lbi $ \exe exelbi ->
      modifyIORef depsVar $ appendDL . singletonDL $ depsEntry (buildInfo exe) exelbi suitelbi
    deps <- fmap ($ []) $ readIORef depsVar

    rewriteFile (map fixchar $ dir </> "Build_" ++ testName suite ++ ".hs") $ unlines 
      [ "module Build_" ++ map fixchar (testName suite) ++ " where"
      , "getDistDir :: FilePath"
      , "getDistDir = " ++ show distDir
      , "getSrcDirs :: [FilePath]"
      , "getSrcDirs = " ++ show srcDirs
      , "deps :: [([FilePath], [String])]"
      , "deps = " ++ show deps
      ]

  where
    formatdeps = map (formatone . snd)
    formatone p = case packageName p of
      PackageName n -> n ++ "-" ++ showVersion (packageVersion p)
    depsEntry targetbi targetlbi suitelbi = (hsSourceDirs targetbi, formatdeps $ testDeps targetlbi suitelbi)
    fixchar '-' = '_'
    fixchar c = c

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys
