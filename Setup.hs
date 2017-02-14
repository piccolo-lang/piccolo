import Control.Monad

import Distribution.Simple
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Distribution.Simple.InstallDirs as I
import Distribution.Simple.LocalBuildInfo as L
import qualified Distribution.Simple.Setup as S
import qualified Distribution.Simple.Program as P
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile)
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Verbosity

import System.FilePath ((</>), isAbsolute)
import System.Directory
import qualified System.FilePath.Posix as Px


make :: Verbosity -> [String] -> IO ()
make verbosity =
  P.runProgramInvocation verbosity . P.simpleProgramInvocation "make"

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks
  { postClean = piccoloClean
  , postBuild = piccoloBuild
  , postCopy  = \_ flags pkg local ->
                  piccoloInstall (S.fromFlag $ S.copyVerbosity flags)
                                 (S.fromFlag $ S.copyDest flags) pkg local
  , postInst  = \_ flags pkg local ->
                  piccoloInstall (S.fromFlag $ S.installVerbosity flags)
                                 NoCopyDest pkg local
  }


piccoloClean :: Args
             -> S.CleanFlags
             -> PackageDescription
             -> ()
             -> IO ()
piccoloClean _ flags _ _ = do
  let verbosity = S.fromFlag $ S.cleanVerbosity flags
  make verbosity [ "-C", "rts", "clean" ]


piccoloBuild :: Args
             -> S.BuildFlags
             -> PackageDescription
             -> LocalBuildInfo
             -> IO ()
piccoloBuild _ flags _ local  = do
  let verbosity = S.fromFlag $ S.buildVerbosity flags
  make verbosity [ "-C", "rts", "build" ]


piccoloInstall :: Verbosity
               -> CopyDest
               -> PackageDescription
               -> LocalBuildInfo
               -> IO ()
piccoloInstall verbosity copy pkg local = do
  let target = libdir $ L.absoluteInstallDirs pkg local copy
  putStrLn $ "Installing runtime in " ++ target
  make verbosity [ "-C", "rts", "install", "TARGET=" ++ target ]

