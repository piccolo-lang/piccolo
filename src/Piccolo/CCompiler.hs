{-|
Module          : Backend.CCompiler
Description     : Description and options of the C compiler
Stability       : experimental

The piccolo compiler first produces C code. This modules gives access and
options to the available C compiler to finally produce an executable.
-}
module Piccolo.CCompiler
  ( compileCCode
  , compileCCodeWithProfiling
  )
where

import Paths_piccolo

import System.Process
import System.IO

-- | C code compilation with gcc in IO
compileCCode' :: Bool -> String -> FilePath -> IO ()
compileCCode' profilMode ccode fname = do
  libDir <- getLibDir
  let ccStd  = ["-std=c11"]
      ccInp  = ["-xc", "-"]
      ccOut  = ["-o", fname]
      ccInc  = ["-I", libDir]
      ccLib  = ["-L", libDir]
      ccLnk  = if profilMode
                  then ["-lpiccolortprof", "-lpthread"]
                  else ["-lpiccolort", "-lpthread"]
      ccArgs = ccStd ++ ccInp ++ ccOut ++ ccInc ++ ccLib ++ ccLnk
  (Just ccStdin, _, _, ccProc) <- createProcess (proc "gcc" ccArgs)
                                  { std_in  = CreatePipe
                                  , std_out = UseHandle stdout
                                  , std_err = UseHandle stderr
                                  }
  hPutStr ccStdin ccode
  hClose ccStdin
  _ <- waitForProcess ccProc
  return ()

compileCCode :: String -> FilePath -> IO ()
compileCCode = compileCCode' False

compileCCodeWithProfiling :: String -> FilePath -> IO ()
compileCCodeWithProfiling = compileCCode' True
