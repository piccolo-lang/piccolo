{-|
Module          : Backend.CCompiler
Description     : Description and options of the C compiler
Stability       : experimental

The piccolo compiler first produces C code. This modules gives access and
options to the available C compiler to finally produce an executable.
-}
module Backend.CCompiler
  (compileCCode
  )
where

import Paths_piccolo

import System.Process
import System.IO

-- | C code compilation with gcc in IO
compileCCode :: String -> FilePath -> Bool -> IO ()
compileCCode ccode fname debug = do
  dataDir <- getDataDir
  let ccInc  = ["-I", dataDir ++ "/runtime"]
      ccLib  = ["-L", dataDir ++ "/runtime"]
      ccOut  = ["-o", fname]
      ccDbg  = if debug 
                 then ["-DPICC_DEBUG_MODE", "-lpiccolortdbg"]
                 else ["-lpiccolort"]
      ccArgs = ["-std=c11", "-xc", "-"] ++ ccOut ++ ccInc ++ ccLib ++ ccDbg ++
               ["-lpthread"]
  (Just ccStdin, _, _, ccProc) <- createProcess (proc "gcc" ccArgs)
                                  { std_in  = CreatePipe
                                  , std_out = UseHandle stdout
                                  , std_err = UseHandle stderr
                                  }
  hPutStr ccStdin ccode
  hClose ccStdin
  _ <- waitForProcess ccProc
  return ()

