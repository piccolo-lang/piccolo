{-|
Module         : Main
Description    : Entry point of the piccolo compiler
Stability      : experimental

The piccolo can be invoked with the following options:

  * --help or -h                   prints the help

It waits for a pi-file name and produces by default binary file a.out.
-}
module Main where

import Errors
import Core.Parser
import Core.AST
import Core.Typecheck
import Core.Environments
import Core.Compilation
import Backend.Codegen
import qualified Backend.CBackend as CBackend

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Process
import Data.List
import Data.Maybe

import Paths_piccolo


-- | Main function
main :: IO ()
main = do
  (args, files) <- getArgs >>= parseArgs
  handleFiles args files

handleFiles :: [Flag] -> [String] -> IO ()
handleFiles _ [] = return ()
handleFiles _ [f] = do
  content  <- readFile f
  ast      <- reportResult $ parseModule content
  typedAst <- reportResult $ typingPass ast
  withEnv  <- reportResult $ computingEnvPass typedAst
  seqAst   <- reportResult $ compilePass withEnv
  let code   = runEmitterM $ CBackend.emitCode (mainDef withEnv) seqAst
  dataDir  <- getDataDir
  let ccInc  = ["-I", dataDir ++ "/runtime"]
      ccLib  = ["-L", dataDir ++ "/runtime"]
      ccArgs = ["-std=c11", "-xc", "-"] ++ ccInc ++ ccLib ++ ["-lpiccolort", "-lpthread"]
  (Just ccStdin, _, _, ccProc) <- createProcess (proc "gcc" ccArgs) { std_in  = CreatePipe
                                                                    , std_out = UseHandle stdout
                                                                    , std_err = UseHandle stderr
                                                                    }
  hPutStr ccStdin code
  hClose ccStdin
  _ <- waitForProcess ccProc
  return ()
  where mainDef m = delete '/' (modName m) ++ "_Main"
handleFiles _ _ = error "one file at once please..."

getOutputFileName :: [Flag] -> String -> String
getOutputFileName args defaultt =
  case mapMaybe f args of
    []  -> defaultt
    x:_ -> x
  where f (OutputFile s) = Just s
        f _              = Nothing

-- | Various flags for compiler options
data Flag
  = Help               -- ^ \--help or -h
  | OutputFile String  -- ^ \-o
  deriving Eq

-- | Flags description to parse with "System.Console.GetOpt" module
flags :: [OptDescr Flag]
flags =
  [ Option "h" ["help"] (NoArg Help)
      "Print this help message"
  , Option "o" ["out"] (ReqArg OutputFile "")
      "Specify the filename of the compiled file"
  ]

-- | The function 'parseArgs' use the "System.Console.GetOpt" module to parse executable options
parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argv = case getOpt Permute flags argv of
  (args,fs,[]) -> do
    let files = if null fs then ["-"] else fs
    if Help `elem` args
      then do putStrLn $ usageInfo header flags
              exitSuccess
      else return (nub args, files)
  (_,_,errs) -> do
    putStrLn (concat errs ++ usageInfo header flags)
    exitWith $ ExitFailure 1
  where header = "Usage: piccolo [-hgs] [file ...]"
