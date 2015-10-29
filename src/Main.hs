{-|
Module         : Main
Description    : Entry point of the piccolo compiler
Stability      : experimental

The piccolo can be invoked with the following options:

  * --version or -v                print version

  * --help or -h                   print the help

  * --out or -o                    specify the output executable filename

It waits for a pi-file name and produces by default binary file a.out.
-}
module Main
  (main
  )
where

import Errors
import Core.Parser
import Core.AST
import Core.Typecheck
import Core.Environments
import Core.Compilation
import Core.DebugSymbols
import Backend.Codegen
import qualified Backend.CBackend as CBackend
import Backend.CCompiler

import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.List
import Data.Maybe
import Control.Monad


-- | Main function
main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOpts, errors) = getOpt Permute options args
  forM_ errors putStrLn
  opts <- foldl (>>=) (return defaultOptions) actions
  when (not (null errors)) $ void (showHelp opts)
  let Options { optDebug  = debug
              , optOutput = output
              , optDumpC  = dumpC
              } = opts
  when (length nonOpts /= 1) $ void (showHelp opts)
  let [piFile] = nonOpts
  content             <- readFile piFile
  ast                 <- reportResult $ parseModule content
  typedAst            <- reportResult $ typeCheck ast
  withEnv             <- reportResult $ computingEnvPass typedAst
  (seqAst, dbgEvents) <- reportResult $ compilePass withEnv
  let code  = runEmitterM $ CBackend.emitCode (mainDef withEnv) seqAst
  when (isJust dumpC) $ writeFile (fromJust dumpC) code
  compileCCode code output debug
  when debug $ appendDebugSymbols dbgEvents output
  where
    mainDef m = delete '/' (modName m) ++ "_Main"

data Options = Options
  { optDebug  :: Bool
  , optOutput :: String
  , optDumpC  :: Maybe String
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optDebug  = False
  , optOutput = "./a.out"
  , optDumpC  = Nothing
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "v" ["version"] (NoArg showVersion)         "show version number"
  , Option "h" ["help"]    (NoArg showHelp)            "show help"
  , Option "g" ["debug"]   (NoArg writeDebug)          "debug mode"
  , Option "o" ["out"]     (ReqArg writeOutput "FILE") "output file"
  , Option ""  ["dump-c"]  (ReqArg writeDumpC "FILE")  "dump c code"
  ]

showVersion :: Options -> IO Options
showVersion _ = do
  putStrLn "The Piccolo Compiler 0.1"
  exitSuccess

showHelp :: Options -> IO Options
showHelp _ = do
  putStr $ usageInfo "Usage: piccolo [options] <piccolo_file>" options
  exitSuccess

writeDebug :: Options -> IO Options
writeDebug opt = return $ opt { optDebug = True }

writeOutput :: String -> Options -> IO Options
writeOutput arg opt = return $ opt { optOutput = arg }

writeDumpC :: String -> Options -> IO Options
writeDumpC arg opt = return $ opt { optDumpC = Just arg }
