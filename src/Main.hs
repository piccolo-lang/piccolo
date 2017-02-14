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

import Piccolo.Errors
import Piccolo.Parser
import Piccolo.AST
import Piccolo.Output
import Piccolo.Typecheck
import Piccolo.Environments
import Piccolo.Compilation
import Piccolo.Codegen
import qualified Piccolo.CBackend as CBackend
import Piccolo.CCompiler

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
  unless (null errors) $ void (showHelp opts)
  when (length nonOpts /= 1) $ void (showHelp opts)
  let [piFile] = nonOpts
  content             <- readFile piFile
  ast                 <- reportResult $ parseModule content
  typedAst            <- reportResult $ typeCheck ast
  withEnv             <- reportResult $ computingEnvPass typedAst
  when (optShowEnvSizes opts) $ printEnvSizes withEnv
  seqAst              <- reportResult $ compilePass withEnv
  let code  = runEmitterM $ CBackend.emitCode (mainDef withEnv)
                                              (mainEnvSize withEnv) seqAst
  when (isJust (optDumpC opts)) $ writeFile (fromJust (optDumpC opts)) code
  compileCCode code $ optOutput opts
  where
    mainDef m     = delete '/' (modName m) ++ "_Main"
    mainEnvSize m = case find (\d -> defName d == "Main") (modDefs m) of
                      Just d  -> defLexicalEnvSize d
                      Nothing -> error "no Main def"

data Options = Options
  { optOutput       :: String
  , optDumpC        :: Maybe String
  , optShowEnvSizes :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optOutput       = "a.out"
  , optDumpC        = Nothing
  , optShowEnvSizes = False
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "v" ["version"]   (NoArg showVersion)         "show version number"
  , Option "h" ["help"]      (NoArg showHelp)            "show help"
  , Option "o" ["out"]       (ReqArg writeOutput "FILE") "output file"
  , Option ""  ["dump-c"]    (ReqArg writeDumpC "FILE")  "dump c code"
  , Option ""  ["show-env-sizes"] (NoArg showEnvSizes)   "show environments sizes"
  ]

showVersion :: Options -> IO Options
showVersion _ = do
  putStrLn "The Piccolo Compiler 0.1"
  exitSuccess

showHelp :: Options -> IO Options
showHelp _ = do
  putStr $ usageInfo "Usage: piccolo [options] <piccolo_file>" options
  exitSuccess

writeOutput :: String -> Options -> IO Options
writeOutput arg opt = return $ opt { optOutput = arg }

writeDumpC :: String -> Options -> IO Options
writeDumpC arg opt = return $ opt { optDumpC = Just arg }

showEnvSizes :: Options -> IO Options
showEnvSizes opt = return $ opt { optShowEnvSizes = True }
