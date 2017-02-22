{-|
Module         : Main
Description    : Entry point of the piccolo compiler
Stability      : experimental

Main module of the piccolo compiler.
-}

module Main
  (main
  )
where

import Piccolo.AST
import Piccolo.Codegen
import Piccolo.Compilation
import Piccolo.Environments
import Piccolo.Errors
import Piccolo.Output
import Piccolo.Parsers.ModuleParser
import Piccolo.Typecheck
import Piccolo.CCompiler
import qualified Piccolo.CBackend as CBackend

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
  content <- readFile piFile

  -- parsing
  ast <- reportResult $ parseModule content
  when (optDumpParsed opts) $ dumpParsedModule ast
  
  -- typing
  typedAst            <- reportResult $ typeCheck ast

  -- computing environment sizes
  withEnv             <- reportResult $ computingEnvPass typedAst
  when (optShowEnvSizes opts) $ printEnvSizes withEnv

  -- compiling
  seqAst              <- reportResult $ compilePass withEnv

  -- emitting C code
  let code  = runEmitterM $ CBackend.emitCode (mainDef withEnv)
                                              (mainLexEnvSize withEnv)
                                              (mainChcEnvSize withEnv)
                                              seqAst
  when (isJust (optDumpC opts)) $ writeFile (fromJust (optDumpC opts)) code
  compileCCode code $ optOutput opts
  where
    mainDef Modul { modName = m } =
      let (ModuleName mainModName) = m in
      intercalate "_" mainModName ++ "_Main"
    mainLexEnvSize m = case find (\d -> defName d == "Main") (modDefs m) of
                         Just d  -> defLexicalEnvSize d
                         Nothing -> error "no Main def"
    mainChcEnvSize m = case find (\d -> defName d == "Main") (modDefs m) of
                         Just d  -> defChoiceMaxSize d
                         Nothing -> error "no Main def"

data Options = Options
  { optOutput       :: String
  , optDumpParsed   :: Bool
  , optDumpC        :: Maybe String
  , optShowEnvSizes :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { optOutput       = "a.out"
  , optDumpParsed   = False
  , optDumpC        = Nothing
  , optShowEnvSizes = False
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "v" ["version"]     (NoArg showVersion)         "show version number"
  , Option "h" ["help"]        (NoArg showHelp)            "show help"
  , Option "o" ["out"]         (ReqArg writeOutput "FILE") "output file"
  , Option ""  ["dump-parsed"] (NoArg dumpParsed)          "dump parsed piccolo module"
  , Option ""  ["dump-c"]      (ReqArg writeDumpC "FILE")  "dump c code"
  , Option ""  ["show-env-sizes"] (NoArg showEnvSizes)     "show environments sizes"
  ]

showVersion :: Options -> IO Options
showVersion _ = do
  putStrLn "The Piccolo Compiler 0.1"
  exitSuccess

showHelp :: Options -> IO Options
showHelp _ = do
  putStr $ usageInfo "Usage: piccolo [options] <file.pi>" options
  exitSuccess

writeOutput :: String -> Options -> IO Options
writeOutput arg opt = return $ opt { optOutput = arg }

dumpParsed :: Options -> IO Options
dumpParsed opt = return $ opt { optDumpParsed = True }

writeDumpC :: String -> Options -> IO Options
writeDumpC arg opt = return $ opt { optDumpC = Just arg }

showEnvSizes :: Options -> IO Options
showEnvSizes opt = return $ opt { optShowEnvSizes = True }
