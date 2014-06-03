{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module         : Main
Description    : Entry point of the piccolo compiler
Stability      : experimental

The piccolo can be invoked with the following options:

  * --help or -h    prints the help

  * --generic or -g produces generic code

It waits for one or more pi-file names and produces by default C code in a.out.
-}
module Main where

import PiccError
import Front.PilParser
import Front.AST
import Middle.Typing
import Middle.IndexesComputations
import Middle.Compilation
import Back.SeqAST
import Back.Backend hiding (null)
import Back.GenericBackend
import Back.CBackend
import Back.CodeEmitter

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit
import Control.Monad.Error
import Data.List

-- | Main function
main :: IO ()
main = do
  (args, files) <- getArgs >>= parseArgs
  handleFiles args files

-- | The 'handleFiles' function takes a list of flags and a list of files and compiles them
handleFiles :: [Flag] -> [String] -> IO ()
handleFiles args [] = return ()
handleFiles args (f:fs) = do
  content <- readFile f
  result  <- reportResult $ handleData args content
  hOut    <- openFile "a.out" WriteMode
  hPutStr hOut result
  hClose hOut
  putStrLn $ "successfully compiled " ++ f ++ " file into a.out"
  handleFiles args fs

-- | The 'handleData' function takes a list of flags and a string representing a
-- piccolo program and compile it
handleData :: [Flag] -> String -> Either PiccError String
handleData args input = do
  transfAst <- parseModule input >>= typingPass >>= computingIndexesPass
  if Generic `elem` args
    then compileToGeneric transfAst
    else compileToC       transfAst

-- | The 'compileToGeneric' function compiles a piccolo AST using the generic backend
compileToGeneric :: ModuleDef -> Either PiccError String
compileToGeneric piAst = do
  seqAst :: Instr GenericBackend <- compilePass piAst
  let output = runEmitterM (emitCode "Main" seqAst)
  return output

-- | The 'compileToC' function compiles a piccolo AST using the C backend
compileToC :: ModuleDef -> Either PiccError String
compileToC piAst = do
  seqAst :: Instr CBackend <- compilePass piAst
  let output = runEmitterM (emitCode "Main" seqAst)
  return output

-- | Various flags for compiler options
data Flag
  = Help       -- ^ \--help or -h
  | Generic    -- ^ \--generic or -g
  deriving Eq

-- | Flags description to parse with "System.Console.GetOpt" module
flags :: [OptDescr Flag]
flags =
  [ Option ['h'] ["help"] (NoArg Help)
      "Print this help message"
  , Option ['g'] ["generic"] (NoArg Generic)
      "Produce generic code instead of C code"
  ]

-- | The function 'parseArgs' use the "System.Console.GetOpt" module to parse executable options
parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argv = case getOpt Permute flags argv of
  (args,fs,[]) -> do
    let files = if null fs then ["-"] else fs
    if Help `elem` args
      then do putStrLn $ usageInfo header flags
              exitWith ExitSuccess
      else return (nub args, files)
  (_,_,errs) -> do
    putStrLn (concat errs ++ usageInfo header flags)
    exitWith $ ExitFailure 1
  where header = "Usage: piccolo [-h] [file ...]"

