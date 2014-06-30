{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module         : Main
Description    : Entry point of the piccolo compiler
Stability      : experimental

The piccolo can be invoked with the following options:

  * --help or -h                   prints the help

  * --generic or -g                produces generic code

  * --sast<level> or -s<level>     print AST<level> in s-expressions style for debugging purposes

It waits for one or more pi-file names and produces by default C code in a.out.
-}
module Main where

import PiccError
import Front.PilParser
import Front.AST
import Front.ASTUtils
import Middle.Typing
import Middle.Environments
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
import Control.Monad
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
  content  <- readFile f
  ast      <- reportResult $ parseModule content
  when ((SAST SimplePrint)  `elem` args) $ do
    putStrLn "------------------------------ output of the parsed AST"
    putStrLn (strSExpr [] ast)
    putStrLn ""
  typedAst <- reportResult $ typingPass ast
  when ((SAST PrintTypes)   `elem` args) $ do
    putStrLn "------------------------------ output of the typed AST"
    putStrLn (strSExpr [PrintTypes] typedAst)
    putStrLn ""
  withEnv  <- reportResult $ computingEnvPass typedAst
  when ((SAST PrintIndexes) `elem` args) $ do
    putStrLn "------------------------------ output of the decorated AST"
    putStrLn (strSExpr [PrintIndexes] withEnv)
    putStrLn ""
  emittedCode    <- reportResult $ if Generic `elem` args
    then compileToGeneric withEnv
    else compileToC       withEnv
  if Generic `elem` args
    then outputCode emittedCode f "a.out"
    else outputCode emittedCode f "a.c"
  handleFiles args fs

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

outputCode :: String -> String -> String -> IO ()
outputCode code f fname = do
  hOut <- openFile fname WriteMode
  hPutStr hOut code
  hClose hOut
  putStrLn $ "successfully compiled " ++ f ++ " into " ++ fname

-- | Various flags for compiler options
data Flag
  = Help              -- ^ \--help or -h
  | Generic           -- ^ \--generic or -g
  | SAST PrintLevel   -- ^ \--sast or -s
  deriving Eq

-- | Flags description to parse with "System.Console.GetOpt" module
flags :: [OptDescr Flag]
flags =
  [ Option "h" ["help"] (NoArg Help)
      "Print this help message"
  , Option "g" ["generic"] (NoArg Generic)
      "Produce generic code instead of C code"
  , Option "s0" ["sast0"] (NoArg (SAST SimplePrint))
      "Print AST in s-epxressions style for debugging purposes"
  , Option "s1" ["sast1"] (NoArg (SAST PrintTypes))
      "Print AST in s-epxressions style for debugging purposes"
  , Option "s2" ["sast2"] (NoArg (SAST PrintIndexes))
      "Print AST in s-epxressions style for debugging purposes"
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
