module Main where

import Front.PilParser
import Front.AST
import Middle.Typing
import Middle.IndexesComputations
import Middle.Compilation
import Back.SeqAST
import Back.RTOptions
import Back.Backend
import Back.GenericBackend
import Back.CBackend
import Utils.CodeEmitter

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit
import Control.Monad.Reader
import Data.List


main :: IO ()
main = do
  (args, files) <- getArgs >>= parseArgs
  mapM_ (handleFile args) files

handleFile :: [Flag] -> String -> IO ()
handleFile args file = do
  content <- readFile file
  case parseModule content of
    Left  err -> do { putStrLn $ "error: " ++ err ; exitWith $ ExitFailure 1 }
    Right moduleDef -> do
      let typedModuleDef     = typingPass moduleDef
      let completedModuleDef = computingIndexesPass typedModuleDef
      if Generic `elem` args
        then compileToGeneric completedModuleDef
        else compileToC       completedModuleDef

compileToGeneric :: ModuleDef -> IO ()
compileToGeneric piAst = do
  let seqAst = compilePass piAst :: Instr GenericBackend
  rtOptions  <- defaultsRTOptions
  hOut       <- openFile "out.gen" WriteMode
  hPutStr hOut $ runEmitterM (emitCode rtOptions "Main" seqAst)
  hClose hOut

compileToC :: ModuleDef -> IO ()
compileToC piAst = do
  let seqAst = compilePass piAst :: Instr CBackend
  rtOptions  <- defaultsRTOptions
  hOut       <- openFile "out.c" WriteMode
  hPutStr hOut $ runEmitterM (emitCode rtOptions "Main" seqAst)
  hClose hOut

data Flag
  = Help       -- -h
  | Generic    -- -g
  deriving Eq

flags :: [OptDescr Flag]
flags =
  [ Option ['h'] ["help"] (NoArg Help)
      "Print this help message"
  , Option ['g'] ["generic"] (NoArg Generic)
      "Produce generic code instead of C code"
  ]

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

