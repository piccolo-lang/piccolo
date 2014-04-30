{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import PiccError
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
import Back.CodeEmitter

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit
import Control.Monad.Error
import Data.List


main :: IO ()
main = do
  (args, files) <- getArgs >>= parseArgs
  handleFiles args files

handleFiles :: [Flag] -> [String] -> IO ()
handleFiles args [] = return ()
handleFiles args (f:fs) = do
  content <- readFile f
  rtOpts  <- defaultsRTOptions
  result  <- reportResult $ handleData args content rtOpts 
  hOut    <- openFile "a.out" WriteMode
  hPutStr hOut result
  hClose hOut
  putStrLn $ "successfully compiled " ++ f ++ " file into a.out"
  handleFiles args fs

handleData :: [Flag] -> String -> RTOptions -> Either PiccError String
handleData args input rtOpts = do
  transfAst <- parseModule input >>= typingPass >>= computingIndexesPass
  if Generic `elem` args
    then compileToGeneric transfAst rtOpts
    else compileToC       transfAst rtOpts

compileToGeneric :: ModuleDef -> RTOptions -> Either PiccError String
compileToGeneric piAst rtOpts = do
  seqAst :: Instr GenericBackend <- compilePass piAst
  let output = runEmitterM (emitCode rtOpts "Main" seqAst)
  return output

compileToC :: ModuleDef -> RTOptions -> Either PiccError String
compileToC piAst rtOpts = do
  seqAst :: Instr CBackend <- compilePass piAst
  let output = runEmitterM (emitCode rtOpts "Main" seqAst)
  return output

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

