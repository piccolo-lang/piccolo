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
--import Front.PilParser
import Front.Parser
import Front.AST
import Front.ASTUtils
import Middle.Typing
import Middle.Environments
import Middle.Compilation
import Back.CodeEmitter
import qualified Back.CBackend as CBackend
--import qualified Back.GenericBackend as GenericBackend

import System.Environment
import System.Console.GetOpt
import System.IO
import System.Exit
--import System.FilePath.Posix
import System.Process
--import Control.Monad
import Data.List
import Data.Maybe

-- | Main function
main :: IO ()
main = do
  (args, files) <- getArgs >>= parseArgs
  handleFiles args files

-- | The 'handleFiles' function takes a list of flags and a list of files and compiles them
{-
handleFiles :: [Flag] -> [String] -> IO ()
handleFiles _ [] = return ()
handleFiles args [f] = do
  content     <- readFile f
  ast         <- reportResult $ parseModule content
  when (SAST SimplePrint  `elem` args) $ do
    putStrLn "------------------------------ output of the parsed AST"
    putStrLn (strSExpr [] ast)
    putStrLn ""
  typedAst    <- reportResult $ typingPass ast
  when (SAST PrintTypes   `elem` args) $ do
    putStrLn "------------------------------ output of the typed AST"
    putStrLn (strSExpr [PrintTypes] typedAst)
    putStrLn ""
  withEnv     <- reportResult $ computingEnvPass typedAst
  when (SAST PrintIndexes `elem` args) $ do
    putStrLn "------------------------------ output of the decorated AST"
    putStrLn (strSExpr [PrintIndexes] withEnv)
    putStrLn ""
  seqAst      <- reportResult $ compilePass withEnv
  if GenericBack `elem` args
    then do let code  = runEmitterM $ GenericBackend.emitCode (mainDef withEnv) seqAst
            let fname = getOutputFileName args (replaceExtension f "out")
            hOut <- openFile fname WriteMode
            hPutStr hOut code
            hClose hOut
    else do let code = runEmitterM $ CBackend.emitCode (mainDef withEnv) seqAst
            let fname = getOutputFileName args (replaceExtension f "c")
            hOut <- openFile fname WriteMode
            hPutStr hOut code
            hClose hOut
  where mainDef m = delete '/' (modName m) ++ "_Main"
handleFiles _ _ = error "one file at once please..."
-}

handleFiles :: [Flag] -> [String] -> IO ()
handleFiles _ [] = return ()
handleFiles _ [f] = do
  content  <- readFile f
  ast      <- reportResult $ parseModule content
  typedAst <- reportResult $ typingPass ast
  withEnv  <- reportResult $ computingEnvPass typedAst
  seqAst   <- reportResult $ compilePass withEnv
  let code = runEmitterM $ CBackend.emitCode (mainDef withEnv) seqAst
  --putStrLn code
  (Just gccStdin, _, _, gccProc) <- createProcess (proc "gcc" ["-std=c11", "-xc", "-", "-lpiccolort", "-lpthread"]) { std_in  = CreatePipe
                                                                            , std_out = UseHandle stdout
                                                                            , std_err = UseHandle stderr
                                                                            }
  hPutStr gccStdin code
  hClose gccStdin
  _ <- waitForProcess gccProc
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
  = Help              -- ^ \--help or -h
  | GenericBack       -- ^ \--generic or -g
  | CBack             -- ^ \--cback or -c
  | SAST PrintLevel   -- ^ \--sast or -s
  | OutputFile String
  deriving Eq

-- | Flags description to parse with "System.Console.GetOpt" module
flags :: [OptDescr Flag]
flags =
  [ Option "h" ["help"] (NoArg Help)
      "Print this help message"
  , Option "g" ["generic"] (NoArg GenericBack)
      "Produce generic code instead of C++ code"
  , Option "c" ["cback"] (NoArg CBack)
      "Produce c code instead of C++ code"
  , Option "" ["sast0"] (NoArg (SAST SimplePrint))
      "Print AST in s-epxressions style for debugging purposes"
  , Option "" ["sast1"] (NoArg (SAST PrintTypes))
      "Print AST in s-epxressions style for debugging purposes"
  , Option "" ["sast2"] (NoArg (SAST PrintIndexes))
      "Print AST in s-epxressions style for debugging purposes"
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
