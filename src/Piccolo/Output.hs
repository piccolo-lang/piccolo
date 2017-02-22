{-|
Module         : Piccolo.Output
Description    : Utilities to display Piccolo internals
Stability      : experimental

Some options of the Piccolo compiler may involve internals printing.
This module helps printing those data.
-}

module Piccolo.Output
  ( dumpParsedModule
  , dumpTypedModule
  , dumpSigModule
  , dumpCCode
  )
where

import Piccolo.AST

import Control.Monad

dumpParsedModule :: Modul -> IO ()
dumpParsedModule m = do
  putStrLn $ replicate 20 '=' ++ " parsed module " ++ replicate 20 '='
  putStrLn ""
  print m

dumpTypedModule :: Modul -> IO ()
dumpTypedModule m = do
  putStrLn $ replicate 20 '=' ++ " typed module " ++ replicate 20 '='
  putStrLn ""
  print m

dumpSigModule :: Modul -> IO ()
dumpSigModule modul = do
  putStrLn $ replicate 20 '=' ++ " module signature " ++ replicate 20 '='
  putStrLn ""
  putStrLn $ "Env sizes for module " ++ show (modName modul) ++ ":"
  forM_ (modDefs modul) $ \def -> do
    putStr $ "  " ++ defName def
    putStr $ replicate (20 - length (defName def)) ' ' ++ " { "
    putStr $ "lexical_env_size: " ++ show (defLexicalEnvSize def) ++ ", "
    putStr $ "choice_max_size: " ++ show (defChoiceMaxSize def)
    putStrLn " }"

dumpCCode :: String -> IO ()
dumpCCode code = do
  putStrLn $ replicate 20 '=' ++ " generated C code " ++ replicate 20 '='
  putStrLn ""
  putStrLn code
