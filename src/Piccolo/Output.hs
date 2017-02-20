{-|
Module         : Piccolo.Output
Description    : Utilities to display Piccolo internals
Stability      : experimental

Some options of the Piccolo compiler may involve internals printing.
This module helps printing those data.
-}
module Piccolo.Output
  (printEnvSizes
  )
where

import Piccolo.AST

import Control.Monad

printEnvSizes :: Modul -> IO ()
printEnvSizes modul = do
  putStrLn $ "Env sizes for module " ++ show (modName modul) ++ ":"
  forM_ (modDefs modul) $ \def -> do
    putStr $ "  " ++ defName def
    putStr $ replicate (20 - length (defName def)) ' ' ++ " { "
    putStr $ "lexical_env_size: " ++ show (defLexicalEnvSize def) ++ ", "
    putStr $ "choice_max_size: " ++ show (defChoiceMaxSize def)
    putStrLn " }"
