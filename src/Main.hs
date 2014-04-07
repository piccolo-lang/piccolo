module Main where

import System.Environment
import System.Exit

import PilParser
import PrettyPrinter

main :: IO ()
main = getArgs >>= parseArgs >>= compile

compile :: [String] -> IO ()
compile (m:ms) = compileModule m >> compile ms
  where compileModule pim = case parseModule pim of
                            Left  e -> putStrLn ("error: " ++ e)
                            Right p -> putStrLn $ prettyPrint p
compile [] = return ()

parseArgs :: [String] -> IO [String]
parseArgs ["-h"] = usage   >> exit
parseArgs ["-v"] = version >> exit
parseArgs []     = sequence [getContents]
parseArgs fs     = mapM readFile fs

usage :: IO ()
usage = putStrLn "Usage: picc [-vh] [file ...]"

version :: IO ()
version = putStrLn "Piccolo 0.1"

exit :: IO a
exit = exitWith ExitSuccess

die :: IO a
die = exitWith (ExitFailure 1)
