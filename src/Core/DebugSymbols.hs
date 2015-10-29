{-|
Module         : Core.DebugSymbols
Description    : Debug symbol generation module
Stability      : experimental

This module declares types for debug events,
and also functions to output them to a special file.
-}
module Core.DebugSymbols
  ( DebugEvent
  , appendDebugSymbols
  )
where

import Core.AST

import Data.List (intercalate)
import System.IO
import System.Process

-- An AST node with a unique ID
type DebugEvent = Process

genDebugSymbols :: [DebugEvent] -> IO ()
genDebugSymbols events = do
  handle <- openFile "/tmp/piccdebug" WriteMode
  emitDebugSymbols' events 0 handle
  hClose handle
  where
    emitDebugSymbols' [] _ _ = return ()
    emitDebugSymbols' (proc:procs) i h = do
      let startLine   = locStartLine   $ localize proc
      let startColumn = locStartColumn $ localize proc
      hPutStrLn h $ intercalate "," [ "{" ++ "\"id\":" ++ show i
                                    , "\"line\":" ++ show startLine
                                    , "\"column\":" ++ show startColumn
                                    , "\"code\":\"" ++ showProc proc ++ "\"}"
                                    ]
      emitDebugSymbols' procs (i+1) h
    showProc proc@PPrefix {} = show $ procPref proc
    showProc proc@PChoice {} = "<choice>"
    showProc proc            = show proc

appendDebugSymbols :: [DebugEvent] -> FilePath -> IO ()
appendDebugSymbols evts fname = do
  genDebugSymbols evts
  (_, _, _, ocProc) <- createProcess (proc "objcopy"
    ["--add-section", "piccdebug=/tmp/piccdebug", fname, fname])
    { std_in  = UseHandle stdout
    , std_out = UseHandle stdout
    , std_err = UseHandle stderr
    }
  _ <- waitForProcess ocProc
  return ()
