{-|
Module         : Core.DebugSymbols
Description    : Debug symbol generation module
Stability      : experimental

This module declares types for debug events,
and also functions to output them to a special file.
-}
module Core.DebugSymbols
  ( DebugEvent
  , emitDebugSymbols
  )
where

import Core.AST

import Data.List (intercalate)
import System.IO

-- An AST node with a unique ID
type DebugEvent = Process

emitDebugSymbols :: [DebugEvent] -> IO ()
emitDebugSymbols events = do
  handle <- openFile "debugsymbols" WriteMode
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
