module Back.RTOptions where

{--
 - Runtime options
 --}

data RTOptions = RTOptions { nbThreads    :: Int
                           , stdGCFuel    :: Int
                           , quickGCFuel  :: Int
                           , activeFactor :: Int
                           }

{-- commented to avoid an superfluous dependency to lib System.Process
defaultsRTOptions :: IO RTOptions
defaultsRTOptions = do
  (_, Just hOut, _, _) <- createProcess (shell "nproc") { std_out = CreatePipe }
  nbThr <- liftM read $ hGetLine hOut
  return $ RTOptions { nbThreads    = nbThr
                     , stdGCFuel    = 42
                     , quickGCFuel  = 42
                     , activeFactor = 42
                     }-}
defaultsRTOptions :: IO RTOptions
defaultsRTOptions = return $ RTOptions { nbThreads    = 4
                                       , stdGCFuel    = 42
                                       , quickGCFuel  = 42
                                       , activeFactor = 42
                                       }
