module Spevery.Logger
  ( logTo
  ) where

import Spevery.Types (LogPath)
import System.Log.FastLogger (ToLogStr, newFileLoggerSet, pushLogStrLn, toLogStr)

logTo :: ToLogStr a => LogPath -> a -> IO ()
logTo logPath s = do l <- newFileLoggerSet 1 logPath
                     pushLogStrLn l $ toLogStr s

