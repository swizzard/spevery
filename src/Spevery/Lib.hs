{-# LANGUAGE OverloadedStrings #-}
module Spevery.Lib
  ( clear
  , scrape
  , tweet
  ) where

import qualified Data.Text.IO as TIO
import           Spevery.Logger (logTo)
import           Spevery.Output (prepSpoons)
import           Spevery.Scrape (run)
import           Spevery.SQLite (clear', pop)
import           Spevery.Twitter (post)
import           Spevery.Types (CredsPath, DBPath, LogPath, WordsPath)

clear :: DBPath -> IO ()
clear dbPath = do res <- clear' dbPath
                  case res of
                     Left e -> TIO.putStrLn e
                     Right ok -> TIO.putStrLn ok

scrape :: DBPath -> IO ()
scrape = run

tweet :: DBPath -> WordsPath -> LogPath -> CredsPath -> IO ()
tweet dbp wp lp cp = do nextName <- pop dbp
                        either (logTo lp) f nextName
                     where f nn = do ss <- prepSpoons wp nn
                                     maybe (tweet dbp wp lp cp) (post cp) ss

