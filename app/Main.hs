module Main where

import Data.Monoid ((<>), mempty)
import Options.Applicative
import Options.Applicative.Builder
import Lib

data Cmd = Scrape String
         | Clear String
         | Tweet { dbPath :: String
                 , wordsPath :: String
                 , logPath :: String
                 , credsPath :: String
                 }

runWithOpts :: Cmd -> IO ()
runWithOpts (Clear dbPath) = clear dbPath
runWithOpts (Scrape dbPath) = scrape dbPath
runWithOpts (Tweet dbPath wordsPath logPath credsPath) = tweet dbPath wordsPath logPath credsPath

dbh = strOption (long "dbPath" <>
                 short 'd' <>
                 metavar "PATH" <>
                 help "path to SQLite3 db")

sc :: Parser Cmd
sc = Scrape <$> dbh

cl :: Parser Cmd
cl = Clear <$> dbh

tw :: Parser Cmd
tw = Tweet <$>  dbh <*>
                strOption (long "words" <>
                           short 'w' <>
                           metavar "PATH" <>
                           help "path to wordfilter wordlist") <*>
                strOption (long "log" <>
                          short 'l' <>
                          metavar "PATH" <>
                          help "path to log file") <*>
                strOption (long "creds" <>
                           short 'c' <>
                           metavar "PATH" <>
                           help "path to creds file")


main :: IO ()
main = customExecParser p opts >>= runWithOpts
  where parser = subparser
          ( command "tweet" (info tw (progDesc "Tweet spoonerism"))
           <>
            command "scrape" (info sc (progDesc "Scrape from wikipedia"))
           <>
            command "empty" (info cl (progDesc "Empty queue"))
          )
        opts = info (parser <**> helper) idm
        p = prefs showHelpOnError

