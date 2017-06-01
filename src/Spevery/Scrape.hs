{-# LANGUAGE OverloadedStrings #-}
module Spevery.Scrape (run) where

import           Control.Concurrent (threadDelay)
import           Control.Lens (view, views)
import           Data.Aeson (decode)
import qualified Data.Char as Char
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BS
import           Data.List (nub)
import           Data.Maybe (catMaybes, listToMaybe, maybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network.Wreq (Response, get, responseBody)
import           Spevery.SQLite (bulkPush)
import           Spevery.Types (DBPath, Resp, cont, titles)


getR :: String -> IO (Response BS.ByteString)
getR c = get $ "https://en.wikipedia.org/w/api.php" <>
               "?format=json&action=query&list=embeddedin" <>
               "&eititle=Template%3AInfobox%20person" <>
               "&redirects&eilimit=max&einamespace=0" <>
               (if null c then "" else "&eicontinue=" ++ c)

decodeResp :: Response BS.ByteString -> Maybe Resp
decodeResp = views responseBody decode

saveResp :: DBPath -> Resp -> IO (Either T.Text ())
saveResp db r = bulkPush db $ views titles filterNames r

filterNames :: [String] -> [T.Text]
filterNames = map T.pack . filter nameFilter
  where nameFilter s = let ws = catMaybes $ listToMaybe <$> words s
                        in not (null ws)
                           && and (Char.isUpper <$> ws)
                           && ((length . nub) ws > 1)


run' :: DBPath -> String -> IO ()
run' dbPath url = do r <- getR url
                     case decodeResp r of
                        Nothing -> putStrLn "bad response from wikipedia"
                        Just resp -> do sr <- saveResp dbPath resp
                                        case sr of
                                          Left e -> TIO.putStrLn e
                                          Right _ -> case view cont resp of
                                                       Nothing -> putStrLn "done"
                                                       Just c -> do threadDelay 20000
                                                                    run' dbPath c

run :: DBPath -> IO ()
run dbPath = run' dbPath ""

