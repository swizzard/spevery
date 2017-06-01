{-# LANGUAGE OverloadedStrings #-}
module Spevery.Output where

import           Control.Applicative ((<|>))
import qualified Data.Text as T
import           Spevery.Censorer (censor)
import           Spevery.Spoon (getSpoons)
import           Spevery.Types (WordsPath)

maxLen :: Int
maxLen = 140

chk :: (a -> Bool) -> a -> Maybe a
chk f a = if f a then Nothing else Just a

checkTooLong :: T.Text -> Maybe T.Text
checkTooLong = chk $ (>= maxLen) . T.length

checkEmpty :: T.Text -> Maybe T.Text
checkEmpty = chk T.null

buildS :: [T.Text] -> T.Text
buildS = T.take maxLen . T.intercalate " "

buildAcc :: Maybe T.Text -> [T.Text] -> Maybe T.Text
buildAcc acc ss = fmap (\a -> T.unlines [a, buildS ss]) acc

prepSpoons :: WordsPath -> T.Text -> IO (Maybe T.Text)
prepSpoons pth = fmap (foldl combineSpoons Nothing) . censor pth . getSpoons
  where combineSpoons :: Maybe T.Text -> [T.Text] -> Maybe T.Text
        combineSpoons acc s = (buildAcc acc s >>= checkTooLong)
                           <|> (acc >>= checkEmpty)
                           <|> checkEmpty (buildS s)

