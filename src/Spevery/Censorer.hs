{-# LANGUAGE OverloadedStrings #-}
module Spevery.Censorer (censor) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Set (Set, member)
import qualified Data.Text as T
import           Spevery.Types (WordsPath)

badWords :: WordsPath -> IO (Set T.Text)
badWords bwf = do
  s <- BS.readFile bwf
  case decode s :: Maybe (Set T.Text) of
    Nothing -> error $ "bad file: " ++ bwf
    Just bws -> return bws

censor' :: Set T.Text -> [[T.Text]] -> [[T.Text]]
censor' bws = filter f
  where f ws = not $ any (`member` bws) ws

censor :: WordsPath -> [[T.Text]] -> IO [[T.Text]]
censor pth ss = censor' <$> badWords pth <*> pure ss


