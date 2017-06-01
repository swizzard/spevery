{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Spevery.Types
  ( Creds
  , consumerKey
  , consumerSecret
  , token
  , secret
  , CredsPath
  , DBPath
  , LogPath
  , Resp
  , cont
  , titles
  , WordsPath
  ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))

type CredsPath = FilePath
type DBPath = FilePath
type LogPath = FilePath
type WordsPath = FilePath

data Creds = Creds { _consumerKey :: String
                   , _consumerSecret :: String
                   , _token :: String
                   , _secret :: String
                   } deriving (Show)

makeLenses ''Creds

instance FromJSON Creds where
  parseJSON = withObject "Creds" $
    \v -> Creds <$> v .: "consumerKey"
                <*> v .: "consumerSecret"
                <*> v .: "token"
                <*> v .: "secret"


data Resp = Resp { _cont :: Maybe String
                 , _titles :: [String]
                 } deriving (Show)

makeLenses ''Resp

instance FromJSON Resp where
  parseJSON = withObject "Resp" $
    \v -> Resp <$> (v .:? "continue" >>= traverse (.: "eicontinue"))
               <*> (v .: "query"
                    >>= (.: "embeddedin")
                    >>= traverse (.: "title"))

