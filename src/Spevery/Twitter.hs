{-# LANGUAGE OverloadedStrings #-}
module Spevery.Twitter (post) where

import           Control.Lens (views)
import           Data.Aeson (decode)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BS
import           Data.Text (Text)
import           Web.Twitter.Conduit (Credential(..), TWInfo, call, def, newManager,
                                      oauthConsumerKey, oauthConsumerSecret,
                                      setCredential, tlsManagerSettings, twitterOAuth, update)
import           Spevery.Types (Creds, CredsPath, consumerKey, consumerSecret, token, secret)

creds :: Creds -> TWInfo
creds cs = setCredential t c def
  where t = twitterOAuth { oauthConsumerKey = getKey consumerKey
                         , oauthConsumerSecret = getKey consumerSecret
                         }
        c = Credential [ ("oauth_token", getKey token)
                       , ("oauth_token_secret", getKey secret)
                       ]
        getKey k = views k C.pack cs

getCreds :: CredsPath -> IO TWInfo
getCreds path = do f <- BS.readFile path
                   case decode f :: Maybe Creds of
                     Nothing -> error "missing creds"
                     (Just cs) -> return $ creds cs


post :: CredsPath -> Text -> IO ()
post credsPath txt = do
  mgr <- newManager tlsManagerSettings
  cs <- getCreds credsPath
  _ <- call cs mgr $ update txt
  return ()

