{-# LANGUAGE OverloadedStrings #-}

module Spevery.SQLite
  ( bulkPush
  , clear'
  , pop
  ) where

import           Control.Exception (try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Database.SQLite.Simple (Connection, Only(..), Query, SQLError,
                                         ToRow, execute, execute_, executeMany,
                                         field, open, query_, toRow, withTransaction)
import           Database.SQLite.Simple.FromRow (FromRow, fromRow)
import           Spevery.Types (DBPath)

bsToT :: C.ByteString -> T.Text
bsToT = T.pack . C.unpack

tableName :: Query
tableName = " names "

indexName :: Query
indexName = " name_idx "

data NameRow = NameRow Int T.Text deriving (Show)

instance FromRow NameRow where
  fromRow = NameRow <$> field <*> field

instance ToRow NameRow where
  toRow (NameRow pk name) = toRow (pk, name)

showE :: SQLError -> Either T.Text a
showE = Left . T.pack . show

withDB :: (Connection -> IO a) -> DBPath -> IO a
withDB f dbName = do c <- open dbName
                     f c

initialize' :: Connection -> IO (Either T.Text ())
initialize' conn = do
  res <- try $ do execute_ conn $ "CREATE TABLE IF NOT EXISTS"
                                  <> tableName <>
                                  "(id INTEGER PRIMARY KEY, name TEXT)"
                  execute_ conn $ "CREATE INDEX IF NOT EXISTS"
                                  <> indexName <> "ON"
                                  <> tableName <> "(name)"
  return $ case res of
        Left e -> showE e
        Right _ -> Right ()

initialize = withDB initialize'

bulkPush' :: [T.Text] -> Connection -> IO (Either T.Text ())
bulkPush' ss conn = do
  i0 <- initialize' conn
  case i0 of
    Left e -> return $ Left e
    Right _ -> do
      res <- try $ do withTransaction conn $
                        executeMany conn ("INSERT INTO" <>
                                          tableName <>
                                          "(name) VALUES (?)")
                                         (Only <$> ss)
      return $ case res of
            Left e -> showE e
            Right _ -> Right ()

bulkPush c t = withDB (bulkPush' t) c

pop' :: Connection -> IO (Either T.Text T.Text)
pop' conn = do
  res <- try $ do withTransaction conn $ do
                    r <- query_ conn $ "SELECT id, name FROM"
                                       <> tableName <>
                                       "ORDER BY name ASC LIMIT 1"
                    case r of
                      [] -> return $ Left "no names"
                      ((NameRow pk name):_) -> do
                        execute conn ("DELETE FROM" <> tableName <> "WHERE id = ?") (Only pk)
                        return $ Right name
  return $ case res of
       Left e -> showE e
       Right (Left msg) -> Left msg
       Right (Right n) -> Right n

pop = withDB pop'

clear'' :: Connection -> IO (Either T.Text T.Text)
clear'' conn = do
  res <- try $ do execute_ conn $ "DROP TABLE IF EXISTS" <> tableName
                  execute_ conn $ "DROP INDEX IF EXISTS" <> indexName
  return $ case res of
             Left e -> showE e
             Right _ -> Right "ok"

clear' = withDB clear''

