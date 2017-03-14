{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Author where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Text.Lazy as T
import Text.Hastache
import Data.Digest.Pure.SHA
import Data.Text.Lazy.Encoding

data Author = Author Int String String String Int String deriving (Show)

instance FromRow Author where 
  fromRow = Author <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Author where
  toRow (Author id name desc pic is_admin pass_sha256) = toRow (id, name, desc, pic, is_admin, pass_sha256)

mkAuthorContext (Author id name desc pic is_admin pass_sha256) =
  (\case field | field == "id" -> MuVariable id
               | field == "name" -> MuVariable name
               | field == "desc" -> MuVariable desc
               | field == "pic" -> MuVariable pic
               | field == "is_admin" -> MuVariable is_admin
               | field == "pass_sha256" -> MuVariable pass_sha256)

data Count = Count Int deriving (Eq)
instance FromRow Count where fromRow = Count <$> field
instance Show Count where show (Count n) = show n

getAuthorList :: IO [Author]
getAuthorList = do
  conn <- open "Database.db"
  r <- query_ conn "SELECT * FROM authors" :: IO [Author]
  return r

createAuthor :: T.Text -> T.Text -> T.Text -> T.Text -> IO ()
createAuthor name desc pic pass = do
  conn <- open "Database.db"
  -- Sanitization is done by sqlite-simple and hastache
  executeNamed conn "INSERT INTO authors (name, desc, pic, is_admin, pass_sha256) VALUES (:name, :desc, :pic, :is, :pass)" [":name" := name, ":desc" := desc, ":pic" := pic, ":is" := (0 :: Integer), ":pass" := (show $ sha256 $ encodeUtf8 pass)]

verifyLogin :: T.Text -> T.Text -> IO Bool
verifyLogin name pass = do
  conn <- open "Database.db"
  r <- queryNamed conn "SELECT COUNT(id) FROM authors WHERE name=:name AND pass_sha256=:pass" [":name" := name, ":pass" := (show $ sha256 $ encodeUtf8 pass)]
  let num = head r
  let res = case num of
                 (Count 1) -> True
                 _         -> False
  return res
