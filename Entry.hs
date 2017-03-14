{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Entry where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import qualified Data.Text.Lazy as T
import Text.Hastache

data Entry = Entry Int String String Int Int Int String deriving (Show)

instance FromRow Entry where 
  fromRow = Entry <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Entry where
  toRow (Entry id title desc date views author_id author) = toRow (id, title, desc, date, views, author_id, author)

mkEntryContext (Entry id title desc date views author_id author) =
  (\case field | field == "id" -> MuVariable id
               | field == "title" -> MuVariable title
               | field == "desc" -> MuVariable desc
               | field == "date" -> MuVariable date
               | field == "views" -> MuVariable views
               | field == "author_id" -> MuVariable author_id
               | field == "author" -> MuVariable author)

data VideoId = VideoId Int
instance FromRow VideoId where fromRow = VideoId <$> field
instance Show VideoId where show (VideoId id) = show id

-- Methods for listing videos

getEntryList :: IO [Entry]
getEntryList = do
  conn <- open "Database.db"
  r <- query_ conn "SELECT entries.id, title, entries.desc, date, views, author_id, name AS author_name FROM entries JOIN authors ON entries.author_id = authors.id" :: IO [Entry]
  return r

-- https://hackage.haskell.org/package/sqlite-simple-0.4.9.0/docs/Database-SQLite-Simple.html#v:queryNamed
getEntryListFromAuthor :: T.Text -> IO [Entry]
getEntryListFromAuthor author_id = do
  conn <- open "Database.db"
  r <- queryNamed conn "SELECT entries.id, title, entries.desc, date, views, author_id, name AS author_name FROM entries JOIN authors ON entries.author_id = authors.id WHERE authors.id = :author_id" [":author_id" := author_id] :: IO [Entry]
  return r

getEntryById :: T.Text -> IO [Entry]
getEntryById id = do
  conn <- open "Database.db"
  r <- queryNamed conn "SELECT entries.id, title, entries.desc, date, views, author_id, name AS author_name FROM entries JOIN authors ON entries.author_id = authors.id WHERE entries.id = :id" [":id" := id] :: IO [Entry]
  incrementViewcount id
  return r

getNextVideoId :: IO VideoId
getNextVideoId = do
  conn <- open "Database.db"
  r <- query_ conn "SELECT MAX(id)+1 FROM entries"
  return $ head r

-- Adding and updating videos

incrementViewcount :: T.Text -> IO ()
incrementViewcount id = do
  conn <- open "Database.db"
  executeNamed conn "UPDATE entries SET views=views+1 WHERE entries.id = :id" [":id" := id] :: IO ()

createEntry :: T.Text -> T.Text -> T.Text -> IO ()
createEntry title desc author_id = do
  conn <- open "Database.db"
  -- Sanitization is done by sqlite-simple and hastache
  executeNamed conn "INSERT INTO entries (title, desc, date, views, author_id) VALUES (:title, :desc, strftime('%s','now'), 0, :author)" [":title" := title, ":desc" := desc, ":author" := author_id]
