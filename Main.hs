-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import Web.Scotty
import Control.Monad.IO.Class
import Text.Hastache
import Text.Hastache.Context

import Entry
import Author
import Video

main = scotty 3000 $ do
  get "/" $ do
    list <- liftIO getEntryList
    authors <- liftIO getAuthorList
    let context "list" = MuList $ map (mkStrContext . mkEntryContext) list
    res <- hastacheFile defaultConfig "templates/list.tpl" (mkStrContext context)
    html res
  
  -- Create accounts (channels)
  get "/account" $ do
    res <- hastacheFile defaultConfig "templates/account.tpl" (mkGenericContext ())
    html res
    
  post "/account" $ do
    name <- param "name"
    pass <- param "pass"
    pic <- param "pic"
    desc <- param "desc" 
    liftIO $ createAuthor name desc pic pass
    text "Account created!"
  
  -- Video upload
  get "/upload" $ do
    res <- hastacheFile defaultConfig "templates/upload.tpl" (mkGenericContext ())
    html res
    
  -- ~ post "/upload" $ do
    -- ~ name <- param "name"
    -- ~ pass <- param "pass"
    -- ~ title <- param "title"
    -- ~ desc <- param "desc"
    -- ~ f <- files
    -- ~ newId <- liftIO $ getNextVideoId
    -- ~ isLoginOk <- liftIO $ verifyLogin name pass
    -- ~ if isLoginOk
      -- ~ then let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- f ]
           -- ~ liftIO $ sequence_ [ B.writeFile ("video" </> newId) fc | (_,fn,fc) <- fs' ]
           -- ~ text "ok"
      -- ~ else text "Wrong credentials."

  
  -- List all chanels
  get "/channels" $ do
    authors <- liftIO getAuthorList
    let context "authors" = MuList $ map (mkStrContext . mkAuthorContext) authors
    res <- hastacheFile defaultConfig "templates/channels.tpl" (mkStrContext context)
    html res
  
  -- View a channel
  get "/author/:author" $ do
    author_id <- param "author"
    list <- liftIO (getEntryListFromAuthor author_id)
    authors <- liftIO getAuthorList
    let context "list" = MuList $ map (mkStrContext . mkEntryContext) list
    res <- hastacheFile defaultConfig "templates/list.tpl" (mkStrContext context)
    html res
  
  -- View a vid
  get "/entry/:id" $ do
    id <- param "id"
    list <- liftIO (getEntryById id)
    let context "list" = MuList $ map (mkStrContext . mkEntryContext) list
    res <- hastacheFile defaultConfig "templates/video.tpl" (mkStrContext context)
    html res
    
  get "/static/style.css" $ do
    file "static/style.css"
  
  get "/file/:id" $ do
    id <- param "id"
    file $ "video/" ++ id ++ ".ogv"

  get "/thumb/:id" $ do
    id <- param "id"
    file $ "thumb/" ++ id ++ ".jpg"
