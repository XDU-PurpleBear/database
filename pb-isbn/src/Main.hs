{-# LANGAUGE ViewPatterns      #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}


module Main
  ( main
  ) where

import           Control.Monad
import           Control.Monad.Random
import           Data.Aeson
import qualified Data.ByteString       as B hiding (pack, unpack)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import           Data.Char
import           Data.Pool
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.Types
import           System.Environment
import           Yesod.Core
import           Yesod.Core.Json
import           Yesod.Core.Content
import           Data.UUID.Types hiding(null)

data PB_ISBN = PB_ISBN
             { pbDatabase :: Pool Connection
             }

data Record = Record
            { rISBN :: Int
            , rLC   :: String
            , rTitle :: String
            , rAuths :: [String]
            , rPublisher :: String
            , rEdition :: Int
            , rPublishData :: Day
            , rImg  :: UUID
            , rTags :: [String]
            , rAbstract :: String
            }
            deriving(Show,Eq)

instance ToJSON Record where
  toJSON Record{..} = object
    [ "isbn" .= rISBN
    , "lc" .= rLC
    , "title" .= rTitle
    , "auths" .= rAuths
    , "publisher" .= rPublisher
    , "publish-data" .= rPublishData
    , "img-uuid" .= show rImg
    , "tags" .= rTags
    , "abstract" .= rAbstract
    ]

instance FromRow Record where
  fromRow = Record
            <$> field <*> field
            <*> field <*> (PGArray <$> field)
            <*> field <*> field
            <*> field <*> field
            <*> field <*> field

mkYesod "PB_ISBN" [parseRoutes|
/api/v1/isbn/#Int ISBNR GET
/api/v1/cover/#String CoverR GET
|]

instance Yesod PB_ISBN

getISBNR :: Int -> Handler Value
getISBNR isbn = do
  cp <-  pbDatabase <$> getYesod
  rt <- liftIO $ withResource cp $ \c -> do
    query c [sql|
                SELECT isbn,lc,title,auths,publisher,publish_date,img_uuid,tags,abstract
                FROM table_upstream
                WHERE isbn = ?
                |]
      (Only isbn)
  if null rt
      then notFound
      else returnJson (head rt :: Record)

getCoverR :: String -> Handler TypedContent
getCoverR uuid' = do
  let uuid = read uuid' :: UUID
  cp <- pbDatabase <$> getYesod
  rt <- liftIO $ withResource cp $ \c ->
    query c [sql|
                SELECT uuid,img,mime
                FROM table_image
                WHERE uuid = ?
                |]
      (Only uuid)
  if null rt
      then notFound
      else
      let (_,img,mime) = head rt :: (UUID,B.ByteString,B.ByteString)
      in respondSource mime $ do
        sendChunkBS img
        sendFlush


main :: IO ()
main = do
  port':dbHost' <- getArgs
  let dbHost = unwords dbHost'
      port   = read port'
  pgPool <-
    createPool
    (connectPostgreSQL $ B.pack dbHost)
    close
    100
    1000
    200
  warp port $ PB_ISBN pgPool
