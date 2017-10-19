{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Main where


import           Control.Monad
import           Data.Word
import           Database.PostgreSQL.Simple
import           Spider.Book.Kind
import           System.Console.CmdArgs
import           System.Environment

data PBSArgs = PBSArgs
             { dbHost    :: String
             , dbPort    :: Word16
             , dbUser    :: String
             , dbPass    :: String
             , dbName    :: String
             , startISBN :: Int
             , endISBN   :: Int
             }
             deriving (Show,Data,Typeable)

pbsArgs = PBSArgs
  { dbHost = "localhost"
    &= typ "URL"
    &= help "host name of db"
    &= explicit
    &= name "host"
    &= name "h"
  , dbPort = 5432
    &= typ "PORT"
    &= help "port of db"
    &= explicit
    &= name "port"
    &= name "p"
  , dbUser = "postgres"
    &= typ "USER"
    &= help "name of user"
    &= name "user"
    &= name "u"
  , dbPass = "postgres"
    &= typ "PASSWORD"
    &= help "password"
    &= explicit
    &= name "passwd"
    &= name "w"
  , dbName = "postgres"
    &= typ "database(name)"
    &= help "name pf database"
    &= explicit
    &= name "name"
    &= name "n"
  , startISBN = 0
    &= typ "INT"
    &= help "start isbn for fetching"
    &= name "start"
    &= name "s"
  , endISBN = maxBound
    &= typ "INT"
    &= help "end isbn for fetching"
    &= name "end"
    &= name "e"
  }

main = do
  PBSArgs{..} <- cmdArgs pbsArgs
  c <- connect ConnectInfo
    { connectHost = dbHost
    , connectPort = dbPort
    , connectUser = dbUser
    , connectPassword = dbPass
    , connectDatabase = dbName
    }
  forM_ [startISBN..endISBN] $ \isbn -> addByISBN c isbn
