{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}


module Main
  (
  ) where

import Yesod.Core
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Pool
import Database.Redis
import Data.Time
import qualified Data.ByteString as B hiding (pack,unpack)
import qualified Data.ByteString.Char8 as B (pack,unpack)


data PBRight = RBRight { expire :: UTCTime
                       , right  :: Double
                       }
               deriving (Show,Read)


data PBAuth = PBAuth
              { pbRedis :: Pool Connection
              }

mkYesod "PBAuth" [parseRoute|
/aii/v1/auth   GET POST
/aii/v1/unauth GET
|]

instance Yesod PBAuth


getAuth :: Handler Value
getAuth = do
  token <- lookupGetParam "token"
  case token of
    Nothing -> return [ "status" .= ("error" :: T.Text)
                      , "meg" .= ("need token" :: T.Text)
                      , "status-code" .= 10
                      ]
    Just t  -> (pbRedis <$> getYesod)
      >>= \cp -> withResource cp
      (\c -> do
          rep <- runRedis c $ get TE.encodeUtf8 token
          case rep of
            Left x -> return [ "ststus" .= ("error" :: T.Text)
                             , "meg" .= show x
                             , "status-code" .= 11
                             ]
            Right Nothing -> return [ "status" .= ("success" :: T.Text)
                                    , "contant" = 0
                                    ]
            Right (Just str) -> do
              now <- getCurrentTime
              let r = read $ B.unpack str
                  diff = diffUTCTime (expire r) now > 0
              (realR,nextExpire) <-
                if | diff > 300 -> return $ (right r,expire r)
                   | diff > 0   ->
                     let addTime = if right r < 2 then 1800 else 7200
                     in return $ (right r,addUTCTime addTime $ expire r)
                   | otherwise -> return (0,now)
              return [ "status" .= "success"
                     , "right" .= realR
                     , "nextExpire" .= diffUTCTime nextExpire now
                     ]
          )




main :: IO ()
main = putStrLn "Hello, Haskell!"
