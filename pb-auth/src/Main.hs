{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiWayIf #-}


module Main
  ( main
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
import Control.Monad
import Control.Monad.Random
import System.Environment
import Yesod.Core.Json
import Data.Char

data PBAuth = PBAuth
              { pbRedis :: Pool Connection
              }

mkYesod "PBAuth" [parseRoutes|
/aii/v1/auth AuthR  POST PUT DELETE
|]

instance Yesod PBAuth


postAuthR :: Handler Value
postAuthR = do
  token <- lookupPostParam "token"
  case token of
    Nothing -> return $ object
               [ "status"      .= ("error" :: T.Text)
               , "meg"         .= ("need token" :: T.Text)
               , "status-code" .= (10 :: Int)
               ]
    Just t  -> (pbRedis <$> getYesod)
      >>= \cp -> liftIO $ withResource cp
      (\c -> do
          let token' = TE.encodeUtf8 t
          rep <- runRedis c $ do
            r <- get token'
            e <- ttl token'
            case (e,r) of
              (Left t,_)          -> return $ Left t
              (_,Left t)          -> return $ Left t
              (Right e',Right r') -> return $ Right (e',r')
          case rep of
            Left x             -> return $ object
                                  [ "status"      .= ("error" :: T.Text)
                                  , "meg"         .= show x
                                  , "status-code" .= (11 :: Int)
                                  ]
            Right (_,Nothing)  -> return $ object
                                  [ "status"  .= ("success" :: T.Text)
                                  , "content" .= (0 :: Int)
                                  ]
            Right (e',Just r') -> do
              let r = read $ B.unpack r'
              newE <-
                if e' < 300
                then runRedis c $ do
                  expire token' $ if r < 2 then 1800 else 7200
                  Right newE <- ttl token'
                  return newE
                else return e'
              return $ object
                [ "status"    .= ("success" :: T.Text)
                , "right"     .= (r :: Int)
                , "newExpire" .= newE
                ]
      )


newmapChar :: Int -> Char
newmapChar i =
  let digit = i + 0x30
      upChr = if digit > 0x39 then digit + 0x07 else digit
      loChr = if upChr > 0x5A then upChr + 0x6  else upChr
  in chr loChr

retryNewKey :: (B.ByteString -> IO Bool)
            -> IO B.ByteString
retryNewKey exst = do
  randStr <- B.pack . map newmapChar . take 64 <$> getRandomRs (0,61)
  rt <- exst randStr
  if rt
    then retryNewKey exst
    else return randStr

putAuthR :: Handler Value
putAuthR = do
  right' <- lookupPostParam "right"
  case right' of
    Nothing -> return $ object
               [ "status"      .= ("error" :: T.Text)
               , "msg"         .= ("need right" :: T.Text)
               , "status-code" .= (12 :: Int)
               ]
    Just r  -> (pbRedis <$> getYesod)
      >>= \cp -> liftIO $ withResource cp
      (\c ->
         let r' = read $ T.unpack r
         in if r' <  1
            then return $ object
                     [ "status" .= ("success" :: T.Text)
                     , "nextExpire" .= (0 :: Int)
                     ]
            else do
           randStr <- retryNewKey $ \k -> liftIO $ runRedis c $ do
             rt <- exists k
             return $ case rt of
               Right False -> False
               _           -> True
           e <-  runRedis c $ do
             set randStr $ TE.encodeUtf8 r
             expire randStr $ if r' < 2 then 1820 else 7220
             Right e <- ttl randStr
             return e
           return $ object
             [ "status"      .= ("success" :: T.Text)
             , "status-code" .= (0 :: Int)
             , "token"       .= TE.decodeUtf8 randStr
             , "nextExpire"  .= e
             ]
      )

deleteAuthR :: Handler Value
deleteAuthR = do
  token <- lookupPostParam "token"
  case token of
    Nothing -> return $ object
               [ "status"      .= ("error" :: T.Text)
               , "meg"         .= ("need token" :: T.Text)
               , "status-code" .= (10 :: Int)
               ]
    Just t  -> (pbRedis <$> getYesod)
      >>= \cp -> liftIO $ withResource cp
      (\c -> do
          let token' = TE.encodeUtf8 t
          rt <- runRedis c $ del [token']
          case rt of
            Left t  -> return $ object
                       [ "status"      .= ("error" :: T.Text)
                       , "meg"         .= show t
                       , "status-code" .= (11 :: Int)
                       ]
            Right v -> return $ object
                       [ "status"      .= ("success" :: T.Text)
                       , "status-code" .= v
                       ]
          )


main :: IO ()
main = do
  port':connHost:connPort':_ <- getArgs
  let connPort = read connPort'
      port     = read port'
  redisConnectionPool <-
    createPool
    ( connect defaultConnectInfo
      { connectHost = connHost
      , connectPort = PortNumber connPort
      }
    )
    (\c -> void $ runRedis c $ quit)
    100
    1000
    200
  warp port $ PBAuth redisConnectionPool
