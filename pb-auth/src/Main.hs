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
import Control.Monad
import Control.Moand.Random


data PBAuth = PBAuth
              { pbRedis :: Pool Connection
              }

mkYesod "PBAuth" [parseRoute|
/aii/v1/auth   POST PUT DELETE
|]

instance Yesod PBAuth


postAuth :: Handler Value
postAuth = do
  token <- lookupPostParam "token"
  case token of
    Nothing -> return [ "status"      .= ("error" :: T.Text)
                      , "meg"         .= ("need token" :: T.Text)
                      , "status-code" .= 10
                      ]
    Just t  -> (pbRedis <$> getYesod)
      >>= \cp -> withResource cp
      (\c -> do
          let token' = TE.encodeUtf8 token
          rep <- runRedis c $ do
            r <- get token'
            e <- ttl token'
            case (e,r) of
              (Left t,_)          -> return $ Left t
              (_,Left t)          -> reutrn $ Left
              (Right e',Right r') -> return $ Right (e',r')
          case rep of
            Left x            -> return
                                 [ "status"      .= ("error" :: T.Text)
                                 , "meg"         .= show x
                                 , "status-code" .= 11
                                 ]
            Right (_,Nothing) -> return
                                 [ "status"  .= ("success" :: T.Text)
                                 , "content" .= 0
                                 ]
            Right (Just e,Just r') -> do
              let r = read $ B.unpack r'
              newE <-
                if (e < 300)
                then runRedis c $ do
                  expire token' $ if r < 2 then 1800 else 7200
                  Right newE <- ttl token'
                  return newE
                else return $ Right e
              return [ "status"    .= "success"
                     , "right"     .= r
                     , "newExpire" .= newE
                     ]
      )


newmapChar :: Int -> Char
newmapChar i =
  let digit = i + 0x30
      upChr = if digit > 0x39 then digit + 0x07 else digit
      loChr = if upChr > 0x5A then upChr + 0x6  else upChr
  in chr loChr

retryNewKey :: (B.ByteString -> Handler Bool)
            -> Handler ByteString
retryNewKey exst = do
  randStr <- B.pack . map newmapChar . take 64 <$> getRandomRs (0,61)
  rt <- exst randStr
  if rt
    then retryNewKey exst
    else return randStr

putAuth :: Handler Value
putAuth = do
  right' <- lookupPostParam "right"
  case token of
    Nothing -> return [ "status"      .= ("error" :: T.Text)
                      , "msg"         .= ("need right" :: T.Text)
                      , "status-code" .= 12
                      ]
    Just r  -> (pdRedis <$> getYesod)
      >>= \cp -> withResource cp
      (\c -> do
          randStr <- newmapChar $ \k -> runRedis c $ do
            rt <- exists k
            case rt of
              Right False -> False
              _           -> True
          runRedis c $ do
            set randStr $ B.pack $ show r
            expire randStr $ if r < 2 then 1800 else 7200
          return [ "status"      .= ("success" :: T.Text)
                 , "status-code" .= 0
                 ]
      )             


main :: IO ()
main = putStrLn "Hello, Haskell!"
