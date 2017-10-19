{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

-- the kind for book

module Spider.Book.Kind
  ( addByISBN
  , BookKind(..)
  , parseBookKind
  , insertBookKind
  , fetchBookCover
  , fetchBookInfo
  ) where


import Text.Parsec 
import Data.List
import Data.Maybe
import Data.Char
import Data.List.Split(splitOn)
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types
import qualified Data.ByteString as B hiding (pack,unpack)
import qualified Data.ByteString.Char8 as B (pack,unpack)
import qualified Network.HTTP as HTTP
import Network.HTTP(Request(..))
import qualified Network.Browser as Browser
import Text.Printf
import Control.Exception as E
import Control.Monad.Random
import Spider.Book.Copy

data BookKind a = BookKind
  { bkTitle :: a
  , bkCoverUrl :: a
  , bkAuthor :: [a]
  , bkISBN   :: Int
  , bkPublisher :: a
  }
  deriving (Show)

parseBookKind :: Parsec String () (BookKind String)
parseBookKind = do
  skipHead
  skipLine >>  spaces >> skipNewLine
  pairs <- many readKeyValue
  return BookKind
    { bkTitle    = lookupFM "NaN" "title" pairs
    , bkCoverUrl = lookupFM "NaN" "cover" pairs
    , bkAuthor   = filter (not.null) $  splitOn "," (lookupFM "" "author" pairs)
    , bkISBN     = read (lookupFM "0" "isbn13" pairs)
    , bkPublisher = lookupFM "NaN" "publisher" pairs
    }
  where
    skipHead = skipMany (noneOf "-")
    skipLine = skipMany (char '-')
    skipNewLine = skipMany (oneOf "\r\n")
    lookupFM d k ps = fromMaybe d $ lookup k ps
    readKeyValue = do
      spaces
      key <- map toLower <$> many (letter <|> digit <|> space)
      char ':'
      spaces
      value <- (reverse . dropWhile (==' ') . reverse . dropWhile (==' ')) <$> many (noneOf "\r\n")
      spaces
      skipNewLine
      return (key,value)

insertBookKind :: Connection ->  BookKind String -> Maybe B.ByteString -> IO ()
insertBookKind c BookKind{..} img = do
  now <- getCurrentTime
  rt  <- execute c [sql| INSERT  INTO table_book_kind
                      ( key_isbn, key_clc, key_name, key_auth
                      , key_publisher, key_edition, key_publish_date
                      , key_imgs) VALUES (?,?,?,?,?,?,?,?) |]
    (bkISBN,"NaN",bkTitle,PGArray bkAuthor,bkPublisher,1::Int,now,Binary <$> img)
  putStrLn $ "insert " ++ show rt ++ "line(s)"
  return ()


instance Functor Request where
  fmap f r = Request
    { rqURI = rqURI r
    , rqMethod = rqMethod r
    , rqHeaders = rqHeaders r
    , rqBody = f $ rqBody r 
    }

fetchBookCover :: BookKind String -> IO (Maybe B.ByteString)
fetchBookCover BookKind{..} = do
  (_, rsp) <- Browser.browse $ do
    Browser.setAllowRedirects True
    Browser.request $ B.pack <$> HTTP.getRequest bkCoverUrl
  return $ case HTTP.rspCode rsp of
    (2,0,0) -> Just $ HTTP.rspBody rsp
    _       -> Nothing


fetchBookInfo :: Int -> IO (Maybe (BookKind String))
fetchBookInfo isbn = do
  let url = "http://www.openisbn.com/download/" ++ printf "%010d" isbn ++ ".txt"
  rsp <- HTTP.simpleHTTP $ HTTP.getRequest url
  HTTP.getResponseCode rsp >>=
    (\code -> case code of
        (2,0,0) -> do
          txt <- HTTP.getResponseBody rsp
          return $ case runParser parseBookKind () "response body" txt of
            Left  _ -> Nothing
            Right i -> Just i
        _       -> return Nothing
        )

addByISBN :: Connection -> Int -> IO ()
addByISBN c isbn = insertCopy =<< update =<< fetchImg =<< fetchBookInfo isbn
  where fetchImg Nothing    = do
          putStrLn $ "Book(" ++ printf "%010d" isbn ++ ") is unavailable."
          return (Nothing,Nothing)
        fetchImg b@(Just i) = (,) <$> pure b <*> fetchBookCover i
        update (Nothing,Nothing) = return Nothing
        update (Just b ,img)     = do
          rt <- E.try $ insertBookKind c b img :: IO (Either SomeException ())
          print rt
          return $ Just isbn
        insertCopy Nothing  = return ()
        insertCopy (Just i) = do
          n <- getRandomR (2,6)
          replicateM_ n $ insertBookCopy c i
          return ()
