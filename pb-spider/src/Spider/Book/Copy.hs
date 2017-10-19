{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Spider for Book from OpenISBN

module Spider.Book.Copy
  ( insertBookCopy
  ) where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Text.Printf


insertBookCopy :: Connection -> Int -> IO ()
insertBookCopy c isbn = do
  rt <- execute c [sql| INSERT INTO table_book_instance
                      ( key_uuid, key_isbn, key_status)
                      VALUES (uuid_generate_v4(),?,'a---')
                      |] (Only (printf "%010d" isbn :: String))
  putStrLn $ "insert " ++ show rt ++ " book instance(s)"
  return ()
