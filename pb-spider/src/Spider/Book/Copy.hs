-- Spider for Book from OpenISBN

module Spider.Book.Kind
  (
  ) where



{-
Project OpenISBN
-----------------------------------------------------------------------------------
Title: Aleph
Cover:  http://www.openisbn.com/cover/0007435517_72.jpg
Author: Paulo Coelho,
Publisher: HarperCollins Publishers
Pages: 320
ISBN10: 0007435517
ISBN13: 9780007435517
URL: http://www.openisbn.com/isbn/9780007435517/
List Price: 27.50
Price comparison:
URL: http://www.openisbn.com/price/9780007435517/
-----------------------------------------------------------------------------------
http://www.openisbn.com
-}



data Book a = Book
              { bTitle :: a
              , coverUrl :: a
              , author
              }
