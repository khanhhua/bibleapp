{-# LANGUAGE OverloadedStrings #-}
module Data.Bible where

import qualified Data.Text as T (Text, breakOn, drop, length, pack, unpack, unwords)

data Verse = Verse
  { addr :: String
  , text :: T.Text
  }
  deriving (Show)

data Book = Book
  { name :: String
  , verses :: [Verse]
  }

bookname :: Verse -> String
bookname (Verse address _) =
  let segments = init $ words address
   in unwords segments

verseNo :: Verse -> String
verseNo (Verse address _) =
  last (words address)

chapterNo :: Verse -> String
chapterNo v =
  let no = verseNo v
   in if ':' `elem` no
        then fst $ break (== ':') no
        else no

parseVerse :: T.Text -> Verse
parseVerse line =
  let
    (addr, text) = T.breakOn "\t" line
   in
    Verse (T.unpack addr) (T.drop 1 text)