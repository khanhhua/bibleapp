{-# LANGUAGE OverloadedStrings #-}

module Main where
import Prelude hiding (takeWhile)
import System.IO (Handle, IOMode (ReadMode), hClose, hIsEOF, openFile)

import Control.Monad (foldM, replicateM, replicateM_, when)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), Reader, ReaderT (runReaderT), runReader)
import Data.List (break)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T (Text, breakOn, drop, length, unpack, unwords)
import qualified Data.Text.IO.Utf8 as T (hGetLine)

data Verse = Verse
  { addr :: String
  , text :: T.Text
  }
  deriving (Show)

data Book = Book
  { name :: String
  , verses :: [Verse]
  }

type VerseReaderT = ReaderT Handle IO (Maybe Verse)

main :: IO ()
main = do
  bookname <- putStrLn "Bookname: " >> getLine
  mbBook <- findBook bookname 
  when (isJust mbBook) $ do
    let book = fromJust mbBook
    print $ "Found: " <> show (length $ verses book)
    print $ "Name: " <> name book
    print $ "Verse: " <> text (head $ verses book)

findBook :: String -> IO (Maybe Book)
findBook bookname = do
  hData <- openFile "data/kjv.txt" ReadMode
  replicateM_ 2 $ T.hGetLine hData
  mbBook <- runReaderT (selectBook bookname) hData 
  hClose hData

  return mbBook

verseReader :: VerseReaderT
verseReader = do
  hData <- ask
  line <- liftIO (T.hGetLine hData)
  isEof <- liftIO $ hIsEOF hData
  if isEof
    then return Nothing
    else return $ Just (parseVerse line)

findAll :: (Verse -> Bool) -> ReaderT Handle IO [Verse]
findAll predicate = reverse <$> collect []
 where
  collect :: [Verse] -> ReaderT Handle IO [Verse]
  collect acc = do
    mbVerse <- verseReader
    case mbVerse of
      Nothing -> return acc
      Just x ->
        if predicate x
          then collect (x : acc)
          else collect acc

takeWhile :: (Verse -> Bool) -> ReaderT Handle IO [Verse]
takeWhile predicate = reverse <$> collect False []
 where
  collect :: Bool -> [Verse] -> ReaderT Handle IO [Verse]
  collect found acc = do
    mbVerse <- verseReader
    case mbVerse of
      Nothing -> return acc
      Just x ->
        if predicate x
          then collect True (x : acc)
          else if found
            then return acc
            else collect False acc


selectBook :: String -> ReaderT Handle IO (Maybe Book)
selectBook name = do
  verses <- takeWhile ((==) name . bookname)
  if null verses
    then return Nothing
    else return $ Just (Book name verses)

bookname :: Verse -> String
bookname (Verse address _) =
  let (book, _) = break (' ' ==) address
  in book

parseVerse :: T.Text -> Verse
parseVerse line =
  let
    (addr, text) = T.breakOn "\t" line
   in
    Verse (T.unpack addr) (T.drop 1 text)
