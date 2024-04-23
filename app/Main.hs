{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (IOMode(ReadMode), Handle, hIsEOF, openFile, hClose)

import qualified Data.Text as T (Text, breakOn, unwords, unpack, length, drop)
import qualified Data.Text.IO.Utf8 as T (hGetLine)
import Control.Monad.Reader (Reader, runReader, MonadReader (ask), ReaderT (runReaderT), MonadIO (liftIO))
import Control.Monad (foldM, replicateM, replicateM_)

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
  hData <- openFile "data/kjv.txt" ReadMode
  replicateM_ 2 $ T.hGetLine hData
  verses <- runReaderT (findAll (\v -> addr v == "Genesis 1:1")) hData
  print $ "Found: " <> show (length verses)
  print $ "Verse: " <> text (head verses)
  hClose hData

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
            then collect (x:acc)
            else collect acc

parseVerse :: T.Text -> Verse
parseVerse line =
  let
    (addr, text) = T.breakOn "\t" line
  in Verse (T.unpack addr) (T.drop 1 text)