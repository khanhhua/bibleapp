module Control.Bible.Monads where

import Prelude hiding (takeWhile)
import System.IO (BufferMode (NoBuffering), Handle, IOMode (ReadMode), hClose, hIsEOF, openFile)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), Reader, ReaderT (runReaderT))
import Control.Monad (replicateM_)
import qualified Data.Text.IO.Utf8 as T (hGetLine)
import qualified Data.Text as T (words, pack)

import Data.Bible

type VerseReaderT = ReaderT Handle IO (Maybe Verse)

verseReader :: VerseReaderT
verseReader = do
  hData <- ask
  line <- liftIO (T.hGetLine hData)
  isEof <- liftIO $ hIsEOF hData
  if isEof
    then return Nothing
    else return $ Just (parseVerse line)

filterBy :: (Verse -> Bool) -> ReaderT Handle IO [Verse]
filterBy predicate = reverse <$> collect []
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
          else
            if found
              then return acc
              else collect False acc

selectBook :: String -> ReaderT Handle IO (Maybe Book)
selectBook name = do
  verses <- takeWhile ((==) name . bookname)
  if null verses
    then return Nothing
    else return $ Just (Book name verses)

selectBooknames :: ReaderT Handle IO [String]
selectBooknames = reverse <$> collect []
 where
  collect acc = do
    mbVerse <- verseReader
    case mbVerse of
      Nothing -> return acc
      Just x -> do
        let name = bookname x
        if name `elem` acc
          then collect acc
          else collect (name : acc)

withDataHandle :: ReaderT Handle IO a -> IO a
withDataHandle reader = do
  hData <- openFile "data/kjv.txt" ReadMode
  replicateM_ 2 $ T.hGetLine hData
  x <- runReaderT reader hData
  hClose hData

  return x

-- Composite IO utility to fetch Book names, Verses...

listBooknames :: IO [String]
listBooknames = withDataHandle selectBooknames

getBook :: String -> IO (Maybe Book)
getBook bookname = withDataHandle (selectBook bookname)

findVerses :: String -> IO [Verse]
findVerses term = withDataHandle (filterBy matchTerm)
 where
  matchTerm (Verse _ text) =
    let tokens = T.words text
     in elem (T.pack term) tokens