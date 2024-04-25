{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (BufferMode (NoBuffering), Handle, IOMode (ReadMode), hClose, hFlush, hGetChar, hIsEOF, hReady, hSetBuffering, hSetEcho, openFile, stdin, stdout, withFile)
import Prelude hiding (takeWhile)

import Control.Monad (foldM, forM_, replicateM, replicateM_, when)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), Reader, ReaderT (runReaderT), runReader)
import Control.Monad.State (StateT, execStateT, get, modify)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.List (break, elem, intercalate, words)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T (Text, breakOn, drop, length, pack, unpack, unwords, words)
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

data FingerRace = FingerRace
  { sentence :: String
  , correct :: Int
  , strokes :: Int
  }

main :: IO ()
main = uiKeyinBooknames

uiKeyinBooknames = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  booknames <- listBooknames
  result <- foldr (\r (c, s) -> (c + correct r, s + strokes r)) (0, 0) <$> traverse gameFingerRace booknames

  putStrLn $ "Final rate: " <> show (fromIntegral (fst result) / fromIntegral (snd result))
 where
  acceptKeystrokes c = do
    k <- getChar

    if k == c
      then putChar k >> hFlush stdout
      else acceptKeystrokes c

uiVerseSearch = do
  term <- putStr "Term: " >> hFlush stdout >> getLine
  verses <- findVerses term
  let
    total = length verses
    top = take 5 verses
  when (total > 0) $ do
    putStrLn $ "Found: " <> show total
    forM_ top (\v -> putStrLn $ addr v <> " " <> T.unpack (text v))
    putStrLn $ show (total - 5) <> " more..."

gameFingerRace :: String -> IO FingerRace
gameFingerRace sentence = do
  putStrLn sentence
  result <- execStateT game (FingerRace sentence 0 0)

  putStrLn $ "\nRate: " <> show (fromIntegral (correct result) / fromIntegral (strokes result))

  return result
 where
  game :: StateT FingerRace IO FingerRace
  game = do
    pairs <- reverse <$> traverse acceptKeystrokes sentence
    let
      (c, s) = head pairs
    modify (\r -> r{correct = c, strokes = s})
    get

  acceptKeystrokes :: Char -> StateT FingerRace IO (Int, Int)
  acceptKeystrokes c = do
    k <- liftIO getChar
    if k == c
      then do
        liftIO (putChar k >> hFlush stdout)
        modify (\r@(FingerRace _ localCorrect localStrokes) -> r{correct = localCorrect + 1, strokes = localStrokes + 1})
        FingerRace _ updatedCorrect updatedStrokes <- get
        return (updatedCorrect, updatedStrokes)
      else do
        modify (\r@(FingerRace _ _ localStrokes) -> r{strokes = localStrokes + 1})
        acceptKeystrokes c

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

withDataHandle :: ReaderT Handle IO a -> IO a
withDataHandle reader = do
  hData <- openFile "data/kjv.txt" ReadMode
  replicateM_ 2 $ T.hGetLine hData
  x <- runReaderT reader hData
  hClose hData

  return x

-- Monadic actions

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

bookname :: Verse -> String
bookname (Verse address _) =
  let segments = init $ words address
   in unwords segments

parseVerse :: T.Text -> Verse
parseVerse line =
  let
    (addr, text) = T.breakOn "\t" line
   in
    Verse (T.unpack addr) (T.drop 1 text)
