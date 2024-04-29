{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (BufferMode (NoBuffering), Handle, IOMode (ReadMode), hFlush, hSetBuffering, hSetEcho, stdin, stdout)

import Control.Monad (when)
import Control.Monad.Reader (MonadIO (liftIO))
import Control.Monad.State (StateT, execStateT, get, modify)
import Data.Foldable (traverse_, forM_)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T (Text, unpack)

import Data.Bible
import Control.Bible.Monads

data FingerRace = FingerRace
  { sentence :: String
  , correct :: Int
  , strokes :: Int
  }

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering

  putStrLn "1. Book names\n2. Verses in capital"
  chosenGame <- putStr "Choose game: " >> getLine
  case chosenGame of
    "1" -> uiKeyinBooknames
    "2" -> uiKeyinVerse
    _ -> putStrLn "Invalid choice"
  main

uiKeyinBooknames = do
  putStrLn "Enter name of the books in the Bible in correct order"
  hSetEcho stdin False
  booknames <- listBooknames
  result <- summarize <$> traverse gameFingerRace booknames

  putStrLn $ "Final rate: " <> show result
  hSetEcho stdin True

uiKeyinVerse = do
  booknames <- listBooknames
  traverse_ putStrLn booknames
  mbBook <- putStr "Choose a book: " >> hFlush stdout >> getLine >>= getBook
  when (isJust mbBook) $ do
    let book = fromJust mbBook
    traverse_ (\v -> putStr $ verseNo v <> " ") (verses book)
    putStrLn ""
    chosenChapter <- putStr "Choose a chapter: " >> getLine

    let
      selectedVerses = filter (\v -> chapterNo v == chosenChapter) (verses book)
      sentences = map (T.unpack . text) selectedVerses
    hSetEcho stdin False
    result <- summarize <$> traverse (\sentence -> putStrLn sentence >> gameFingerRace sentence) sentences
    putStrLn $ "Final rate: " <> show result
    hSetEcho stdin True

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

summarize rows =
  let (countCorrect, countStrokes) = foldr (\r (c, s) -> (c + correct r, s + strokes r)) (0, 0) rows
   in fromIntegral countCorrect / fromIntegral countStrokes

gameFingerRace :: String -> IO FingerRace
gameFingerRace sentence = do
  execStateT game (FingerRace sentence 0 0)
 where
  game :: StateT FingerRace IO FingerRace
  game = do
    pairs <- reverse <$> traverse acceptKeystrokes sentence
    liftIO $ putStrLn ""
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
