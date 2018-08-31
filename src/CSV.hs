{-|
This module contains functions related to parsing CSV files.
-}
module CSV
    ( readLines
    , firstLineJudge
    , showMismatches
    )
where

import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isSpace)
import Pipes
import System.IO (FilePath, Handle, hIsEOF)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Pipes.Prelude as P

import CSVSpec


-- | Splits a string into 2 parts on the first char it finds, skipping the char
splitWith :: Char -> T.Text -> (T.Text, T.Text)
splitWith c txt =
    let (this, that) = T.break (== c) txt
        rest = T.drop 1 that
    in (this, rest)

-- | Parses a bit of text by commas into a row
splitRow :: T.Text -> RawRow
splitRow = RawRow . reverse . map removeWhitespace . go []
  where
    removeWhitespace :: T.Text -> T.Text
    removeWhitespace = T.takeWhile (not . isSpace) . T.dropWhile isSpace
    go :: [T.Text] -> T.Text -> [T.Text]
    go acc txt
      | T.null txt = acc
      | otherwise  =
        let (this, rest) = splitWith ',' txt
        in go (this : acc) rest


liftPipe :: Monad m => (a -> b) -> Pipe a b m ()
liftPipe f = forever (await >>= (yield . f))


-- | Returns indices of mismatching elements as they appear, starting from 1
noteMismatches :: Monad m => CSVSpec -> Pipe RawRow (Int, Mismatch) m ()
noteMismatches spec = go 1
  where
    go n = do
        x <- await
        let mismatch = findMismatch spec x
        unless (noMismatches mismatch) (yield (n, mismatch))
        go (n + 1)


-- | Returns lines from a file
readLines :: Handle -> Producer T.Text IO ()
readLines h = do
    isEOF <- liftIO (hIsEOF h)
    unless isEOF $ do
        line <- liftIO (T.hGetLine h)
        yield line
        readLines h
    return ()


-- | Uses the first line of a csv file as a template for which to judge the rest
-- Receives in lines of text from upstream, assuming the first one is to be used as the template
-- produces commentary lines, ready to be printed.
firstLineJudge :: Monad m => Pipe T.Text (Int, Mismatch) m ()
firstLineJudge = liftPipe splitRow >-> judge
  where
    judge = do
        firstRaw <- await
        let spec = rawSpec firstRaw
        noteMismatches spec

-- | Takes in a row and a mismatch, and produces a text commentary
showMismatches :: Monad m => Pipe (Int, Mismatch) T.Text m ()
showMismatches = liftPipe showMismatchedRow