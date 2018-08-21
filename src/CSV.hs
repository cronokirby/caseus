{-|
This module contains functions related to parsing CSV files.
-}
module CSV
    ( splitLines
    , splitRow
    , rows
    )
where

import Control.Monad (forever, unless, when)
import Data.Char (isSpace)
import qualified Data.Text as T
import Pipes
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

-- | Splits a string into individual lines
splitLines :: Monad m => T.Text -> Producer T.Text m ()
splitLines txt = do
    let (this, rest) = splitWith '\n' txt
    yield this
    unless (T.null rest) (splitLines rest)


liftPipe :: Monad m => (a -> b) -> Pipe a b m ()
liftPipe f = forever (await >>= (yield . f))

-- | Produces rows from a text formatted in csv
rows :: Monad m => T.Text -> Producer RawRow m ()
rows t = splitLines t >-> liftPipe splitRow


-- | Returns indices of matching elements as they appear, starting from 1
noteMismatches :: Monad m => CSVSpec -> Pipe RawRow (Int, Mismatch) m ()
noteMismatches spec = go 1
  where
    go n = do
        x <- await
        let mismatch = findMismatch spec x
        unless (noMismatches mismatch) (yield (n, mismatch))
        go (n + 1)