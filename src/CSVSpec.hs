{-# LANGUAGE OverloadedStrings #-}
{-|
This module contains the types and functions related to checking CSV specs.
-}
module CSVSpec
    ( RawRow (..)
    , CSVType(..)
    , CSVSpec(..)
    , MismatchedColumn(..)
    , Mismatch(..)
    , typeText
    , findMismatch
    , noMismatches
    , rawSpec
    , showMismatchedRow
    )
where

import Data.Char (isDigit)
import Data.Functor (($>))
import Data.List (intersperse, zipWith3)
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import Control.Monad (guard)
import qualified Data.Text as T


-- | Represents the different types a CSV column can have
data CSVType
    = CSVInteger -- ^ An Integer litteral
    | CSVString -- ^ A String litteral
    deriving (Eq, Show)

-- | Type a single column into a CSVType
typeText :: T.Text -> CSVType
typeText t
    | T.all isDigit t = CSVInteger
    | otherwise       = CSVString


-- | Represents a raw CSV Row
data RawRow = RawRow [T.Text] deriving (Eq, Show)

-- | A specification for each row of a CSV
-- Each element of the list represent what type of thing each column should be
data CSVSpec = CSVSpec [CSVType] deriving (Eq, Show)

-- | Represents a single mismatched column compared to a spec
data MismatchedColumn
    = MismatchedColumn
    { index :: Int -- ^ the index of the mismatching column
    , expected :: CSVType -- ^ the type of element that should be there
    , mismatch :: CSVType -- ^ the type of element that is there
    }
    deriving (Eq, Show)

-- | Represents how a certain row mismatches the specification
newtype Mismatch = Mismatch [MismatchedColumn] deriving (Eq, Show)


rawSpec :: RawRow -> CSVSpec
rawSpec (RawRow row) = CSVSpec (map typeText row)

-- | Looks at a RawRow and reports mismatches according to a spec
findMismatch :: CSVSpec -> RawRow -> Mismatch
findMismatch (CSVSpec types) (RawRow row) =
    let rawTypes = map typeText row
        makeMisMatch i t1 t2 = guard (t1 /= t2) $> MismatchedColumn i t1 t2
    in Mismatch . catMaybes $ zipWith3 makeMisMatch [1..] types rawTypes


noMismatches :: Mismatch -> Bool
noMismatches (Mismatch l) = null l

-- note: maybe make a prelude to this
showText :: Show s => s -> T.Text
showText = T.pack . show

-- | Pretty prints a csv type
showCSVType :: CSVType -> T.Text
showCSVType CSVInteger = "Integer"
showCSVType CSVString = "String"

showMismatch :: Mismatch -> T.Text
showMismatch (Mismatch cols) = mconcat . intersperse " | " $ map foo cols
  where
    foo :: MismatchedColumn -> T.Text
    foo (MismatchedColumn i e m) =
        showText i <> ": expected " <> showCSVType e <> ", got " <> showCSVType m

showMismatchedRow :: (Int, Mismatch) -> T.Text
showMismatchedRow (i, m) = "Row " <> showText i <> "\n" <> showMismatch m