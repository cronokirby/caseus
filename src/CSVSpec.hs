{-|
This module contains the types and functions related to checking CSV specs.
-}
module CSVSpec
    ( RawRow (..)
    , CSVSpec(..)
    , MismatchedColumn(..)
    , Mismatch
    , findMismatch
    , noMismatches
    )
where

import Data.Char (isDigit)
import Data.Functor (($>))
import Data.List (zipWith3)
import Data.Maybe (catMaybes)
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
    deriving (Show)

-- | Represents how a certain row mismatches the specification
newtype Mismatch = Mismatch [MismatchedColumn] deriving (Show)


findMismatch :: CSVSpec -> RawRow -> Mismatch
findMismatch (CSVSpec types) (RawRow row) =
    let rawTypes = map typeText row
        makeMisMatch i t1 t2 = guard (t1 == t2) $> MismatchedColumn i t1 t2
    in Mismatch . catMaybes $ zipWith3 makeMisMatch [1..] types rawTypes


noMismatches :: Mismatch -> Bool
noMismatches (Mismatch l) = null l