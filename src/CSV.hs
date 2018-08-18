{-|
This module contains functions related to parsing CSV files.
-}
module CSV
    ( splitLines
    )
where

import qualified Data.Text as T
import Pipes
import qualified Pipes.Prelude as P


-- | splits a string into individual lines
splitLines :: Monad m => T.Text -> Producer T.Text m ()
splitLines txt = do
    let (this, that) = T.break (== '\n') txt
        rest = T.drop 1 that -- removing the newline
    yield this
    if T.null rest
        then return ()
        else splitLines rest