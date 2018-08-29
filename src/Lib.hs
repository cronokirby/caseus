module Lib
    ( someFunc
    )
where

import Control.Monad.IO.Class (liftIO)
import Pipes
import System.IO (IOMode(..), withFile, getLine, putStrLn)
import qualified Data.Text.IO as T

import CSV


someFunc :: IO ()
someFunc = do
    putStrLn "Enter a filename: "
    file <- getLine
    withFile file ReadMode $ \h -> runEffect $
        for (readLines h >-> firstLineJudge) (liftIO . T.putStrLn)