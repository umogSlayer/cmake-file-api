module Main where

import Data.CMakeFileApi
import System.Environment

main :: IO ()
main = do args <- getArgs
          parsedResult <- findAndParseIndexFile (head args)
          print parsedResult
