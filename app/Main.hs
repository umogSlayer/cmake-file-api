module Main where

import Data.CMakeFileApi
import System.Environment

main :: IO ()
main = do args <- getArgs
          parsedResult <- analyzeCMakeOutput (head args)
          print parsedResult
