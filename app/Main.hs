module Main where

import Data.CMakeFileApi

main :: IO ()
main = do parsedResult <- findAndParseIndexFile "/home/umogslayer/source/autocomplete/ias64d-gcc/.cmake/api/v1/reply"
          print parsedResult
