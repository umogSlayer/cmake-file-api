module Data.CMakeFileApi.Types (
    ParseResult(..)
) where

data ParseResult a = ExternalError IOError
                   | ParseError String
                   | Retry
                   | InvalidDirectory
                   | Success a
                   deriving Show
