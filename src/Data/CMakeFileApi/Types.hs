module Data.CMakeFileApi.Types(
    ParseResult(..),
    parseUsingResult,
    parseSelectValue
) where

import Control.Monad
import Control.Applicative

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parse, prependFailure)

data ParseResult a = ExternalError IOError
                   | ParseError String
                   | Retry String
                   | InvalidDirectory String
                   | Success a
                   deriving Show

propogateError :: ParseResult a -> ParseResult b
propogateError (ExternalError err)    = ExternalError err
propogateError (ParseError err)       = ParseError err
propogateError (Retry str)            = Retry str
propogateError (InvalidDirectory str) = InvalidDirectory str

instance Functor ParseResult where
    fmap transform (Success val) = Success (transform val)
    fmap _ err                   = propogateError err

instance Applicative ParseResult where
    pure = Success
    (<*>) (Success transform) = fmap transform
    (<*>) error               = \_ -> propogateError error

instance Monad ParseResult where
    return = pure
    fail = error
    (>>=) (Success val) transform  = transform val
    (>>=) error _                  = propogateError error

parseUsingResult :: ParseResult a -> (a -> IO (ParseResult b)) -> IO (ParseResult b)
parseUsingResult value transform = case value of
                                        Success val -> transform val
                                        error       -> return $ propogateError error

parseSelectValue :: [Aeson.Value -> Parser a] -> Aeson.Value -> Parser a
parseSelectValue xs val = foldl (<|>) empty $ map (\fun -> fun val) xs
