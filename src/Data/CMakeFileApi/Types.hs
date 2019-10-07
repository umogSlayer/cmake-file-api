module Data.CMakeFileApi.Types (
    ParseResult(..)
) where

import Control.Monad

data ParseResult a = ExternalError IOError
                   | ParseError String
                   | Retry String
                   | InvalidDirectory String
                   | Success a
                   deriving Show

instance Functor ParseResult where
    fmap transform (Success val)  = Success (transform val)
    fmap _ (ExternalError err)    = ExternalError err
    fmap _ (ParseError err)       = ParseError err
    fmap _ (Retry str)            = Retry str
    fmap _ (InvalidDirectory str) = InvalidDirectory str

instance Applicative ParseResult where
    pure = Success
    (<*>) (Success transform) (Success value) = Success (transform value)
    (<*>) (ExternalError err) _               = ExternalError err
    (<*>) (ParseError err) _                  = ParseError err
    (<*>) (Retry str) _                       = Retry str
    (<*>) (InvalidDirectory str) _            = InvalidDirectory str

instance Monad ParseResult where
    return = pure
    fail = error
    (>>=) (Success val) transform  = transform val
    (>>=) (ExternalError err) _    = ExternalError err
    (>>=) (ParseError err) _       = ParseError err
    (>>=) (Retry str) _            = Retry str
    (>>=) (InvalidDirectory str) _ = InvalidDirectory str
