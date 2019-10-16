module Data.CMakeFileApi.Types(
    MonadReadFile(..),
    MonadWriteFile(..),
    MonadCreateDirectory(..),
    MonadReadDirectory(..),
    MonadFileOperations,
    ParseResult(..),
    parseUsingResult,
    parseSelectValue
) where

import System.IO
import System.IO.Error

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Applicative

import System.Directory

import qualified Data.ByteString.Lazy as BSL

import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, parse, prependFailure)

data ParseResult a = ParseError String
                   | Retry String
                   | InvalidDirectory String
                   | Success a
                   deriving Show

propogateError :: ParseResult a -> ParseResult b
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

parseSelectValue :: [Aeson.Value -> Parser a] -> Aeson.Value -> Parser a
parseSelectValue xs val = foldl (<|>) empty $ map (\fun -> fun val) xs

parseUsingResult :: MonadReadFile m => ParseResult a -> (a -> m (ParseResult b)) -> m (ParseResult b)
parseUsingResult value transform = case value of
                                        Success val -> transform val
                                        error       -> return $ propogateError error

class Monad m => MonadReadFile m where
    readFileFunc :: FilePath -> m BSL.ByteString
    readFileFunc' :: FilePath -> m BSL.ByteString
    fileDoesNotExist :: m a -> m (ParseResult a)

class Monad m => MonadWriteFile m where
    writeFileFunc :: FilePath -> BSL.ByteString -> m ()

class Monad m => MonadReadDirectory m where
    listDirectoryFunc :: FilePath -> m [FilePath]
    doesFileExistFunc :: FilePath -> m Bool

class Monad m => MonadCreateDirectory m where
    createDirectoryIfMissingFunc :: FilePath -> m ()

class (MonadReadFile m, MonadWriteFile m, MonadCreateDirectory m, MonadReadDirectory m) => MonadFileOperations m

instance MonadReadFile IO where
    readFileFunc = BSL.readFile
    readFileFunc' filePath = do contents <- readFileFunc filePath
                                evaluate (force contents)
                                return contents
    fileDoesNotExist input = do result <- tryJust (\e -> if isDoesNotExistError e then Just e else Nothing) input
                                case result of
                                     Left err -> return $ Retry (show err)
                                     Right value -> return $ Success value

instance MonadWriteFile IO where
    writeFileFunc filePath byteString = withFile filePath WriteMode $ \fileHandle ->
        BSL.hPut fileHandle byteString

instance MonadReadDirectory IO where
    listDirectoryFunc = listDirectory
    doesFileExistFunc = doesFileExist

instance MonadCreateDirectory IO where
    createDirectoryIfMissingFunc = createDirectoryIfMissing True

instance MonadFileOperations IO
