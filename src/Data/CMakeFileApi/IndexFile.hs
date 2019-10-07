{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.CMakeFileApi.IndexFile (
    CMakeDescription(..),
    CMakePaths(..),
    CMakeVersion(..),
    ClientReplyValue(..),
    GeneratorDescription(..),
    IndexFile(..),
    KindVersion(..),
    ReplyError(..),
    ReplyFileReference(..),
    ReplyValue(..),
    StatefulResponse(..),
    StatefulResponseValue(..)
) where

import Data.Text (Text)
import qualified Data.Map.Strict as DMap
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (Parser, parse, prependFailure)

data GeneratorDescription = GeneratorDescription {
    name :: Text,
    platform :: Maybe Text
} deriving (Generic, Show)

instance FromJSON GeneratorDescription
instance ToJSON GeneratorDescription where
    toEncoding = genericToEncoding defaultOptions

data CMakePaths = CMakePaths {
    cmake :: Text,
    cpack :: Text,
    ctest :: Text,
    root :: Text
} deriving (Generic, Show)

instance FromJSON CMakePaths
instance ToJSON CMakePaths where
    toEncoding = genericToEncoding defaultOptions

data CMakeVersion = CMakeVersion {
    isDirty :: Bool,
    major :: Int,
    minor :: Int,
    patch :: Int,
    string :: Text,
    suffix :: Text
} deriving (Generic, Show)

instance FromJSON CMakeVersion
instance ToJSON CMakeVersion where
    toEncoding = genericToEncoding defaultOptions

data CMakeDescription = CMakeDescription {
    generator :: GeneratorDescription,
    paths :: CMakePaths,
    version :: CMakeVersion
} deriving (Generic, Show)

instance FromJSON CMakeDescription
instance ToJSON CMakeDescription where
    toEncoding = genericToEncoding defaultOptions

data KindVersion = KindVersion {
    kindMajor :: Int,
    kindMinor :: Int
} deriving Show

instance ToJSON KindVersion where
    toJSON  (KindVersion kindMajor kindMinor) =
        object ["major" .= kindMajor, "minor" .= kindMinor]

    toEncoding (KindVersion kindMajor kindMinor) =
        pairs ("major" .= kindMajor <> "minor" .= kindMinor)

instance FromJSON KindVersion where
    parseJSON = withObject "KindVersion" $ \v -> KindVersion
        <$> v .: "major"
        <*> v .: "minor"

data ReplyFileReference = ReplyFileReference {
    kind :: Text,
    replyVersion :: KindVersion,
    jsonFile :: Text
} deriving Show

instance ToJSON ReplyFileReference where
    toJSON  (ReplyFileReference kind replyVersion jsonFile) =
        object ["kind" .= kind, "version" .= replyVersion, "jsonFile" .= jsonFile]

    toEncoding (ReplyFileReference kind replyVersion jsonFile) =
        pairs ("kind" .= kind <> "version" .= replyVersion <> "jsonFile" .= jsonFile)

instance FromJSON ReplyFileReference where
    parseJSON = withObject "ReplyFileReference" $ \v -> ReplyFileReference
        <$> v .: "kind"
        <*> v .: "version"
        <*> v .: "jsonFile"

newtype ReplyError = ReplyError {
    error :: Text
} deriving (Show, Generic)

instance FromJSON ReplyError
instance ToJSON ReplyError where
    toEncoding = genericToEncoding defaultOptions

type ReplyKey = Text

data StatefulResponseValue = StatefulFileReference ReplyFileReference
                           | StatefulErrorValue ReplyError
                           deriving Show

instance ToJSON StatefulResponseValue where
    toJSON x = case x of
                    StatefulFileReference v -> toJSON v
                    StatefulErrorValue v -> toJSON v
    toEncoding x = case x of
                        StatefulFileReference v -> toEncoding v
                        StatefulErrorValue v -> toEncoding v


instance FromJSON StatefulResponseValue where
    parseJSON = parseSelectValue [parseStatefulErrorValueJSON,
                                  parseStatefulFileReferenceJSON]
                where parseStatefulFileReferenceJSON x = StatefulFileReference <$> parseJSON x
                      parseStatefulErrorValueJSON x = StatefulErrorValue <$> parseJSON x

data StatefulResponse = StatefulResponse {
    client :: Maybe Object,
    requests :: [Object],
    responses :: [StatefulResponseValue]
} deriving (Show, Generic)

instance FromJSON StatefulResponse
instance ToJSON StatefulResponse where
    toEncoding = genericToEncoding defaultOptions

data ClientReplyValue = ClientStatelessResponse ReplyFileReference
                      | ClientStatefulResponse StatefulResponse
                      | ClientErrorValue ReplyError
                      deriving Show

instance ToJSON ClientReplyValue where
    toJSON x = case x of
                    ClientStatelessResponse v -> toJSON v
                    ClientStatefulResponse v -> toJSON v
                    ClientErrorValue v -> toJSON v
    toEncoding x = case x of
                        ClientStatelessResponse v -> toEncoding v
                        ClientStatefulResponse v -> toEncoding v
                        ClientErrorValue v -> toEncoding v

instance FromJSON ClientReplyValue where
    parseJSON = parseSelectValue [parseClientErrorValueJSON,
                                  parseClientStatelessResponseJSON,
                                  parseClientStatefulResponseJSON]
                where parseClientStatelessResponseJSON x = ClientStatelessResponse <$> parseJSON x
                      parseClientStatefulResponseJSON x = ClientStatefulResponse <$> parseJSON x
                      parseClientErrorValueJSON x = ClientErrorValue <$> parseJSON x

data ReplyValue = SharedStatelessResponseValue ReplyFileReference
                | ClientResponseValue (DMap.Map ReplyKey ClientReplyValue)
                | ErrorValue ReplyError
                deriving Show

instance ToJSON ReplyValue where
    toJSON x = case x of
                    SharedStatelessResponseValue v -> toJSON v
                    ClientResponseValue v -> toJSON v
                    ErrorValue v -> toJSON v
    toEncoding x = case x of
                        SharedStatelessResponseValue v -> toEncoding v
                        ClientResponseValue v -> toEncoding v
                        ErrorValue v -> toEncoding v

instance FromJSON ReplyValue where
    parseJSON = parseSelectValue [parseErrorValueJSON,
                                  parseSharedStatelessResponseValueJSON,
                                  parseClientStatelessResponseValueJSON]
                where parseSharedStatelessResponseValueJSON x = SharedStatelessResponseValue <$> parseJSON x
                      parseClientStatelessResponseValueJSON x = ClientResponseValue <$> parseJSON x
                      parseErrorValueJSON x = ErrorValue <$> parseJSON x

data IndexFile = IndexFile {
    cmakeDesc :: CMakeDescription,
    objects :: [ReplyFileReference],
    reply :: DMap.Map ReplyKey ReplyValue
} deriving Show

instance ToJSON IndexFile where
    toJSON (IndexFile cmakeDesc objects reply) =
        object ["cmake" .= cmakeDesc, "objects" .= objects, "reply" .= reply]

    toEncoding (IndexFile cmakeDesc objects reply) =
        pairs ("cmake" .= cmakeDesc <> "objects" .= objects <> "reply" .= reply)

instance FromJSON IndexFile where
    parseJSON = withObject "IndexFile" $ \v -> IndexFile
        <$> v .: "cmake"
        <*> v .: "objects"
        <*> v .: "reply"

parseSelectValuePair :: (Value -> Parser a) -> (Value -> Parser a) -> Value -> Parser a
parseSelectValuePair x y v = case parse x v of
                                  Success z -> return z
                                  Error str -> prependFailure ("[" ++ str ++ "]" ++ " -> ") $ y v

parseSelectValue :: [Value -> Parser a] -> Value -> Parser a
parseSelectValue (x:y:xs) = foldl parseSelectValuePair (parseSelectValuePair x y) xs
parseSelectValue [x] = x
parseSelectValue _ = fail "No types specified"
