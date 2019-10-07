{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.CMakeFileApi.CodeModel (
    CodeModel(..),
    Configuration(..),
    Paths(..),
    Target(..)
) where

import Data.Text (Text)
import qualified Data.Map.Strict as DMap
import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (Parser, parse, prependFailure)

data Target = Target {
    targetName :: Text,
    targetId :: Int,
    directoryIndex :: Int,
    projectIndex :: Int,
    jsonFile :: Text
} deriving Show

instance ToJSON Target where
    toJSON target =
        object ["name" .= targetName target,
                "id" .= targetId target,
                "directoryIndex" .= directoryIndex target,
                "projectIndex" .= projectIndex target,
                "jsonFile" .= jsonFile target]

    toEncoding target =
        pairs ("name" .= targetName target
               <> "id" .= targetId target
               <> "directoryIndex" .= directoryIndex target
               <> "projectIndex" .= projectIndex target
               <> "jsonFile" .= jsonFile target)

instance FromJSON Target where
    parseJSON = withObject "Target" $ \v -> Target
        <$> v .: "name"
        <*> v .: "id"
        <*> v .: "directoryIndex"
        <*> v .: "projectIndex"
        <*> v .: "jsonFile"

data Configuration = Configuration {
    name :: Text,
    directories :: [Object],
    projects :: [Object],
    targets :: [Target]
} deriving (Show, Generic)

instance FromJSON Configuration
instance ToJSON Configuration where
    toEncoding = genericToEncoding defaultOptions

data Paths = Paths {
    source :: Text,
    build :: Text
} deriving (Show, Generic)

instance FromJSON Paths
instance ToJSON Paths where
    toEncoding = genericToEncoding defaultOptions

data CodeModel = CodeModel {
    paths :: Paths,
    configurations :: [Configuration]
} deriving (Show, Generic)

instance FromJSON CodeModel
instance ToJSON CodeModel where
    toEncoding = genericToEncoding defaultOptions

parseSelectValuePair :: (Value -> Parser a) -> (Value -> Parser a) -> Value -> Parser a
parseSelectValuePair x y v = case parse x v of
                                  Success z -> return z
                                  Error str -> prependFailure ("[" ++ str ++ "]" ++ " -> ") $ y v

parseSelectValue :: [Value -> Parser a] -> Value -> Parser a
parseSelectValue (x:y:xs) = foldl parseSelectValuePair (parseSelectValuePair x y) xs
parseSelectValue [x] = x
parseSelectValue _ = fail "No types specified"

