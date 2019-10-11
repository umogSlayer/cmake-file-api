{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.CMakeFileApi.CodeModelTarget (
    CodeModelTarget(..),
) where

import Data.Text (Text)
import qualified Data.Map.Strict as DMap
import qualified Data.Primitive.Array as Array
import qualified Data.Primitive.PrimArray as PrimArray
import GHC.Generics (Generic)

import Data.Aeson

import Data.CMakeFileApi.CodeModel (Paths(..))

newtype Path = Path {
    path :: Text
} deriving (Show, Generic)

instance FromJSON Path
instance ToJSON Path where
    toEncoding = genericToEncoding defaultOptions

type CompileLanguage = Text

data SourceDescription = SourceDescription {
    sourcePath :: Text,
    compileGroupIndex :: Maybe Int,
    sourceGroupIndex :: Maybe Int,
    isGenerated :: Bool,
    backtrace :: Maybe Int
} deriving Show

instance ToJSON SourceDescription where
    toJSON target =
        object ["path" .= sourcePath target,
                "compileGroupIndex" .= compileGroupIndex target,
                "sourceGroupIndex" .= sourceGroupIndex target,
                "isGenerated" .= isGenerated target,
                "backtrace" .= backtrace target]

    toEncoding target =
        pairs ("path" .= sourcePath target
               <> "compileGroupIndex" .= compileGroupIndex target
               <> "sourceGroupIndex" .= sourceGroupIndex target
               <> "isGenerated" .= isGenerated target
               <> "backtrace" .= backtrace target)

instance FromJSON SourceDescription where
    parseJSON = withObject "SourceDescription" $ \v -> SourceDescription
        <$> v .: "path"
        <*> v .:? "compileGroupIndex"
        <*> v .:? "sourceGroupIndex"
        <*> v .:? "isGenerated" .!= False
        <*> v .:? "backtrace"

data SourceGroupDescription = SourceGroupDescription {
    sourceGroupName :: Text,
    groupSourceIndices :: PrimArray.PrimArray Int
} deriving Show

instance ToJSON SourceGroupDescription where
    toJSON target =
        object ["name" .= sourceGroupName target,
                "sourceIndexes" .= groupSourceIndices target]

    toEncoding target =
        pairs ("name" .= sourceGroupName target
               <> "sourceIndexes" .= groupSourceIndices target)

instance FromJSON SourceGroupDescription where
    parseJSON = withObject "SourceGroupDescription" $ \v -> SourceGroupDescription
        <$> v .: "name"
        <*> v .: "sourceIndexes"

data CompileGroupDescription = CompileGroupDescription {
    compileSourceIndices :: PrimArray.PrimArray Int,
    compileLanguage :: CompileLanguage
} deriving Show

instance ToJSON CompileGroupDescription where
    toJSON target =
        object ["sourceIndexes" .= compileSourceIndices target,
                "language" .= compileLanguage target]

    toEncoding target =
        pairs ("sourceIndexes" .= compileSourceIndices target
               <> "language" .= compileLanguage target)

instance FromJSON CompileGroupDescription where
    parseJSON = withObject "CompileGroupDescription" $ \v -> CompileGroupDescription
        <$> v .: "sourceIndexes"
        <*> v .: "language"

data CodeModelTarget = CodeModelTarget {
    name :: Text,
    id :: Text,
    folder :: Maybe Object,
    paths :: Paths,
    nameOnDisk :: Maybe Text,
    artifacts :: [Path],
    isGeneratorProvided :: Bool,
    sources :: Array.Array SourceDescription,
    sourceGroups :: Array.Array SourceGroupDescription,
    compileGroups :: Array.Array CompileGroupDescription,
    backtraceGraph :: Maybe Object
} deriving (Show, Generic)

instance FromJSON CodeModelTarget where
    parseJSON = withObject "CodeModelTarget" $ \v -> CodeModelTarget
                <$> v .:  "name"
                <*> v .:  "id"
                <*> v .:? "folder"
                <*> v .:  "paths"
                <*> v .:? "nameOnDisk"
                <*> v .:? "artifacts" .!= []
                <*> v .:? "isGeneratorProvided" .!= False
                <*> v .:? "sources" .!= Array.fromListN 0 []
                <*> v .:? "sourceGroups" .!= Array.fromListN 0 []
                <*> v .:? "compileGroups" .!= Array.fromListN 0 []
                <*> v .:? "backtraceGraph"

instance ToJSON CodeModelTarget where
    toEncoding = genericToEncoding defaultOptions
