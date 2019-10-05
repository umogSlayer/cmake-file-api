{-# LANGUAGE OverloadedStrings #-}

module Data.CMakeFileApi (
    findAndParseIndexFile,
    parseIndexFileContents,
    parseIndexFile
) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BSL
import System.Directory (listDirectory)

import Data.CMakeFileApi.IndexFile (IndexFile)
import Data.CMakeFileApi.Types

import Control.Exception
import Control.Monad

import System.IO.Error


parseIndexFileContents :: BSL.ByteString -> ParseResult IndexFile
parseIndexFileContents contents =
    let parseResult = Aeson.eitherDecode contents :: Either String IndexFile
    in case parseResult of
            Left str -> ParseError str
            Right value -> Success value


parseIndexFile :: FilePath -> IO (ParseResult IndexFile)
parseIndexFile indexFile =
    do indexFileContents <- tryJust (guard . isDoesNotExistError) (BSL.readFile indexFile)
       case indexFileContents of
            Left _ -> return Retry
            Right contents -> return $ parseIndexFileContents contents

findAndParseIndexFile :: FilePath -> IO (ParseResult IndexFile)
findAndParseIndexFile folder =
    let getIndexFiles = filter isIndexFile . map Text.pack
        hasIndexPrefix = (== indexFilePrefix) . Text.take (Text.length indexFilePrefix)
        hasIndexSuffix = (== indexFileSuffix) . Text.takeEnd (Text.length indexFileSuffix)
        isIndexFile fileName = hasIndexPrefix fileName && hasIndexSuffix fileName
        indexFilePrefix = "index-" :: Text.Text
        indexFileSuffix = ".json" :: Text.Text
    in
        do directoryContents <- listDirectory folder
           let indexFiles = getIndexFiles directoryContents in
                   if null indexFiles
                       then return InvalidDirectory
                       else parseIndexFile (folder ++ "/" ++ (Text.unpack . maximum $ indexFiles))
