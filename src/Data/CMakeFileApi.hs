{-# LANGUAGE OverloadedStrings #-}

module Data.CMakeFileApi (
    findAndParseIndexFile,
    parseFileContents,
    parseFile
) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as DMap
import System.Directory (listDirectory)

import Data.CMakeFileApi.IndexFile
import Data.CMakeFileApi.Types
import Data.CMakeFileApi.CodeModel

import Control.Exception
import Control.Monad

import System.IO.Error


parseFileContents :: Aeson.FromJSON a => BSL.ByteString -> ParseResult a
parseFileContents contents =
    let parseResult = Aeson.eitherDecode contents
    in case parseResult of
            Left str -> ParseError str
            Right value -> Success value


parseFile :: Aeson.FromJSON a => FilePath -> IO (ParseResult a)
parseFile file =
    do indexFileContents <- tryJust (guard . isDoesNotExistError) (BSL.readFile file)
       case indexFileContents of
            Left err -> return $ Retry $ "File " ++ file ++ " does not exist"
            Right contents -> return $ parseFileContents contents

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
                       then return $ InvalidDirectory ("No index file inside " ++ folder)
                       else parseFile (folder ++ "/" ++ (Text.unpack . maximum $ indexFiles))

findSharedCodeModel :: IndexFile -> Maybe ReplyFileReference
findSharedCodeModel indexFile =
    let codeModelObject = DMap.lookup "codemodel-v2" . reply
    in do SharedStatelessResponseValue replyFileReference <- codeModelObject indexFile
          return replyFileReference

findHcmakeStatelessCodeModel :: IndexFile -> Maybe ReplyFileReference
findHcmakeStatelessCodeModel indexFile =
    let clientResponse = DMap.lookup "client-hcmake" . reply
        clientStatelessResponse = DMap.lookup "codemodel-v2"
    in do ClientResponseValue clientResponseMap <- clientResponse indexFile
          ClientStatelessResponse replyFileReference <- clientStatelessResponse clientResponseMap
          return replyFileReference

findHcmakeStatefulCodeModel :: IndexFile -> Maybe ReplyFileReference
findHcmakeStatefulCodeModel indexFile =
    let clientResponse = DMap.lookup "client-hcmake" . reply
        clientStatefulResponse = DMap.lookup "query.json"
        clientStatefulResponses list = [replyFileReference | StatefulFileReference replyFileReference <- list,
                                                             kind replyFileReference == "codemodel",
                                                             (kindMajor . replyVersion) replyFileReference == 2]
    in do ClientResponseValue clientResponseMap <- clientResponse indexFile
          ClientStatefulResponse clientStatefulResponse <- clientStatefulResponse clientResponseMap
          case clientStatefulResponses . responses $ clientStatefulResponse of
               [replyFileReference] -> return replyFileReference
               _ -> Nothing


--parseCodeModel :: IndexFile -> IO (ParseResult CodeModel)
--parseCodeModel indexFile = 
