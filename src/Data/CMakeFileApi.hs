{-# LANGUAGE OverloadedStrings #-}

module Data.CMakeFileApi (
    ConfigurationCMakeOutput(..),
    CodeModelCMakeOutput(..),
    analyzeCMakeOutput,
    analyzeCMakeOutputIO,
    putCMakeQuery,
    putCMakeQueryIO
) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as DMap
import qualified Data.Primitive.Array as Array
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Either

import Data.CMakeFileApi.IndexFile
import qualified Data.CMakeFileApi.IndexFile as IndexFile
import Data.CMakeFileApi.CodeModel
import qualified Data.CMakeFileApi.CodeModel as CodeModel
import Data.CMakeFileApi.CodeModelTarget
import qualified Data.CMakeFileApi.CodeModelTarget as CodeModelTarget
import Data.CMakeFileApi.Types

import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Exception
import Control.Monad

import System.FilePath

parseFileContents :: Aeson.FromJSON a => BSL.ByteString -> ParseResult a
parseFileContents contents =
    let parseResult = Aeson.eitherDecode contents
    in case parseResult of
            Left str -> ParseError str
            Right value -> Success value


parseFile :: (Aeson.FromJSON a, MonadReadFile m) => FilePath -> m (ParseResult a)
parseFile file =
    do indexFileContents <- fileDoesNotExist (readFileFunc' file)
       return $ indexFileContents >>= parseFileContents

findAndParseIndexFile :: (MonadReadDirectory m, MonadReadFile m) => FilePath -> m (ParseResult IndexFile)
findAndParseIndexFile folder =
    let getIndexFiles = filter isIndexFile . map Text.pack
        hasIndexPrefix = (== indexFilePrefix) . Text.take (Text.length indexFilePrefix)
        hasIndexSuffix = (== indexFileSuffix) . Text.takeEnd (Text.length indexFileSuffix)
        isIndexFile fileName = hasIndexPrefix fileName && hasIndexSuffix fileName
        indexFilePrefix = "index-" :: Text.Text
        indexFileSuffix = ".json" :: Text.Text
    in
        do directoryContents <- listDirectoryFunc folder
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

parseCodeModel :: MonadReadFile m => FilePath -> IndexFile -> m (ParseResult CodeModel)
parseCodeModel folder indexFile =
    let sharedCodeModel = findSharedCodeModel indexFile
        clientStatelessCodeModel = findHcmakeStatelessCodeModel indexFile
        clientStatefulCodeModel = findHcmakeStatefulCodeModel indexFile
        selectedCodeModelMaybe = clientStatefulCodeModel <|> clientStatelessCodeModel <|> sharedCodeModel
        parseCodeModelFile :: MonadReadFile m => ReplyFileReference -> m (ParseResult CodeModel)
        parseCodeModelFile = parseFile . (\v -> folder ++ "/" ++ v) . Text.unpack . IndexFile.jsonFile
    in case selectedCodeModelMaybe of
            Just replyFileRef -> parseCodeModelFile replyFileRef
            Nothing -> return $ Retry "Reference file not found"

type ConfigurationWithTargets = (Configuration, [CodeModelTarget])

parseTargets :: MonadReadFile m => FilePath -> CodeModel -> m (ParseResult [ConfigurationWithTargets])
parseTargets folder codeModel =
    let configurationsList :: [(Configuration, [CodeModel.Target])]
        configurationsList = [(configuration, CodeModel.targets configuration)
                          | configuration <- CodeModel.configurations codeModel]
        parseTargetFile :: MonadReadFile m => CodeModel.Target -> m (ParseResult CodeModelTarget)
        parseTargetFile = parseFile . (\v -> folder ++ "/" ++ v) . Text.unpack . CodeModel.jsonFile
        parseFileSequence :: MonadReadFile m => [CodeModel.Target] -> m [ParseResult CodeModelTarget]
        parseFileSequence = traverse parseTargetFile
        parseConfiguration :: MonadReadFile m => (Configuration, [CodeModel.Target]) -> m (ParseResult (Configuration, [CodeModelTarget]))
        parseConfiguration (config, targets) = do codeModelTargets <- parseFileSequence targets
                                                  return $ (\vm -> do v <- vm; return (config, v)) $ sequence codeModelTargets
        sequencedConfigurations :: MonadReadFile m => m [ParseResult ConfigurationWithTargets]
        sequencedConfigurations = sequence [parseConfiguration configuration | configuration <- configurationsList]
    in sequence <$> sequencedConfigurations

data ConfigurationCMakeOutput = ConfigurationCMakeOutput {
    name :: Text.Text,
    description :: CodeModel.Configuration,
    targets :: Array.Array CodeModelTarget
} deriving Show

data CodeModelCMakeOutput = CodeModelCMakeOutput {
    codeModel :: CodeModel,
    configurations :: Array.Array ConfigurationCMakeOutput
} deriving Show

fromConfigurationWithTargets :: ConfigurationWithTargets -> ConfigurationCMakeOutput
fromConfigurationWithTargets (configuration, targets) =
    ConfigurationCMakeOutput {
        Data.CMakeFileApi.name        = CodeModel.name configuration,
        Data.CMakeFileApi.description = configuration,
        Data.CMakeFileApi.targets     = Array.fromList targets
    }

analyzeCMakeOutput :: (MonadReadDirectory m, MonadReadFile m) => FilePath -> m (ParseResult CodeModelCMakeOutput)
analyzeCMakeOutput buildDirectory = let directory = buildDirectory ++ "/.cmake/api/v1/reply"
    in do indexFileMonad <- findAndParseIndexFile directory
          codeModelMonad <- parseUsingResult indexFileMonad $ parseCodeModel directory
          targetsMonad <- parseUsingResult codeModelMonad $ parseTargets directory
          return $ do codeModel <- codeModelMonad
                      targets <- targetsMonad
                      return CodeModelCMakeOutput {
                          Data.CMakeFileApi.codeModel = codeModel,
                          Data.CMakeFileApi.configurations = Array.fromList [fromConfigurationWithTargets config | config <- targets]
                      }

requestObject :: Aeson.Value
requestObject = Aeson.Object $ HashMap.fromList [("kind", Aeson.String "codemodel"),
                                                 ("version", Aeson.Object versionObject)]
                where versionObject = HashMap.fromList [("major", Aeson.Number 2)]

requestQuery :: Aeson.Object
requestQuery = HashMap.fromList [("requests", Aeson.Array $ Vector.fromList [requestObject])]

updateClientStatefulQueryValue :: Aeson.Value -> Aeson.Value
updateClientStatefulQueryValue currentValue =
    case currentValue of
         Aeson.Object obj -> Aeson.Object $ modifyObject obj
         _ -> Aeson.Object requestQuery
    where modifyObject currentValue = HashMap.unionWith (const Prelude.id) currentValue requestQuery

updateClientStatefulQuery :: (MonadWriteFile m, MonadReadFile m) => FilePath -> m ()
updateClientStatefulQuery filePath =
    do jsonContent <- readFileFunc filePath
       let decodedObject = fromRight (Aeson.Object (HashMap.fromList [])) . Aeson.eitherDecode $ jsonContent
           in decodedObject `deepseq` (writeFileFunc filePath . Aeson.encode) $ updateClientStatefulQueryValue decodedObject

createClientStatefulQuery :: (MonadCreateDirectory m, MonadWriteFile m, MonadReadFile m) => FilePath -> m ()
createClientStatefulQuery filePath =
    do createDirectoryIfMissingFunc directory
       writeFileFunc filePath ""
       updateClientStatefulQuery filePath
    where directory = dropFileName filePath

putCMakeQuery :: MonadFileOperations m => FilePath -> m ()
putCMakeQuery buildDirectory = let requestDirectory = buildDirectory ++ "/.cmake/api/v1/query"
                                   clientRequestDirectory = requestDirectory ++ "/client-hcmake"
                                   sharedStatelessQueryFile = requestDirectory ++ "/codemodel-v2"
                                   clientStatelessQueryFile = clientRequestDirectory ++ "/codemodel-v2"
                                   clientStatefulQueryFile = clientRequestDirectory ++ "/query.json"
    in do clientStatefulQueryFileExist <- doesFileExistFunc clientStatefulQueryFile
          when clientStatefulQueryFileExist $ updateClientStatefulQuery clientStatefulQueryFile
          clientStatelessQueryFileExist <- doesFileExistFunc clientStatelessQueryFile
          sharedStatelessQueryFileExist <- doesFileExistFunc sharedStatelessQueryFile
          unless (clientStatefulQueryFileExist || clientStatelessQueryFileExist || sharedStatelessQueryFileExist)
              $ createClientStatefulQuery clientStatefulQueryFile

analyzeCMakeOutputIO :: FilePath -> IO (ParseResult CodeModelCMakeOutput)
analyzeCMakeOutputIO = analyzeCMakeOutput

putCMakeQueryIO :: FilePath -> IO ()
putCMakeQueryIO = putCMakeQuery
