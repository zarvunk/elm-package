{-# LANGUAGE FlexibleContexts #-}
module Cache where

import Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.RWS (MonadIO, liftIO, MonadReader, asks)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), dropFileName)

import qualified Elm.Package as Package
import qualified Catalog
import qualified Manager


getJson
    :: (MonadIO m, MonadReader Manager.Environment m, MonadError String m, FromJSON a)
    => String
    -> FilePath
    -> Package.Name
    -> Package.Version
    -> L.Location
    -> (String -> Package.Name -> Package.Version -> m LBS.ByteString)
    -> m a
getJson metadata metadataPath name version location =
  do  cacheDir <- asks Manager.cacheDirectory
      let fullMetadataPath =
            cacheDir </> Package.toFilePath name </> Package.versionToString version </> metadataPath

      exists <- liftIO (doesFileExist fullMetadataPath)

      let handler =
         case location of
            L.Catalog -> Catalog.getJson
            L.GitHub -> undefined
            L.Local path -> undefined path
            L.Url url -> undefined url

      content <-
        case exists of
          True -> liftIO (LBS.readFile fullMetadataPath)
          False ->
            do  response <- handler metadata name version
                liftIO $ createDirectoryIfMissing True (dropFileName fullMetadataPath)
                liftIO $ LBS.writeFile fullMetadataPath response
                return response

      case Json.eitherDecode content of
        Right value -> return value
        Left err ->
          throwError $
            "Unable to get " ++ metadataPath ++ " for "
            ++ Package.toString name ++ " " ++ Package.versionToString version
            ++ "\n" "from the " ++ locationMessage
            ++ "\n" ++ err

  where locationMessage =
            case location of
                L.Catalog ->
                    "catalog"
                L.GitHub ->
                    "repo at https://github.com/" ++ Package.nameToString name
                L.Local path ->
                    "local repo at " ++ path
                L.Url url ->
                    "repo at " ++ url
