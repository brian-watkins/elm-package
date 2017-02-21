{-# LANGUAGE OverloadedStrings #-}
module Catalog where

import Control.Exception (catch, throwIO)
import Control.Monad.Except (throwError)
import Control.Monad.RWS (liftIO, asks)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock as Time
import qualified Data.List as List
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Version (showVersion)
import Network.HTTP
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.MultipartFormData as Multi
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>), dropFileName)
import System.IO.Error (isDoesNotExistError)

import qualified Elm.Docs as Docs
import qualified Elm.Package.Description as Desc
import qualified Elm.Package as Package
import qualified Elm.Package.Paths as P
import qualified Manager
import qualified Paths_elmer_package as This
import qualified Reporting.Error as Error
import qualified Utils.Http as Http
import qualified Elmer

-- MAKE URL

catalogUrl :: String -> String -> [(String,String)] -> String
catalogUrl host path vars =
  let
    version = ("elm-package-version", showVersion This.version)
  in
    host ++ "/" ++ path ++ "?" ++ urlEncodeVars (version : vars)


catalog :: String -> [(String,String)] -> Manager.Manager String
catalog path vars =
  do  domain <- asks Manager.catalog
      return $ catalogUrl domain path vars



-- EASY REQUESTS


versions :: Package.Name -> Manager.Manager (Maybe [Package.Version])
versions name =
  get "versions" [("name", Package.toString name)]


permissions :: Package.Name -> Manager.Manager Bool
permissions name =
  get "permissions" [("name", Package.toString name)]


get :: (Binary.Binary a) => String -> [(String,String)] -> Manager.Manager a
get path vars =
  do  url <- catalog path vars
      Http.send url $ \request manager ->
        do  response <- Client.httpLbs request manager
            return $ Binary.decode $ Client.responseBody response



-- FANCIER REQUESTS


allPackages :: Maybe Time.UTCTime -> Manager.Manager (Maybe [(Package.Name, [Package.Version])])
allPackages maybeTime =
  do  url <- catalog "all-packages" vars
      Http.send url $ \request manager -> do
          response <- Client.httpLbs request manager
          case Json.eitherDecode (Client.responseBody response) of
            Left _ ->
              return Nothing

            Right summaries ->
              return $ Just $ map (\(PackageSummary s) -> s) summaries
  where
    vars =
      case maybeTime of
        Nothing -> []
        Just time -> [("since", show time)]

elmerPackage :: Maybe Time.UTCTime -> Manager.Manager (Maybe [(Package.Name, [Package.Version])])
elmerPackage maybeTime =
  do  elmerUrl <- return $ catalogUrl Elmer.catalogHost "all-packages" vars
      Http.send elmerUrl $ \request manager -> do
          elmerResponse <- Client.httpLbs request manager
          case Json.eitherDecode (Client.responseBody elmerResponse) of
            Left _ ->
              return $ Nothing

            Right elmerVersions ->
              return $ Just $ map (\(PackageSummary s) -> s) elmerVersions

  where
    vars =
      case maybeTime of
        Nothing -> []
        Just time -> [("since", show time)]


newtype PackageSummary = PackageSummary (Package.Name, [Package.Version])


instance Json.FromJSON PackageSummary where
    parseJSON (Json.Object obj) =
      do  name <- obj .: "name"
          versions <- obj .: "versions"
          return (PackageSummary (name, versions))

    parseJSON _ =
      fail "package summary must be an object"


register :: Package.Name -> Package.Version -> Manager.Manager ()
register name version =
  do  url <- catalog "register" vars
      Http.send url $ \request manager -> do
          request' <- Multi.formDataBody files request
          let request'' = request' { Client.responseTimeout = Nothing }
          Client.httpLbs request'' manager
          return ()
  where
    vars =
        [ ("name", Package.toString name)
        , ("version", Package.versionToString version)
        ]

    files =
        [ Multi.partFileSource "documentation" P.documentation
        , Multi.partFileSource "description" P.description
        , Multi.partFileSource "readme" "README.md"
        ]



-- GET JSON


description :: Package.Name -> Package.Version -> Manager.Manager Desc.Description
description name version =
  getJson "description" P.description name version


documentation :: Package.Name -> Package.Version -> Manager.Manager [Docs.Documentation]
documentation name version =
  getJson "documentation" "documentation.json" name version


jsonUrl :: String -> Package.Name -> Package.Version -> Manager.Manager String
jsonUrl metadata name version =
  let
    vars = [("name", Package.toString name), ("version", Package.versionToString version)]
  in
    do
      if name == Elmer.packageName then
        return $ catalogUrl Elmer.catalogHost metadata vars
        else do
          catalog metadata vars


getJson :: (Json.FromJSON a) => String -> FilePath -> Package.Name -> Package.Version -> Manager.Manager a
getJson metadata metadataPath name version =
  do  cacheDir <- asks Manager.cacheDirectory
      let fullMetadataPath =
            cacheDir </> Package.toFilePath name </> Package.versionToString version </> metadataPath

      exists <- liftIO (doesFileExist fullMetadataPath)

      content <-
        case exists of
          True ->
            liftIO (LBS.readFile fullMetadataPath)

          False ->
            do  url <- jsonUrl metadata name version
                Http.send url $ \request manager ->
                    do  response <- Client.httpLbs request manager
                        createDirectoryIfMissing True (dropFileName fullMetadataPath)
                        LBS.writeFile fullMetadataPath (Client.responseBody response)
                        return (Client.responseBody response)

      case Json.eitherDecode content of
        Right value ->
          return value

        Left problem ->
          do  liftIO $ removeIfExists fullMetadataPath
              throwError $ Error.CorruptJson metadataPath name version problem


removeIfExists :: FilePath -> IO ()
removeIfExists path =
  let
    handleExists e =
      if isDoesNotExistError e then return () else throwIO e
  in
    removeFile path `catch` handleExists
