{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Common (CurlploadSettings(..), Upload(..), Visibility(..), Expiration(..), (<|>))
import DB (withDB)
import Data.Default (def)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple (Connection, ConnectInfo(..))
import Database.PostgreSQL.Simple.Bind (bindFunction)
import HTTP (ContentDisposition(..), hContentDisposition, parseContentDisposition)
import HTTP (formatContentDisposition, processBody, hVisibilityType, parseVisibilityType)
import HTTP (hExpirationTime, parseExpirationTime)
import Network.HTTP.Types (HeaderName, StdMethod(..), parseMethod, status200, status404, status403, hContentType)
import Network.Wai (Application, Request(..), responseLBS, responseFile)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import Network.Wai.Handler.Warp.AutoQuit (withAutoQuit, withHeartBeat, AutoQuitSettings(..))
import Network.Wai.Handler.Warp.SocketActivation (withSocketActivation, SocketActivationSettings(..))
import Settings (bindOptions, getSettings)
import System.FilePath.Posix ((</>), takeExtension)
import System.Posix.Syslog (Option(..), withSyslog)
import System.Posix.Syslog (SyslogConfig(..), Facility(..), Priority(..), PriorityMask(..), SyslogFn)
import Text.Heredoc (str)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Control.Exception (Exception)
import Control.Monad.Catch (throwM, catch)
import Data.Typeable (Typeable)

data CurlploadException
  = WrongHeaderFormat HeaderName
  | UnresolvedVisibilityType
  | UnresolvedExpirationTime
  deriving (Eq, Typeable)

instance Show CurlploadException where
  show (WrongHeaderFormat h) = "Wrong format of " ++ (show h) ++ " header"
  show _                     = "Something went wrong"

instance (Exception CurlploadException)


concat <$> mapM (bindFunction bindOptions) [
    [str|
    |function add_upload (
    |  p_file_name    varchar
    |, p_mime_type    varchar
    |, p_disposition  varchar
    |, p_hash_length  bigint
    |, p_expire_after interval default null
    |, p_limit        bigint   default 8
    |) returns varchar
    |]

  , "function get_upload (p_name varchar) returns setof t_upload"
  ]


app :: CurlploadSettings -> SyslogFn -> Connection -> Application
app (CurlploadSettings {..}) syslog conn request respond = dispatch (method, path) where
  method = parseMethod . requestMethod $ request
  path = T.unpack <$> pathInfo request

  dispatch = \case
    (Right GET, []) -> do
      script <- readFile $ csShareDir </> "upload.sh"
      respond $ responsePlain status200 $ script ++ csHostName ++ "\n"

    (Right POST, []) ->
      withHeader hContentType $ \contentType ->
      withHeader hVisibilityType $ \visibilityType ->
      withHeader hExpirationTime $ \expirationTime ->
      withHeader hContentDisposition $ \contentDisposition -> flip catch handleError $ do

        hashLength <- maybe
          (throwM $ WrongHeaderFormat hVisibilityType)
          (return . (<|> csDefaultVisibility))
          (parseVisibilityType visibilityType) >>= \case
              Public  -> return csPublicHashLength
              Private -> return csPrivateHashLength
              _       -> throwM $ UnresolvedVisibilityType

        expiration <- maybe
          (throwM $ WrongHeaderFormat hExpirationTime)
          (return . (<|> csDefaultExpiration))
          (parseExpirationTime expirationTime) >>= \case
              Never         -> return Nothing
              ExpireAfter t -> return . Just $ t
              _             -> throwM $ UnresolvedExpirationTime

        (ContentDisposition {..}) <- maybe
          (throwM $ WrongHeaderFormat hContentDisposition)
          (return)
          (parseContentDisposition contentDisposition)

        let filename = case csKeepNames of
              True  -> cdFilename
              False -> takeExtension cdFilename

        name <- addUpload conn
          filename
          (BSC8.unpack contentType)
          cdType
          hashLength
          expiration
          Nothing

        processBody request >>= BSL.writeFile (csUploadsPath </> name)
        syslog DAEMON Notice . BSC8.pack $ "Uploaded file: " ++ name
        respond $ responsePlain status200 (csHostName </> name)

    (Right GET, [name]) -> (listToMaybe <$> getUpload conn name) >>= maybe
      (respond $ responsePlain status404 "Not found")
      (\Upload {..} -> do
        let cd = ContentDisposition {
            cdType = uDispositionType
          , cdFilename = case csKeepNames of
              True  -> uFileName
              False -> name
          }
        respond $ responseFile
          status200
          [(hContentType, BSC8.pack uMimeType), (hContentDisposition, formatContentDisposition cd)]
          (csUploadsPath </> name)
          Nothing)

    _  -> respond $ responsePlain status403 "Not supported"

  responsePlain status message = responseLBS
    status
    [(hContentType, "text/plain")]
    (BSL.fromStrict . BSC8.pack $ message)

  withHeader hdr response = maybe
    (respond $ responsePlain status403 $ (show hdr) ++ " must be provided")
    (response)
    (lookup hdr $ requestHeaders request)

  handleError = \(ex :: CurlploadException) -> respond $ responsePlain status403 (show ex)


withEcho :: (SyslogFn -> IO a) -> (SyslogFn -> IO a)
withEcho f = \syslog -> f $ \facility priority message -> do
   syslog facility priority message
   putStrLn . intercalate "::" $ [
       show facility
     , show priority
     , BSC8.unpack message
     ]

main :: IO ()
main = withSyslog (SyslogConfig {
    identifier = "curlpload"
  , options = [PID, ODELAY]
  , defaultFacility = DAEMON
  , priorityMask = UpTo Debug
  }) . withEcho $ \syslog -> do

    syslog DAEMON Notice "Started"

    csSettings@(CurlploadSettings {..}) <- getSettings syslog

    let saSettings = (def::SocketActivationSettings) {
        sasPort = csHostPort
      , sasHostPreference = "*4"
      }

    let aqSettings = (def::AutoQuitSettings) {
        aqsTimeout = csHostLifetime
      , aqsOnExit = syslog DAEMON Notice "Staying inactive for a long time"
      }

    dbPassword <- maybe (return "") (fmap init . readFile) csDBPassword

    let connectInfo = ConnectInfo {
          connectHost     = csDBHost
        , connectPort     = csDBPort
        , connectDatabase = csDBName
        , connectUser     = csDBUser
        , connectPassword = dbPassword
        }

    withSocketActivation saSettings $
      \sock -> withDB (csShareDir </> "db") syslog connectInfo $
       \conn -> withAutoQuit aqSettings $
        \chan -> runSettingsSocket defaultSettings sock $
          withHeartBeat chan $ app csSettings syslog conn

    syslog DAEMON Notice "Exiting"
