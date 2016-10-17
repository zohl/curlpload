{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Config (bindOptions)
import Control.Applicative (liftA2, (<|>))
import Control.Exception.Base (bracket, catch)
import Control.Monad (void)
import Data.Attoparsec.ByteString (Parser, parseOnly, many') 
import Data.Attoparsec.ByteString.Char8 (takeWhile1, notInClass, char, notChar, stringCI, skipSpace, scan)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import Data.Default (def)
import Data.HashMap.Strict (HashMap)
import Data.Ini (Ini(..), readIniFile)
import Data.List (intercalate)
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Database.PostgreSQL.Simple (SqlError, Connection, ConnectInfo(..), (:.)(..), connect, close, execute_, query_, Only(..))
import Database.PostgreSQL.Simple.Ok (Ok(..)) 
import Database.PostgreSQL.Simple.Bind (bindFunction, PostgresType)
import Database.PostgreSQL.Simple.Types (Query(..))
import Database.PostgreSQL.Simple.FromField (Conversion, FieldParser)
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError, typename)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Network.HTTP.Types (StdMethod(..), HeaderName, parseMethod, status200, status404, status403, hContentType)
import Network.Wai (Application, Request(..), responseLBS, responseFile)
import Network.Wai.Handler.Warp (runSettingsSocket, defaultSettings)
import Network.Wai.Handler.Warp.AutoQuit (withAutoQuit, withHeartBeat, AutoQuitSettings(..))
import Network.Wai.Handler.Warp.SocketActivation (withSocketActivation, SocketActivationSettings(..))
import System.Console.CmdArgs (Data, cmdArgs, (&=), help, explicit, name, typ, typFile, typDir, opt)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.FilePath.Posix ((</>), takeExtension)
import System.IO (withFile, IOMode(..))
import System.Posix.Syslog (Option(..), withSyslog)
import System.Posix.Syslog (SyslogConfig(..), Facility(..), Priority(..), PriorityMask(..), SyslogFn)
import Text.Heredoc (str)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import Debug.Trace 



data Upload = Upload {
    uFileName        :: FilePath
  , uMimeType        :: String
  , uDispositionType :: String
  , uCreationDate    :: UTCTime
  } deriving (Eq, Show)



type instance PostgresType "varchar"     = String
type instance PostgresType "text"        = Text
type instance PostgresType "timestamptz" = UTCTime 
type instance PostgresType "t_upload"    = Upload


concat <$> mapM (bindFunction bindOptions) [
    "function get_version () returns varchar"

  , [str|
    |function add_upload (
    |  p_file_name   varchar
    |, p_mime_type   varchar
    |, p_disposition varchar
    |) returns varchar
    |]
    
  , "function get_upload (p_name varchar) returns setof t_upload"
  ]


-- TODO rewrite with generic parser
instance FromField Upload where
  fromField f v = checkType parseValue where

    checkType cb = (("t_upload" /=) <$> typename f) >>= \b -> case b of
      True -> returnError Incompatible f ""
      False -> ($ v) $ maybe (returnError UnexpectedNull f "") cb

    parseValue bs = ($ (parseOnly value bs)) $ either
      (returnError ConversionFailed f) pure

    value = do
      _                <- char '('
      uFileName        <- postgresString
      _                <- char ','
      uMimeType        <- postgresString
      _                <- char ','
      uDispositionType <- postgresString
      _                <- char ','
      tmp              <- postgresString
      _                <- char ')'
      let uCreationDate = posixSecondsToUTCTime 0
      return $ Upload {..}

    postgresString = ((char '"') *> (many' $ notChar '"') <* (char '"')) <|> (many' $ notChar ',')


-- TODO import from http-types
hContentDisposition :: HeaderName
hContentDisposition = "Content-Disposition"


data ContentDisposition = ContentDisposition {
    cdType     :: String
  , cdFilename :: FilePath                       
  } deriving (Eq, Show)

  
parseContentDisposition :: BS.ByteString -> Maybe ContentDisposition
parseContentDisposition = either (const Nothing) Just . (parseOnly header) where
  header :: Parser ContentDisposition 
  header = do
    _ <- skipSpace
    cdType <- BSC8.unpack <$> (takeWhile1 $ notInClass " ;")
    cdFilename <- (BSC8.unpack . fromMaybe undefined . lookup "filename") <$> params
    return $ ContentDisposition {..}

    
  params :: Parser [(BS.ByteString, BS.ByteString)]
  params = (liftA2 (:) param params) <|> (return [])

  param :: Parser (BS.ByteString, BS.ByteString)
  param = do
    _ <- skipSpace <* char ';' <* skipSpace
    name <- (takeWhile1 (notInClass " ="))
    _ <- skipSpace <* char '=' <* skipSpace
    value <- ((char '"') *> (takeWhile1 (/= '"')) <* (char '"')) <|> (takeWhile1 (/= ';'))
    return (name, value)
    
    
formatContentDisposition :: ContentDisposition -> BS.ByteString
formatContentDisposition (ContentDisposition {..}) = BSC8.pack . concat $ [
    cdType, "; filename=\"", cdFilename, "\""
  ]

withDB :: ConnectInfo -> (Connection -> IO a) -> IO a
withDB connectInfo = bracket (connect connectInfo) close

include :: Connection -> String -> IO ()
include conn fn = void $ BS.readFile fn >>= (execute_ conn . Query)

data Curlpload = Curlpload {
    configFile     :: Maybe FilePath
  , uploadsPath    :: Maybe FilePath
  , hostName       :: Maybe String
  , keepNames      :: Maybe Bool
  } deriving (Show, Data, Typeable)

data CurlploadSettings = CurlploadSettings {
    csDBHost      :: String
  , csDBPort      :: Word16
  , csDBName      :: String
  , csDBUser      :: String
  , csUploadsPath :: FilePath
  , csHostName    :: String    
  , csKeepNames   :: Bool
  }

getSettings :: SyslogFn -> IO CurlploadSettings
getSettings syslog = do
    cmd <- cmdArgs Curlpload {
        configFile = def
          &= explicit &= name "config-file" &= name "c"
          &= help "Configuration file"
          &= typFile

      , uploadsPath = def
          &= explicit &= name "uploads" &= name "u"
          &= help "Directory with uploads"
          &= typDir

      , hostName = def
          &= explicit &= name "host-name" &= name "h"
          &= help "Host name"

      , keepNames = def
          &= explicit &= name "keep-names" &= name "k"
          &= help "Keep names of uploaded files"
      }

    let emptyIni = Ini . HMap.fromList $ []
    ini <- ($ (configFile cmd)) $ maybe
      (return emptyIni)
      (\fn -> readIniFile fn >>= either 
        (\err -> do
           syslog DAEMON Error (BSC8.pack $ "Cannot parse ini file: " ++ err)
           return emptyIni) 
        (return))
    let fromIni section name = HMap.lookup section (unIni ini) >>= HMap.lookup name

    uploadsPath' <- getTemporaryDirectory >>= \tmp -> do
      let result = tmp </> "uploads"
      createDirectoryIfMissing True result
      return result

    return $ CurlploadSettings {
        csDBHost = fromMaybe "localhost" (T.unpack <$> fromIni "DATABASE" "host")
      , csDBPort = fromMaybe 5432 ((read . T.unpack) <$> fromIni "DATABASE" "port")
      , csDBName = fromMaybe "curlpload" (T.unpack <$> fromIni "DATABASE" "name")
      , csDBUser = fromMaybe "curlpload" (T.unpack <$> fromIni "DATABASE" "user")

      , csUploadsPath = fromMaybe uploadsPath' . listToMaybe . catMaybes $ [
          (T.unpack <$> fromIni "UPLOADS" "path")
        , (uploadsPath cmd)
        ]

      , csHostName = fromMaybe "localhost:8080" . listToMaybe . catMaybes $ [
          (T.unpack <$> fromIni "UPLOADS" "host_name")
        , (hostName cmd)
        ]

      , csKeepNames = fromMaybe False . listToMaybe . catMaybes $ [
          ((read . T.unpack) <$> fromIni "UPLOADS" "keep_names")
        , (keepNames cmd)
        ]
      }


processBody :: Request -> (BS.ByteString -> IO a) -> IO [a]
processBody request f = requestBody request >>= \result ->
  if (result == BS.empty)
  then return []
  else liftA2 (:) (f result) (processBody request f)
                                              
app :: CurlploadSettings -> Connection -> Application
app (CurlploadSettings {..}) conn request respond = dispatch (method, path) where
  method = parseMethod . requestMethod $ request
  path = T.unpack <$> pathInfo request

  dispatch = \case
    (Right GET, []) -> respond $ responsePlain status200 $ tail $ [str|
      |#!/bin/sh 
      |#
      |# This script uploads given file to the server.
      |# Usage:
      |# $ upload.sh filename [...script options] [...curl options]
      |#   where script options are:
      |#   --inline            Set disposition type to inline
      |#   --attachment       Set disposition type to attachment
      |
      |DISPOSITION_TYPE=x-default
      |FILE=$1
      |
      |while true; do
      |  shift
      |  case $1 in
      |  --inline)
      |    DISPOSITION_TYPE="inline"
      |    ;;
      |  --attachment)
      |    DISPOSITION_TYPE="attachment"
      |    ;;
      |  *) break;
      |  esac
      |done
      |
      |curl --data-binary "@$FILE" \
      |  -H "Content-Type: `file -bi "$FILE"`" \
      |  -H 'Content-Disposition: '$DISPOSITION_TYPE'; filename="'`basename "$FILE"`'"' \
      |  "$@" |] ++ csHostName ++ "\n"

    (Right POST, []) ->
      withHeader hContentType $ \contentType ->
      withHeader hContentDisposition $ maybe 
        (respond $ responsePlain status403 "Wrong format of Content-Disposition header")
        (\ContentDisposition {..} -> do 
          let filename = case csKeepNames of
                True  -> cdFilename
                False -> takeExtension cdFilename
          name <- addUpload conn filename (BSC8.unpack contentType) cdType
          _    <- withFile (csUploadsPath </> name) WriteMode $ processBody request . BS.hPutStr
          respond $ responsePlain status200 (csHostName </> name))
        . parseContentDisposition 

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

checkDB :: Connection -> IO ()
checkDB conn = do
  version <- catch
    (getVersion conn)
    (\(_ :: SqlError) -> include conn "db/init.sql" >> return "init")

  putStrLn version

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
    identifier = "curpload"
  , options = [PID, ODELAY]
  , defaultFacility = DAEMON
  , priorityMask = UpTo Debug
  }) . withEcho $ \syslog -> do

    syslog DAEMON Notice "Started" 
        
    csSettings <- getSettings syslog

    let saSettings = SocketActivationSettings {
        sasPort = Just 8080
      , sasHostPreference = "*4"
      }

    let aqSettings = AutoQuitSettings {
        aqsTimeout = fromIntegral (600 :: Integer)
      , aqsOnExit = syslog DAEMON Notice "Staying inactive for a long time"
      }


    let connectInfo = ConnectInfo {
          connectHost     = csDBHost csSettings
        , connectPort     = csDBPort csSettings
        , connectDatabase = csDBName csSettings
        , connectUser     = csDBUser csSettings
        , connectPassword = "TEST"
        }

    withSocketActivation saSettings $
      \sock -> withDB connectInfo $
       \conn -> do
         checkDB conn
         withAutoQuit aqSettings $
          \chan -> runSettingsSocket defaultSettings sock $
            withHeartBeat chan $ app csSettings conn

    syslog DAEMON Notice "Exiting"

