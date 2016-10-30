{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Settings (
    bindOptions
  , getSettings
  ) where


import Common (CurlploadSettings(..))
import Data.Default (def)
import Data.Ini (Ini(..), readIniFile)
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.Bind (PostgresBindOptions(..), PGFunction(..))
import System.Console.CmdArgs (Data, cmdArgs, (&=), help, explicit, name, typFile, typDir)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.FilePath.Posix ((</>))
import System.Posix.Syslog (Facility(..), Priority(..), SyslogFn)
import Text.CaseConversion (convertCase, WordCase(..))
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T


mkFunctionName :: PGFunction -> String
mkFunctionName (PGFunction _schema fname _args _result) = convertCase Snake Camel $ fname

bindOptions :: PostgresBindOptions
bindOptions = (def :: PostgresBindOptions) {
    pboFunctionName = mkFunctionName
  }


data Curlpload = Curlpload {
    configFile     :: Maybe FilePath
  , uploadsPath    :: Maybe FilePath
  , hostName       :: Maybe String
  , keepNames      :: Maybe Bool
  } deriving (Show, Data, Typeable)



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
    let fromIni section key = HMap.lookup section (unIni ini) >>= HMap.lookup key

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

