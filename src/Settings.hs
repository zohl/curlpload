{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Settings (
    bindOptions
  , getSettings
  ) where


import Common (CurlploadSettings(..))
import Data.Default (Default, def)
import Data.Ini (Ini(..), readIniFile)
import Data.Maybe (listToMaybe, fromMaybe, catMaybes)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Database.PostgreSQL.Simple.Bind (PostgresBindOptions(..), PGFunction(..), ReturnType(..))
import GHC.Generics (Generic)
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
    pboFunctionName    = mkFunctionName
  , pboSetOfReturnType = \case
      "t_upload" -> AsRow
      _          -> AsField
  , pboIsNullable      = isNullable
  } where
    isNullable :: String -> String -> Bool
    isNullable _ _ = False


data CmdArgs = CmdArgs {
    cmdConfigFile  :: Maybe FilePath

  , cmdUploadsPath :: Maybe FilePath
  , cmdKeepNames   :: Maybe Bool

  , cmdHostName    :: Maybe String
  , cmdHostPort    :: Maybe Int
  } deriving (Show, Data, Typeable)


data IniArgs = IniArgs {
    iniDBHost      :: Maybe String
  , iniDBPort      :: Maybe Word16
  , iniDBName      :: Maybe String
  , iniDBUser      :: Maybe String

  , iniUploadsPath :: Maybe FilePath
  , iniKeepNames   :: Maybe Bool

  , iniHostName    :: Maybe String
  , iniHostPort    :: Maybe Int
  } deriving (Show, Generic)

instance Default IniArgs

readIniFileM :: SyslogFn -> FilePath -> IO (Maybe Ini)
readIniFileM syslog fn = readIniFile fn >>= either
  (\err -> do
      syslog DAEMON Error (BSC8.pack $ "Cannot parse ini file: " ++ err)
      return Nothing)
  (return . Just)


getSettings :: SyslogFn -> IO CurlploadSettings
getSettings syslog = do
  (CmdArgs {..}) <- cmdArgs CmdArgs {
        cmdConfigFile = def
          &= explicit &= name "config-file" &= name "c"
          &= help "Configuration file"
          &= typFile

      , cmdUploadsPath = def
          &= explicit &= name "uploads" &= name "u"
          &= help "Directory with uploads"
          &= typDir

      , cmdKeepNames = def
          &= explicit &= name "keep-names" &= name "k"
          &= help "Keep names of uploaded files"

      , cmdHostName = def
          &= explicit &= name "host-name" &= name "h"
          &= help "Host name"

      , cmdHostPort = def
          &= explicit &= name "host-port" &= name "p"
          &= help "Port of the application"
    }

  (IniArgs {..}) <- case cmdConfigFile of
    Nothing -> return def
    Just fn -> readIniFileM syslog fn >>= \case
      Nothing -> return def
      Just d  -> return IniArgs {
          iniDBHost      =          fromIni "Database" "host"
        , iniDBPort      = read <$> fromIni "Database" "port"
        , iniDBName      =          fromIni "Database" "name"
        , iniDBUser      =          fromIni "Database" "user"
        , iniUploadsPath =          fromIni "Uploads"  "path"
        , iniKeepNames   = read <$> fromIni "Uploads"  "keep_names"
        , iniHostName    =          fromIni "Server"   "host"
        , iniHostPort    = read <$> fromIni "Server"   "port"
        } where
        fromIni :: T.Text -> T.Text -> Maybe String
        fromIni section key = T.unpack <$> (HMap.lookup section (unIni d) >>= HMap.lookup key)

  uploadsPath' <- getTemporaryDirectory >>= \tmp -> do
    let result = tmp </> "uploads"
    createDirectoryIfMissing True result
    return result

  return CurlploadSettings {
      csDBHost      = getValue "localhost"      [iniDBHost                     ]
    , csDBPort      = getValue 5432             [iniDBPort                     ]
    , csDBName      = getValue "curlpload"      [iniDBName                     ]
    , csDBUser      = getValue "curlpload"      [iniDBUser                     ]
    , csUploadsPath = getValue uploadsPath'     [iniUploadsPath, cmdUploadsPath]
    , csKeepNames   = getValue False            [iniKeepNames  , cmdKeepNames  ]
    , csHostName    = getValue "localhost:8080" [iniHostName   , cmdHostName   ]
    , csHostPort    = listToMaybe . catMaybes $ [iniHostPort   , cmdHostPort   ]
    } where
    getValue :: a -> [Maybe a] -> a
    getValue x mxs = fromMaybe x (listToMaybe . catMaybes $ mxs)
