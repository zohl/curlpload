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
  , cmdHostName    :: Maybe String
  , cmdKeepNames   :: Maybe Bool
  } deriving (Show, Data, Typeable)


data IniArgs = IniArgs {
    iniDBHost      :: Maybe String
  , iniDBPort      :: Maybe Word16
  , iniDBName      :: Maybe String
  , iniDBUser      :: Maybe String
  , iniUploadsPath :: Maybe FilePath
  , iniHostName    :: Maybe String
  , iniKeepNames   :: Maybe Bool
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

      , cmdHostName = def
          &= explicit &= name "host-name" &= name "h"
          &= help "Host name"

      , cmdKeepNames = def
          &= explicit &= name "keep-names" &= name "k"
          &= help "Keep names of uploaded files"
    }

  (IniArgs {..}) <- case cmdConfigFile of
    Nothing -> return def
    Just fn -> readIniFileM syslog fn >>= \case
      Nothing -> return def
      Just d  -> return IniArgs {
          iniDBHost      =          fromIni "DATABASE" "host"
        , iniDBPort      = read <$> fromIni "DATABASE" "port"
        , iniDBName      =          fromIni "DATABASE" "name"
        , iniDBUser      =          fromIni "DATABASE" "user"
        , iniUploadsPath =          fromIni "UPLOADS"  "path"
        , iniHostName    =          fromIni "UPLOADS"  "host_name"
        , iniKeepNames   = read <$> fromIni "UPLOADS"  "keep_names"
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
    , csDBName      = getValue "test_db"        [iniDBName                     ]
    , csDBUser      = getValue "test_role"      [iniDBUser                     ]
    , csUploadsPath = getValue uploadsPath'     [iniUploadsPath, cmdUploadsPath]
    , csHostName    = getValue "localhost:8080" [iniHostName   , cmdHostName   ]
    , csKeepNames   = getValue False            [iniKeepNames  , cmdKeepNames  ]
    } where
    getValue :: a -> [Maybe a] -> a
    getValue x mxs = fromMaybe x (listToMaybe . catMaybes $ mxs)
