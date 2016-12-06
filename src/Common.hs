{-# LANGUAGE DeriveGeneric #-}

module Common (
    Upload(..)
  , CurlploadSettings(..)
  ) where

import Data.Time (UTCTime, NominalDiffTime)
import Data.Word (Word16)
import GHC.Generics (Generic)


data Upload = Upload {
    uFileName        :: FilePath
  , uMimeType        :: String
  , uDispositionType :: String
  , uCreationDate    :: UTCTime
  } deriving (Eq, Show, Generic)


data CurlploadSettings = CurlploadSettings {
    csDBHost       :: String
  , csDBPort       :: Word16
  , csDBName       :: String
  , csDBUser       :: String
  , csDBPassword   :: Maybe FilePath
  , csDBScripts    :: FilePath
  , csUploadsPath  :: FilePath
  , csKeepNames    :: Bool
  , csHostName     :: String
  , csHostPort     :: Maybe Int
  , csHostLifetime :: Maybe NominalDiffTime
  } deriving (Eq, Show)
