{-# LANGUAGE DeriveGeneric #-}

module Common (
    Upload(..)
  , CurlploadSettings(..)
  ) where

import Data.Time (UTCTime)
import Data.Word (Word16)
import GHC.Generics (Generic)


data Upload = Upload {
    uFileName        :: FilePath
  , uMimeType        :: String
  , uDispositionType :: String
  , uCreationDate    :: UTCTime
  } deriving (Eq, Show, Generic)


data CurlploadSettings = CurlploadSettings {
    csDBHost      :: String
  , csDBPort      :: Word16
  , csDBName      :: String
  , csDBUser      :: String
  , csUploadsPath :: FilePath
  , csHostName    :: String
  , csKeepNames   :: Bool
  }
