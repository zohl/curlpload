{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Common (
    Upload(..)
  , CurlploadSettings(..)
  ) where

import Data.Char (isAlpha)
import Data.Time (UTCTime, NominalDiffTime)
import Data.Word (Word16)
import GHC.Generics (Generic)

data Visibility = Public | Private | Default deriving (Show, Eq)

instance Read Visibility where
  reads s = let (w, s') = break (not . isAlpha) s in case w of
    "public"  -> (Public, s')
    "private" -> (Private, s')
    "default" -> (Default, s')
    otherwise -> error $ "unknown visibility type: '" ++ w ++ "'"


data Upload = Upload {
    uFileName        :: FilePath
  , uMimeType        :: String
  , uDispositionType :: String
  , uCreationDate    :: UTCTime
  } deriving (Eq, Show, Generic)


data CurlploadSettings = CurlploadSettings {
    csDBHost             :: String
  , csDBPort             :: Word16
  , csDBName             :: String
  , csDBUser             :: String
  , csDBPassword         :: Maybe FilePath
  , csUploadsPath        :: FilePath
  , csKeepNames          :: Bool
  , csHostName           :: String
  , csHostPort           :: Maybe Int
  , csHostLifetime       :: Maybe NominalDiffTime
  , csShareDir           :: FilePath
  , csPublicHashLength   :: Int
  , csPrivateHashLength  :: Int
  , csDefaultPrivacyType :: Visibility
  } deriving (Eq, Show)
