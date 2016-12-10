{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Common (
    Upload(..)
  , CurlploadSettings(..)
  , Visibility(..)
  , (<|>)
  ) where

import Data.Char (isAlpha)
import Data.Time (UTCTime, NominalDiffTime)
import Data.Word (Word16)
import GHC.Generics (Generic)

data Visibility = Public | Private | Default deriving (Eq, Show)

instance Read Visibility where
  readsPrec _ s = let (w, s') = break (not . isAlpha) s in case w of
    "public"  -> [(Public, s')]
    "private" -> [(Private, s')]
    "default" -> [(Default, s')]
    _         -> []

(<|>) :: Visibility -> Visibility -> Visibility
(<|>) Default v = v
(<|>) v _       = v


data Upload = Upload {
    uFileName        :: FilePath
  , uMimeType        :: String
  , uDispositionType :: String
  , uCreationDate    :: UTCTime
  } deriving (Eq, Show, Generic)


data CurlploadSettings = CurlploadSettings {
    csDBHost            :: String
  , csDBPort            :: Word16
  , csDBName            :: String
  , csDBUser            :: String
  , csDBPassword        :: Maybe FilePath
  , csUploadsPath       :: FilePath
  , csKeepNames         :: Bool
  , csHostName          :: String
  , csHostPort          :: Maybe Int
  , csHostLifetime      :: Maybe NominalDiffTime
  , csShareDir          :: FilePath
  , csPublicHashLength  :: Int
  , csPrivateHashLength :: Int
  , csDefaultVisibility :: Visibility
  } deriving (Eq, Show)
