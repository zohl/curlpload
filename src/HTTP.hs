{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HTTP (
    ContentDisposition(..)
  , hContentDisposition
  , parseContentDisposition
  , formatContentDisposition

  , hVisibilityType
  , parseVisibilityType

  , processBody
  ) where

import Control.Applicative (liftA2, (<|>))
import Data.Attoparsec.ByteString (Parser, parseOnly)
import Data.Attoparsec.ByteString.Char8 (takeWhile1, notInClass, char, skipSpace)
import Data.ByteString.Builder (Builder, toLazyByteString, byteString)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC8
import Common (Visibility)
import Data.Char (isAlpha)

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


hVisibilityType :: HeaderName
hVisibilityType = "Visibility-Type"

parseVisibilityType :: BS.ByteString -> Maybe Visibility
parseVisibilityType = either (const Nothing) Just . (parseOnly header) where
  header :: Parser Visibility
  header = do
    _ <- skipSpace
    read . BSC8.unpack <$> takeWhile1 isAlpha


processBody :: Request -> IO BSL.ByteString
processBody request = toLazyByteString <$> getBody (byteString BS.empty) where
  getBody :: Builder -> IO Builder
  getBody builder = requestBody request >>= \result ->
    if (result == BS.empty)
    then (return builder)
    else getBody (builder <> (byteString result))
