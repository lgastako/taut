{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Taut.Types.BotAccessToken
       ( BotAccessToken
       , fromText
       , fromTextE
       ) where

import qualified Data.Text              as Text
import           Focus.Prelude
import           Taut.Constants                 ( botTokenPrefix )
import           Taut.Types.AccessToken         ( AccessToken
                                                , accessTokenText
                                                )

data BotAccessToken = BotAccessToken Text
  deriving (Eq, Generic, Ord, Read, Show)

instance AccessToken BotAccessToken where
  accessTokenText (BotAccessToken text) = text

fromText :: Text -> Maybe BotAccessToken
fromText = eitherToMaybe . fromTextE

fromTextE :: Text -> Either Text BotAccessToken
fromTextE text
  | Text.null s                         = Left "Bot access token is an empty string"
  | not $ s `startsWith` botTokenPrefix = Left mismatchedTokensError
  | otherwise                           = Right . BotAccessToken $ s
  where
    s = Text.strip text

    mismatchedTokensError =
      "Attempt to create a BotAccessToken value from a user access token."
        <> " Either supply a bot access token or use AccessToken instead"
        <> " of BotAccessToken."
