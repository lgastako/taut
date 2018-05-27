{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Taut.Types.BotAccessToken
     ( BotAccessToken
     , fromText
     , fromTextE
     ) where

import           Taut.Prelude

import qualified Data.Text              as Text
import           Taut.Constants                 ( botTokenPrefix )
import           Taut.Types.AccessToken         ( AccessToken
                                                , accessTokenText
                                                )

newtype BotAccessToken = BotAccessToken { unBotAccessToken :: Text }
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance AccessToken BotAccessToken where
  accessTokenText = unBotAccessToken

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
