{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Taut.Types.BotAccessToken
     ( BotAccessToken
     , fromText
     , fromTextE
     ) where

import Taut.Prelude    hiding ( null )

import Data.Text              ( null, strip )
import Taut.Constants         ( botTokenPrefix )
import Taut.Types.AccessToken ( AccessToken
                              , accessTokenText
                              )
import Test.QuickCheck        ( Arbitrary
                              , arbitrary
                              )

newtype BotAccessToken = BotAccessToken { unBotAccessToken :: Text }
  deriving (Data, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance AccessToken BotAccessToken where
  accessTokenText = unBotAccessToken

instance Arbitrary BotAccessToken where
  arbitrary = BotAccessToken <$> arbitrary

fromText :: Text -> Maybe BotAccessToken
fromText = eitherToMaybe . fromTextE

fromTextE :: Text -> Either Text BotAccessToken
fromTextE text
  | null s                              = Left "Bot access token is an empty string"
  | not $ s `startsWith` botTokenPrefix = Left mismatchedTokensError
  | otherwise                           = Right . BotAccessToken $ s
  where
    s = strip text

    mismatchedTokensError =
      "Attempt to create a BotAccessToken value from a user access token."
        <> " Either supply a bot access token or use AccessToken instead"
        <> " of BotAccessToken."
