{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Taut.Types.UserAccessToken
     ( UserAccessToken
     , fromText
     , fromTextE
     , unUserAccessToken
     ) where

import           Taut.Prelude

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Text              as Text
import           Taut.Constants                 ( botTokenPrefix )
import           Taut.Types.AccessToken         ( AccessToken
                                                , accessTokenText
                                                )
import           Test.QuickCheck                ( Arbitrary
                                                , arbitrary
                                                )

newtype UserAccessToken = UserAccessToken { unUserAccessToken :: Text }
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance AccessToken UserAccessToken where
  accessTokenText = unUserAccessToken

instance Arbitrary UserAccessToken where
  arbitrary = UserAccessToken <$> arbitrary

fromText :: Text -> Maybe UserAccessToken
fromText = eitherToMaybe . fromTextE

fromTextE :: Text -> Either Text UserAccessToken
fromTextE text
  | Text.null s                   = Left "User access token is an empty string"
  | s `startsWith` botTokenPrefix = Left mismatchedTokensError
  | otherwise                     = Right . UserAccessToken $ s
  where
    s = Text.strip text

    mismatchedTokensError =
      "Attempt to create a UserAccessToken value from a bot access token."
        <> " Either supply a user access token or use BotAccessToken instead"
        <> " of UserAccessToken."
