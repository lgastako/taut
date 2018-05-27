{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Taut.Types.AccessToken
     ( AccessToken
     , AnyAccessToken
     , accessTokenText
     , fromText
     ) where

import Taut.Prelude


class AccessToken a where
  accessTokenText :: a -> Text

newtype AnyAccessToken = AnyAccessToken { unAnyAccessToken :: Text }
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance AccessToken AnyAccessToken where
  accessTokenText = unAnyAccessToken

fromText :: Text -> AnyAccessToken
fromText = AnyAccessToken
