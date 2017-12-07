{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Taut.Types.AccessToken
       ( AccessToken
       , AnyAccessToken
       , accessTokenText
       , fromText
       ) where

import Focus.Prelude

class AccessToken a where
  accessTokenText :: a -> Text

data AnyAccessToken = AnyAccessToken Text
  deriving (Eq, Generic, Ord, Read, Show)

instance AccessToken AnyAccessToken where
  accessTokenText (AnyAccessToken text) = text

fromText :: Text -> AnyAccessToken
fromText = AnyAccessToken
