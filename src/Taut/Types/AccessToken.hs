{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Taut.Types.AccessToken
       ( AccessToken
       , AnyAccessToken
       , accessTokenString
       , accessTokenText
       , fromText
       ) where

import Focus.Prelude

import Data.Text     ( pack
                     , unpack
                     )

class AccessToken a where
  accessTokenText :: a -> Text

instance AccessToken Text where
  accessTokenText = identity

instance AccessToken String where
  accessTokenText = pack

data AnyAccessToken = AnyAccessToken Text
  deriving (Eq, Generic, Ord, Read, Show)

instance AccessToken AnyAccessToken where
  accessTokenText (AnyAccessToken text) = text

fromText :: Text -> AnyAccessToken
fromText = AnyAccessToken

accessTokenString :: AccessToken a => a -> String
accessTokenString = unpack . accessTokenText
