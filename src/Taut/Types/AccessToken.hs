{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Taut.Types.AccessToken
     ( AccessToken
     , AnyAccessToken
     , accessTokenText
     , fromText
     , toText
     ) where

import Taut.Prelude

import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )
import Test.QuickCheck.Instances ()

class AccessToken a where
  accessTokenText :: a -> Text

newtype AnyAccessToken = AnyAccessToken { unAnyAccessToken :: Text }
  deriving (Data, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance AccessToken AnyAccessToken where
  accessTokenText = unAnyAccessToken

instance Arbitrary AnyAccessToken where
  arbitrary = AnyAccessToken <$> arbitrary

fromText :: Text -> AnyAccessToken
fromText = AnyAccessToken

toText :: AnyAccessToken -> Text
toText = unAnyAccessToken
