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

import Data.Aeson                ( FromJSON
                                 , ToJSON
                                 )
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )
import Test.QuickCheck.Instances ()

class AccessToken a where
  accessTokenText :: a -> Text

newtype AnyAccessToken = AnyAccessToken { unAnyAccessToken :: Text }
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance AccessToken AnyAccessToken where
  accessTokenText = unAnyAccessToken

instance Arbitrary AnyAccessToken where
  arbitrary = AnyAccessToken <$> arbitrary

fromText :: Text -> AnyAccessToken
fromText = AnyAccessToken
