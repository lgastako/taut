{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Taut.Types.UserId
     ( UserId
     , fromText
     , toText
     , uidText
     , unUserId
     ) where

import Taut.Prelude

import Control.Lens              ( Iso'
                                 , iso
                                 )
import Data.Aeson.Types          ( FromJSONKey
                                 , ToJSONKey
                                 )
import Data.Csv                  ( ToField
                                 , toField
                                 )
import Data.DeriveTH             ( derive
                                 , makeArbitrary
                                 )
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )
import Test.QuickCheck.Instances ()
import Web.HttpApiData           ( FromHttpApiData
                                 , ToHttpApiData
                                 , parseQueryParam
                                 , toQueryParam
                                 )

newtype UserId = UserId { unUserId :: Text }
  deriving (Data, Eq, FromJSON, FromJSONKey, Generic, Ord, Read, Show, ToJSON, ToJSONKey)

instance ToField UserId where
  toField = encodeUtf8 . unUserId

instance FromHttpApiData UserId where
  parseQueryParam = Right . fromText

instance ToHttpApiData UserId where
  toQueryParam = unUserId

fromText :: Text -> UserId
fromText = UserId

toText :: UserId -> Text
toText = unUserId

uidText :: Iso' UserId Text
uidText = iso unUserId fromText

derive makeArbitrary ''UserId
