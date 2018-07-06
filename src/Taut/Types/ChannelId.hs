{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Taut.Types.ChannelId
     ( ChannelId
     , cidText
     , fromText
     , toText
     , unChannelId
     ) where

import Taut.Prelude

import Control.Lens              ( Iso'
                                 , iso
                                 )
import Data.Aeson.Types          ( FromJSONKey
                                 , ToJSONKey
                                 , toJSONKey
                                 , toJSONKeyText
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

newtype ChannelId = ChannelId { unChannelId :: Text }
  deriving (Data, Eq, FromJSON, FromJSONKey, Generic, Ord, Read, Show, ToJSON)

instance ToJSONKey ChannelId where
  toJSONKey = toJSONKeyText unChannelId

instance ToField ChannelId where
  toField = encodeUtf8 . unChannelId

instance FromHttpApiData ChannelId where
  parseQueryParam = Right . fromText

instance ToHttpApiData ChannelId where
  toQueryParam = unChannelId

fromText :: Text -> ChannelId
fromText = ChannelId

cidText :: Iso' ChannelId Text
cidText = iso unChannelId fromText

toText :: ChannelId -> Text
toText = unChannelId

derive makeArbitrary ''ChannelId
