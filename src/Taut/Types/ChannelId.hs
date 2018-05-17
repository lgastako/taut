{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.ChannelId
       ( ChannelId
       , cidText
       , fromText
       , toText
       ) where

import Focus.Prelude

import Control.Lens              ( Iso'
                                 , iso
                                 )
import Data.Aeson.TH             ( defaultOptions
                                 , deriveJSON
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
import Web.HttpApiData           ( ToHttpApiData
                                 , toQueryParam
                                 )

newtype ChannelId = ChannelId Text
  deriving (Eq, Generic, Ord, Read, Show, FromJSONKey)

instance ToJSONKey ChannelId where
  toJSONKey = toJSONKeyText toText

instance ToField ChannelId where
  toField = encodeUtf8 . toText

instance ToHttpApiData ChannelId where
  toQueryParam = toText

fromText :: Text -> ChannelId
fromText = ChannelId

toText :: ChannelId -> Text
toText (ChannelId u) = u

cidText :: Iso' ChannelId Text
cidText = iso toText fromText

$(deriveJSON defaultOptions ''ChannelId)

derive makeArbitrary ''ChannelId
