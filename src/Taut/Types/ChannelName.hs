{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.ChannelName
       ( ChannelName
       , channelName
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

newtype ChannelName = ChannelName Text
  deriving (Eq, Generic, Ord, Read, Show)

instance FromHttpApiData ChannelName where
  parseQueryParam = Right . fromText

instance ToHttpApiData ChannelName where
  toQueryParam = toText

fromText :: Text -> ChannelName
fromText = ChannelName

toText :: ChannelName -> Text
toText (ChannelName n) = n

channelName :: Iso' ChannelName Text
channelName = iso toText fromText

$(deriveJSON defaultOptions ''ChannelName)

derive makeArbitrary ''ChannelName
