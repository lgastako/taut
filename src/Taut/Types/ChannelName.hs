{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Taut.Types.ChannelName
     ( ChannelName
     , channelName
     , fromText
     , unChannelName
     ) where

import Taut.Prelude

import Control.Lens              ( Iso'
                                 , iso
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

newtype ChannelName = ChannelName { unChannelName :: Text }
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance FromHttpApiData ChannelName where
  parseQueryParam = Right . fromText

instance ToHttpApiData ChannelName where
  toQueryParam = unChannelName

fromText :: Text -> ChannelName
fromText = ChannelName

channelName :: Iso' ChannelName Text
channelName = iso unChannelName fromText

derive makeArbitrary ''ChannelName
