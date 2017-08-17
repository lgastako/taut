{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.ChannelId
       ( ChannelId
       , cidText
       , fromText
       , toText
       ) where

import Control.Lens              ( Iso'
                                 , iso
                                 )
import Data.Aeson.TH             ( defaultOptions
                                 , deriveJSON
                                 )
import Data.Csv                  ( ToField
                                 , toField
                                 )
import Data.DeriveTH             ( derive
                                 , makeArbitrary
                                 )
import Data.Text.Encoding        ( encodeUtf8 )
import Infinity
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )
import Test.QuickCheck.Instances ()

newtype ChannelId = ChannelId Text
  deriving (Eq, Generic, Ord, Read, Show)

instance ToField ChannelId where
  toField = encodeUtf8 . toText

fromText :: Text -> ChannelId
fromText = ChannelId

toText :: ChannelId -> Text
toText (ChannelId u) = u

cidText :: Iso' ChannelId Text
cidText = iso toText fromText

$(deriveJSON defaultOptions ''ChannelId)

derive makeArbitrary ''ChannelId
