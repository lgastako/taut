{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Taut.Types.TeamId
       ( TeamId
       , fromText
       , tidText
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

newtype TeamId = TeamId Text
  deriving (Eq, Generic, Ord, Read, Show, FromJSONKey, ToJSONKey)

instance ToField TeamId where
  toField = encodeUtf8 . toText

fromText :: Text -> TeamId
fromText = TeamId

toText :: TeamId -> Text
toText (TeamId u) = u

tidText :: Iso' TeamId Text
tidText = iso toText fromText

$(deriveJSON defaultOptions ''TeamId)

derive makeArbitrary ''TeamId
