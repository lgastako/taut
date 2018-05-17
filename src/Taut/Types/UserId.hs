{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Taut.Types.UserId
       ( UserId
       , fromText
       , toText
       , uidText
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
import           Web.HttpApiData                                       ( ToHttpApiData
                                                                       , toQueryParam
                                                                       )

newtype UserId = UserId Text
  deriving (Eq, Generic, Ord, Read, Show, FromJSONKey, ToJSONKey)

instance ToField UserId where
  toField = encodeUtf8 . toText

instance ToHttpApiData UserId where
  toQueryParam = toText

fromText :: Text -> UserId
fromText = UserId

toText :: UserId -> Text
toText (UserId u) = u

uidText :: Iso' UserId Text
uidText = iso toText fromText

$(deriveJSON defaultOptions ''UserId)

derive makeArbitrary ''UserId
