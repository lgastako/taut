{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Taut.Types.UserId
       ( UserId
       , fromText
       , toText
       , uidText
       ) where

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
import Data.Text.Encoding        ( encodeUtf8 )
import GHC.Generics              ( Generic )
import Infinity
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )
import Test.QuickCheck.Instances ()

newtype UserId = UserId Text
  deriving (Eq, Generic, Ord, Read, Show, ToJSONKey, FromJSONKey)

instance ToField UserId where
  toField = encodeUtf8 . toText

fromText :: Text -> UserId
fromText = UserId

toText :: UserId -> Text
toText (UserId u) = u

uidText :: Iso' UserId Text
uidText = iso toText fromText

$(deriveJSON defaultOptions ''UserId)

derive makeArbitrary ''UserId
