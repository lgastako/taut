{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Taut.Types.UserId
     ( UserId
     , fromText
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

newtype UserId = UserId { unUserId :: Text }
  deriving (Eq, FromJSON, FromJSONKey, Generic, Ord, Read, Show, ToJSON, ToJSONKey)

instance ToField UserId where
  toField = encodeUtf8 . unUserId

fromText :: Text -> UserId
fromText = UserId

uidText :: Iso' UserId Text
uidText = iso unUserId fromText

derive makeArbitrary ''UserId
