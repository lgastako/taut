{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.Message.Attachment.Action.Confirm
       ( Confirm( Confirm )
       , dismissText
       , okText
       , text
       , title
       ) where

import Focus.Prelude

import Control.Lens              ( makeLenses )
import Data.Aeson                ( FromJSON( parseJSON )
                                 , ToJSON ( toJSON )
                                 , defaultOptions
                                 , genericParseJSON
                                 , genericToJSON
                                 )
import Data.Aeson.Types          ( Options( fieldLabelModifier
                                          , omitNothingFields
                                          )
                                 , camelTo2
                                 )
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 , genericShrink
                                 , shrink
                                 )
import Test.QuickCheck.Instances ()

data Confirm = Confirm
  { _dismissText :: Text
  , _okText      :: Text
  , _title       :: Text
  , _text        :: Text
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''Confirm

instance FromJSON Confirm where
  parseJSON = genericParseJSON confirmOptions

instance ToJSON Confirm where
  toJSON = genericToJSON confirmOptions

instance Arbitrary Confirm where
  arbitrary = Confirm
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

confirmOptions :: Options
confirmOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1
  , omitNothingFields = True
  }
