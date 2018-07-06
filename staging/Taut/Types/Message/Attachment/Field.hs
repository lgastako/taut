{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Taut.Types.Message.Attachment.Field
     ( Field( Field )
     , short
     , title
     , value
     ) where

import Taut.Prelude

import Control.Lens              ( makeLenses )
import Data.Aeson                ( FromJSON( parseJSON )
                                 , ToJSON( toJSON )
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

data Field = Field
  { _short :: Bool
  , _title :: Text
  , _value :: Text
  } deriving (Data, Eq, Generic, Ord, Read, Show)

makeLenses ''Field

instance Arbitrary Field where
  arbitrary = Field
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance FromJSON Field where
  parseJSON = genericParseJSON fieldOptions

instance ToJSON Field where
  toJSON = genericToJSON fieldOptions

fieldOptions :: Options
fieldOptions = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 1
  , omitNothingFields  = True
  }
