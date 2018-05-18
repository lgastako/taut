{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.UserName
       ( UserName
       , fromText
       , toText
       , userName
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

newtype UserName = UserName Text
  deriving (Eq, Generic, Ord, Read, Show)

instance FromHttpApiData UserName where
  parseQueryParam = Right . fromText

instance ToHttpApiData UserName where
  toQueryParam = toText

fromText :: Text -> UserName
fromText = UserName

toText :: UserName -> Text
toText (UserName u) = u

userName :: Iso' UserName Text
userName = iso toText fromText

$(deriveJSON defaultOptions ''UserName)

derive makeArbitrary ''UserName
