{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Taut.Types.OauthToken
     ( OauthToken
     , fromText
     , tidText
     , unOauthToken
     ) where

import Taut.Prelude

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
import Test.QuickCheck
import Test.QuickCheck.Instances ()

newtype OauthToken = OauthToken { unOauthToken :: Text }
  deriving (Eq, Ord, Read, Show, Generic)

instance ToField OauthToken where
  toField = encodeUtf8 . unOauthToken

fromText :: Text -> OauthToken
fromText = OauthToken

tidText :: Iso' OauthToken Text
tidText = iso unOauthToken fromText

$(deriveJSON defaultOptions ''OauthToken)

derive makeArbitrary ''OauthToken
