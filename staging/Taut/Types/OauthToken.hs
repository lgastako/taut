{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Taut.Types.OauthToken
     ( OauthToken
     , fromText
     , tidText
     , toText
     , unOauthToken
     ) where

import Taut.Prelude

import Control.Lens              ( Iso'
                                 , iso
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
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance ToField OauthToken where
  toField = encodeUtf8 . unOauthToken

fromText :: Text -> OauthToken
fromText = OauthToken

toText :: OauthToken -> Text
toText = unOauthToken

tidText :: Iso' OauthToken Text
tidText = iso unOauthToken fromText

derive makeArbitrary ''OauthToken
