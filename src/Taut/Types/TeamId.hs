{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Taut.Types.TeamId
     ( TeamId
     , fromText
     , tidText
     , toText
     , unTeamId
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
import Web.HttpApiData           ( FromHttpApiData
                                 , ToHttpApiData
                                 , parseQueryParam
                                 , toQueryParam
                                 )

newtype TeamId = TeamId { unTeamId :: Text }
  deriving (Data, Eq, FromJSON, FromJSONKey, Generic, Ord, Read, Show, ToJSON, ToJSONKey)

instance ToField TeamId where
  toField = encodeUtf8 . unTeamId

instance FromHttpApiData TeamId where
  parseQueryParam = Right . fromText

instance ToHttpApiData TeamId where
  toQueryParam = unTeamId

fromText :: Text -> TeamId
fromText = TeamId

toText :: TeamId -> Text
toText = unTeamId

tidText :: Iso' TeamId Text
tidText = iso unTeamId fromText

derive makeArbitrary ''TeamId
