{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Taut.Types.TeamId
     ( TeamId
     , fromText
     , tidText
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

newtype TeamId = TeamId { unTeamId :: Text }
  deriving (Eq, FromJSON, FromJSONKey, Generic, Ord, Read, Show, ToJSON, ToJSONKey)

instance ToField TeamId where
  toField = encodeUtf8 . unTeamId

fromText :: Text -> TeamId
fromText = TeamId

tidText :: Iso' TeamId Text
tidText = iso unTeamId fromText

derive makeArbitrary ''TeamId
