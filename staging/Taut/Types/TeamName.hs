{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Taut.Types.TeamName
     ( TeamName
     , fromText
     , teamName
     , unTeamName
     ) where

import Taut.Prelude

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
import Web.HttpApiData           ( ToHttpApiData
                                 , toQueryParam
                                 )

newtype TeamName = TeamName { unTeamName :: Text }
  deriving (Eq, Generic, Ord, Read, Show)

instance ToHttpApiData TeamName where
  toQueryParam = unTeamName

fromText :: Text -> TeamName
fromText = TeamName

teamName :: Iso' TeamName Text
teamName = iso unTeamName fromText

$(deriveJSON defaultOptions ''TeamName)

derive makeArbitrary ''TeamName
