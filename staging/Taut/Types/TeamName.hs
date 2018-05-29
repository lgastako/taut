{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

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

newtype TeamName = TeamName { unTeamName :: Text }
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance FromHttpApiData TeamName where
  parseQueryParam = Right . fromText

instance ToHttpApiData TeamName where
  toQueryParam = unTeamName

fromText :: Text -> TeamName
fromText = TeamName

teamName :: Iso' TeamName Text
teamName = iso unTeamName fromText

derive makeArbitrary ''TeamName
