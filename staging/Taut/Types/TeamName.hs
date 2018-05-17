{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.TeamName
       ( TeamName
       , fromText
       , teamName
       , toText
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
import           Web.HttpApiData                                       ( ToHttpApiData
                                                                       , toQueryParam
                                                                       )

newtype TeamName = TeamName Text
  deriving (Eq, Generic, Ord, Read, Show)

instance ToHttpApiData TeamName where
  toQueryParam = toText

fromText :: Text -> TeamName
fromText = TeamName

toText :: TeamName -> Text
toText (TeamName n) = n

teamName :: Iso' TeamName Text
teamName = iso toText fromText

$(deriveJSON defaultOptions ''TeamName)

derive makeArbitrary ''TeamName
