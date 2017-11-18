{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module Taut.Types.TeamName
       ( TeamName
       , teamName
       , fromText
       , toText
       ) where

import Control.Lens              ( Iso'
                                 , iso
                                 )
import Data.Aeson.TH             ( defaultOptions
                                 , deriveJSON
                                 )
import Data.DeriveTH             ( derive
                                 , makeArbitrary
                                 )
import Infinity
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )
import Test.QuickCheck.Instances ()

newtype TeamName = TeamName Text
  deriving (Eq, Generic, Ord, Read, Show)

fromText :: Text -> TeamName
fromText = TeamName

toText :: TeamName -> Text
toText (TeamName n) = n

teamName :: Iso' TeamName Text
teamName = iso toText fromText

$(deriveJSON defaultOptions ''TeamName)

derive makeArbitrary ''TeamName
