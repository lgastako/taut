{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Taut.Types.UserName
     ( UserName
     , fromText
     , unUserName
     , userName
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

newtype UserName = UserName { unUserName :: Text }
  deriving (Eq, Generic, Ord, Read, Show)

instance ToHttpApiData UserName where
  toQueryParam = unUserName

fromText :: Text -> UserName
fromText = UserName

userName :: Iso' UserName Text
userName = iso unUserName fromText

$(deriveJSON defaultOptions ''UserName)

derive makeArbitrary ''UserName
