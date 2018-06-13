{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TemplateHaskell            #-}

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

newtype UserName = UserName { unUserName :: Text }
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance FromHttpApiData UserName where
  parseQueryParam = Right . fromText

instance ToHttpApiData UserName where
  toQueryParam = unUserName

fromText :: Text -> UserName
fromText = UserName

userName :: Iso' UserName Text
userName = iso unUserName fromText

derive makeArbitrary ''UserName
