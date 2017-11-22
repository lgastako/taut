{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.OauthToken
       ( OauthToken
       , fromText
       , tidText
       , toText
       ) where

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
import Data.Text.Encoding        ( encodeUtf8 )
import GHC.Generics              ( Generic )
import Infinity
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )
import Test.QuickCheck.Instances ()

newtype OauthToken = OauthToken Text
  deriving (Eq, Ord, Read, Show, Generic)

instance ToField OauthToken where
  toField = encodeUtf8 . toText

fromText :: Text -> OauthToken
fromText = OauthToken

toText :: OauthToken -> Text
toText (OauthToken u) = u

tidText :: Iso' OauthToken Text
tidText = iso toText fromText

$(deriveJSON defaultOptions ''OauthToken)

derive makeArbitrary ''OauthToken
