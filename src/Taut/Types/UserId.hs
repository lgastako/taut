{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.UserId
       ( UserId
       , fromText
       , toText
       , uidText
       ) where

import Control.Lens       ( Iso'
                          , iso
                          )
import Data.Aeson.TH      ( defaultOptions
                          , deriveJSON
                          )
import Data.Csv           ( ToField
                          , toField
                          )
import Data.Text.Encoding ( encodeUtf8 )
import Infinity

newtype UserId = UserId Text
  deriving (Eq, Ord, Read, Show)

instance ToField UserId where
  toField = encodeUtf8 . toText

fromText :: Text -> UserId
fromText = UserId

toText :: UserId -> Text
toText (UserId u) = u

uidText :: Iso' UserId Text
uidText = iso toText fromText

$(deriveJSON defaultOptions ''UserId)
