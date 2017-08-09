{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.TeamId
       ( TeamId
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

newtype TeamId = TeamId Text
  deriving (Eq, Ord, Read, Show)

instance ToField TeamId where
  toField = encodeUtf8 . toText

fromText :: Text -> TeamId
fromText = TeamId

toText :: TeamId -> Text
toText (TeamId u) = u

uidText :: Iso' TeamId Text
uidText = iso toText fromText

$(deriveJSON defaultOptions ''TeamId)
