{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module Taut.Types.UserName
       ( UserName
       , fromText
       , toText
       , userName
       ) where

import Control.Lens  ( Iso'
                     , iso
                     )
import Data.Aeson.TH ( defaultOptions
                     , deriveJSON
                     )
import Data.Text     ( Text )
import Infinity

newtype UserName = UserName Text
  deriving (Eq, Generic, Ord, Read, Show)

fromText :: Text -> UserName
fromText = UserName

toText :: UserName -> Text
toText (UserName u) = u

userName :: Iso' UserName Text
userName = iso toText fromText

$(deriveJSON defaultOptions ''UserName)
