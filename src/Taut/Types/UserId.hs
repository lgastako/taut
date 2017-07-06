{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.UserId
       ( UserId
       , make
       , toText
       ) where

import Data.Aeson.TH ( defaultOptions
                     , deriveJSON
                     )
import Infinity

newtype UserId = UserId Text
  deriving (Eq, Ord, Read, Show)

make :: Text -> UserId
make = UserId

toText :: UserId -> Text
toText (UserId u) = u

$(deriveJSON defaultOptions ''UserId)
