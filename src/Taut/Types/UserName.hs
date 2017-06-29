{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module Taut.Types.UserName
       ( UserName
       , make
       ) where

import Data.Aeson.TH ( defaultOptions
                     , deriveJSON
                     )
import Data.Text     ( Text )
import Infinity

newtype UserName = UserName Text
  deriving (Eq, Generic, Ord, Read, Show)

make :: Text -> UserName
make = UserName

$(deriveJSON defaultOptions ''UserName)
