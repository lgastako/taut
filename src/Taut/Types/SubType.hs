{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.SubType
       ( SubType
       , empty
       , make
       , null
       ) where

import Prelude       hiding ( null )

import Data.Aeson.TH        ( defaultOptions
                            , deriveJSON
                            )
import Infinity

newtype SubType = SubType Text
  deriving (Eq, Ord, Read, Show)

make :: Text -> SubType
make = SubType

empty :: SubType
empty = make ""

null :: SubType -> Bool
null subType
  | subType == empty = True
  | otherwise        = False

$(deriveJSON defaultOptions ''SubType)
