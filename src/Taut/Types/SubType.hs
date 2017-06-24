module Taut.Types.SubType
       ( SubType
       , empty
       , make
       ) where

import Data.Text ( Text )

newtype SubType = SubType Text
  deriving (Eq, Ord, Read, Show)

make :: Text -> SubType
make = SubType

empty :: SubType
empty = make ""
