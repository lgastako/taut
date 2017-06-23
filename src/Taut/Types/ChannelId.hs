module Taut.Types.ChannelId
       ( ChannelId
       , make
       ) where

import Data.Text ( Text )

newtype ChannelId = ChannelId Text
  deriving (Eq, Ord, Read, Show)

make :: Text -> ChannelId
make = ChannelId
