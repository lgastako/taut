{-# LANGUAGE DeriveAnyClass #-}
module Taut.Types.ChannelId
       ( ChannelId
       , empty
       , make
       ) where

import Data.Text ( Text )

newtype ChannelId = ChannelId Text
  deriving (Eq, Ord, Read, Show)

instance Monoid ChannelId where
  mempty = empty
  (ChannelId a) `mappend` (ChannelId b) = ChannelId (a `mappend` b)

make :: Text -> ChannelId
make = ChannelId

empty :: ChannelId
empty = ChannelId "CH0PST1CK"
