{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Taut.Types.ChannelAggregate
       ( ChannelAggregate
       ) where

import Infinity
import Taut.Types.ChannelId   ( ChannelId )
import Taut.Types.ChannelName ( ChannelName )

data ChannelAggregate a = ChannelAggregate ChannelId ChannelName a
  deriving (Eq, Functor, Generic, Ord, Read, Show)
