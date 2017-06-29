{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Taut.Types.ChannelSet
       ( ChannelSet
       , empty
       , evince
       ) where

import           Control.Lens                               ( (^.) )
import qualified Data.Map                    as Map
import           Infinity
import           Taut.Types.ChannelAggregate                ( ChannelAggregate )
import           Taut.Types.ChannelId                       ( ChannelId )
import           Taut.Types.MessageEvent                    ( MessageEvent
                                                            , subType
                                                            , type_
                                                            )
import qualified Taut.Types.MessageType      as MessageType
import qualified Taut.Types.SubType          as SubType

data ChannelSet a = ChannelSet (Map ChannelId (ChannelAggregate a))
  deriving (Eq, Functor, Generic, Ord, Read, Show)

empty :: ChannelSet a
empty = ChannelSet Map.empty

evince :: [MessageEvent a] -> ChannelSet b
evince = foldr add empty
  where
    add e us
      |    e ^. type_   == MessageType.message
        && e ^. subType == SubType.empty
                  = insertAll us . extractChannels $ e
      | otherwise = us

    insertAll    = undefined

    extractChannels = undefined
