{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Taut.Types.UserSet
       ( UserSet
       , empty
       , evince
       ) where

import           Control.Lens                            ( (^.) )
import qualified Data.Map                 as Map
import           Infinity
import           Taut.Types.MessageEvent                 ( MessageEvent
                                                         , subType
                                                         , type_
                                                         )
import qualified Taut.Types.MessageType   as MessageType
import qualified Taut.Types.SubType       as SubType
import           Taut.Types.UserAggregate                ( UserAggregate )
import           Taut.Types.UserId                       ( UserId )

data UserSet a = UserSet (Map UserId (UserAggregate a))
  deriving (Eq, Functor, Generic, Ord, Read, Show)

empty :: UserSet a
empty = UserSet Map.empty

evince :: [MessageEvent a] -> UserSet b
evince = foldr add empty
  where
    add e us
      |    e ^. type_   == MessageType.message
        && e ^. subType == SubType.empty
                  = insertAll us . extractUsers $ e
      | otherwise = us

    insertAll    = undefined

    extractUsers = undefined
