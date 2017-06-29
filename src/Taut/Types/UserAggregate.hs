{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Taut.Types.UserAggregate
       ( UserAggregate
       ) where

import Infinity
import Taut.Types.UserId   ( UserId )
import Taut.Types.UserName ( UserName )

data UserAggregate a = UserAggregate UserId UserName a
  deriving (Eq, Functor, Generic, Ord, Read, Show)
