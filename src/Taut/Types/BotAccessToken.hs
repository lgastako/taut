{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Taut.Types.BotAccessToken
       ( BotAccessToken
       ) where

import Focus.Prelude

data BotAccessToken = BotAccessToken Text
  deriving (Eq, Generic, Ord, Read, Show)
