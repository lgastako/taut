{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.UserId
       ( UserId
       , empty
       , make
       ) where

import Data.Aeson.TH ( defaultOptions
                     , deriveJSON
                     )
import Infinity

newtype UserId = UserId Text
  deriving (Eq, Ord, Read, Show)

instance Monoid UserId where
  mempty = empty
  (UserId a) `mappend` (UserId b) = UserId (a `mappend` b)

make :: Text -> UserId
make = UserId

empty :: UserId
empty = make "UD3ADB33F"

$(deriveJSON defaultOptions ''UserId)
