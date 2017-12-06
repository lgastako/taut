{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Taut.Types.AccessToken
       ( AccessToken
       ) where

import Data.Validation ( AccValidation )
import Focus.Prelude

data AccessToken = AccessToken Text
  deriving (Eq, Generic, Ord, Read, Show)

fromTextE :: Text -> AccValidation Text AccessToken
fromTextE s = notImplemented "fromTextE"
