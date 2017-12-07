{-# LANGUAGE NoImplicitPrelude #-}
module Taut.Types.AccessToken
       ( AccessToken
       , accessTokenText
       ) where

import Focus.Prelude

class AccessToken a where
  accessTokenText :: a -> Text
