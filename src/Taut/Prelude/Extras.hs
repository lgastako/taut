{-# LANGUAGE NoImplicitPrelude #-}

module Taut.Prelude.Extras
     ( eitherToMaybe
     , StartsWith( startsWith )
     ) where

import qualified Data.Text as Text
import           Protolude

class StartsWith a where
  startsWith :: a -> a -> Bool

instance StartsWith Text where
  startsWith = flip Text.isPrefixOf

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
