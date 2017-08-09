module Taut.Types.UrlText
       ( UrlText(UrlText)
       ) where

import Data.Text ( Text )

newtype UrlText = UrlText Text
  deriving (Eq, Ord, Read, Show)
