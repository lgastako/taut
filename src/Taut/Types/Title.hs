module Taut.Types.Title
       ( Title(Title)
       ) where

import Data.Text ( Text )

newtype Title = Title Text
  deriving (Eq, Ord, Read, Show)
