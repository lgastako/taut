module Taut.Types.UserId
       ( UserId
       , make
       ) where

import Data.Text ( Text )

newtype UserId = UserId Text
  deriving (Eq, Ord, Read, Show)

make :: Text -> UserId
make = UserId
