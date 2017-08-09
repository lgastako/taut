module Taut.Types.AvatarHash
       ( AvatarHash(AvatarHash)
       ) where

import Data.Text ( Text )

newtype AvatarHash = AvatarHash Text
  deriving (Eq, Ord, Read, Show)
