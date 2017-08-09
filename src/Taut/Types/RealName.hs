module Taut.Types.RealName
       ( RealName
       ) where

import Data.Text ( Text )

newtype RealName = RealName Text
  deriving (Eq, Ord, Read, Show)
