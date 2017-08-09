module Taut.Types.RealNameNormalized
       ( RealNameNormalized(RealNameNormalized)
       ) where

import Data.Text ( Text )

newtype RealNameNormalized = RealNameNormalized Text
  deriving (Eq, Ord, Read, Show)
