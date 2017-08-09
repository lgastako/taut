module Taut.Types.ApiAppId
       ( ApiAppId(ApiAppId)
       ) where

import Data.Text ( Text )

newtype ApiAppId = ApiAppId Text
  deriving (Eq, Ord, Read, Show)
