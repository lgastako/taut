module Taut.Types.Timestamp
       ( Timestamp
       , make
       , fromSlackText
       ) where

import           Data.Text                     ( Text )
import qualified Data.Text             as Text
import           Data.Time.Clock               ( UTCTime )
import           Data.Time.Clock.POSIX         ( posixSecondsToUTCTime )

newtype Timestamp = Timestamp UTCTime
  deriving (Eq, Ord, Read, Show)

make :: UTCTime -> Timestamp
make = Timestamp

fromSlackText :: Text -> Timestamp
fromSlackText = Timestamp . posixSecondsToUTCTime . fromInteger . read . Text.unpack
