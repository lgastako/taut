{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Timestamp
       ( Timestamp
       , empty
       , make
       , fromSlackText
       ) where

import           Data.Aeson                    ()
import           Data.Aeson.TH                 ( defaultOptions
                                               , deriveJSON
                                               )
import           Data.Text                     ( Text )
import qualified Data.Text             as Text
import           Data.Time.Clock               ( UTCTime )
import           Data.Time.Clock.POSIX         ( posixSecondsToUTCTime )

newtype Timestamp = Timestamp UTCTime
  deriving (Eq, Ord, Read, Show)

make :: UTCTime -> Timestamp
make = Timestamp

fromSlackText :: Text -> Timestamp
fromSlackText = Timestamp
  . posixSecondsToUTCTime
  . realToFrac
  . (read :: String -> Double)
  . Text.unpack

empty :: Timestamp
empty = fromSlackText "0"

$(deriveJSON defaultOptions ''Timestamp)
