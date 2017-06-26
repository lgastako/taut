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
import           Data.Time.Calendar            ( Day )
import           Data.Time.Clock               ( DiffTime
                                               , UTCTime( UTCTime )
                                               )
import           Data.Time.Clock.POSIX         ( posixSecondsToUTCTime )

newtype Timestamp = Timestamp UTCTime
  deriving (Eq, Ord, Read, Show)

instance Monoid Timestamp where
  mempty = empty
  (Timestamp (UTCTime da sa)) `mappend` (Timestamp (UTCTime db sb)) =
    Timestamp (UTCTime dc se)
    where
      dc = addDays da db
      sc = addSecs sa sb
      (ed, sd) = if sc > daysWorthOfSeconds
                   then (1 :: Int, sc - daysWorthOfSeconds)
                   else (0, sc)
      se = something sd ed
      daysWorthOfSeconds = 24 * 60 * 60 -- TODO: nominalDay

      addDays :: Day -> Day -> Day
      addDays = error "Timestamp Monoid addDays not implemented"

      addSecs :: DiffTime -> DiffTime -> DiffTime
      addSecs = error "Timestamp Monoid addSecs not implemented"

      something :: _a -> _b -> _c
      something = error "Timestamp Monoid something not implemented"


make :: UTCTime -> Timestamp
make = Timestamp

fromSlackText :: Text -> Timestamp
fromSlackText = Timestamp . posixSecondsToUTCTime . fromInteger . read . Text.unpack

empty :: Timestamp
empty = fromSlackText "0"

$(deriveJSON defaultOptions ''Timestamp)
