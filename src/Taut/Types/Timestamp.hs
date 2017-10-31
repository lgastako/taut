{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Timestamp
       ( Timestamp
       , fromUTCTime
       , fromSlackTimeText
       , slackTimeText
       , toSlackTimeText
       , toUTCTime
       , utcTime
       ) where

import           Control.Lens                      ( Iso'
                                                   , iso
                                                   )
import           Data.Aeson                        ()
import           Data.Aeson.TH                     ( defaultOptions
                                                   , deriveJSON
                                                   )
import           Data.Csv                          ( ToField
                                                   , toField
                                                   )
import           Data.Default                      ( Default( def ) )
import           Data.DeriveTH                     ( derive
                                                   , makeArbitrary
                                                   )
import           Data.Text                         ( Text )
import qualified Data.Text                 as Text
import           Data.Text.Encoding                ( encodeUtf8 )
import           Data.Time.Clock                   ( UTCTime )
import           Data.Time.Clock.POSIX             ( posixSecondsToUTCTime
                                                   , utcTimeToPOSIXSeconds
                                                   )
import           GHC.Generics                      ( Generic )
import           Test.QuickCheck                   ( Arbitrary
                                                   , arbitrary
                                                   )
import           Test.QuickCheck.Instances         ()
import           Text.Printf                       ( printf )

newtype Timestamp = Timestamp UTCTime
  deriving (Eq, Generic, Ord, Read, Show)

instance ToField Timestamp where
  toField = encodeUtf8 . toSlackTimeText

instance Default Timestamp where
  def = fromSlackTimeText "0"

fromUTCTime :: UTCTime -> Timestamp
fromUTCTime = Timestamp

fromSlackTimeText :: Text -> Timestamp
fromSlackTimeText = Timestamp
  . posixSecondsToUTCTime
  . realToFrac
  . (read :: String -> Double)
  . Text.unpack

toSlackTimeText :: Timestamp -> Text
toSlackTimeText (Timestamp t) = Text.pack . printf "%0.0f" $ d
  -- TODO: is this right?
  where
    d :: Double
    d = realToFrac . utcTimeToPOSIXSeconds $ t

toUTCTime :: Timestamp -> UTCTime
toUTCTime (Timestamp t) = t

utcTime :: Iso' Timestamp UTCTime
utcTime = iso toUTCTime fromUTCTime

slackTimeText :: Iso' Timestamp Text
slackTimeText = iso toSlackTimeText fromSlackTimeText

$(deriveJSON defaultOptions ''Timestamp)

derive makeArbitrary ''Timestamp
