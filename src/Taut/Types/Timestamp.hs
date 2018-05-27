{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Taut.Types.Timestamp
     ( Timestamp
     , fromSlackTimeText
     , fromUTCTime
     , slackTimeText
     , toSlackTimeText
     , unTimestamp
     , utcTime
     ) where

import qualified Prelude                   as P
import           Taut.Prelude                      hiding ( decodeUtf8 )

import           Control.Lens                             ( Iso'
                                                          , iso
                                                          )
import           Data.Aeson                               ( encode )
import           Data.Csv                                 ( ToField
                                                          , toField
                                                          )
import           Data.Default                             ( Default( def ) )
import           Data.DeriveTH                            ( derive
                                                          , makeArbitrary
                                                          )
import qualified Data.Text                 as Text
import           Data.Text.Lazy.Encoding                  ( decodeUtf8 )
import           Data.Time.Clock                          ( UTCTime )
import           Data.Time.Clock.POSIX                    ( POSIXTime
                                                          , posixSecondsToUTCTime
                                                          , utcTimeToPOSIXSeconds
                                                          )
import           Test.QuickCheck                          ( Arbitrary
                                                          , arbitrary
                                                          )
import           Test.QuickCheck.Instances                ()
import           Text.Printf                              ( printf )
import           Web.HttpApiData                          ( ToHttpApiData
                                                          , toQueryParam
                                                          )

newtype Timestamp = Timestamp { unTimestamp :: UTCTime }
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance ToField Timestamp where
  toField = encodeUtf8 . toSlackTimeText

instance Default Timestamp where
  def = fromSlackTimeText "0"

instance ToHttpApiData Timestamp where
  toQueryParam = toStrict . decodeUtf8 . encode

fromUTCTime :: UTCTime -> Timestamp
fromUTCTime = Timestamp

fromSlackTimeText :: Text -> Timestamp
fromSlackTimeText = Timestamp
  . posixSecondsToUTCTime
  . realToFrac
  . (P.read :: P.String -> Double)
  . Text.unpack

toSlackTimeText :: Timestamp -> Text
toSlackTimeText = Text.pack
  . printf "%0.0f"
  . (realToFrac :: POSIXTime -> Double)
  . utcTimeToPOSIXSeconds
  . unTimestamp

utcTime :: Iso' Timestamp UTCTime
utcTime = iso unTimestamp fromUTCTime

slackTimeText :: Iso' Timestamp Text
slackTimeText = iso toSlackTimeText fromSlackTimeText

derive makeArbitrary ''Timestamp
