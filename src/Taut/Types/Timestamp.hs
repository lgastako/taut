{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.Timestamp
       ( Timestamp
       , fromUTCTime
       , fromSlackTimeText
       , slackTimeText
       , toSlackTimeText
       , toUTCTime
       , utcTime
       ) where

import qualified Prelude                   as P
import           Focus.Prelude                       hiding ( decodeUtf8 )

import           Control.Lens                               ( Iso'
                                                            , iso
                                                            )
import           Data.Aeson                                 ( eitherDecode
                                                            , encode
                                                            )
import           Data.Aeson.TH                              ( defaultOptions
                                                            , deriveJSON
                                                            )
import           Data.Csv                                   ( ToField
                                                            , toField
                                                            )
import           Data.Default                               ( Default( def ) )
import           Data.DeriveTH                              ( derive
                                                            , makeArbitrary
                                                            )
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import           Data.Text.Lazy.Encoding                    ( decodeUtf8 )
import qualified Data.Text.Lazy.Encoding   as LTextE
import           Data.Time.Clock                            ( UTCTime )
import           Data.Time.Clock.POSIX                      ( posixSecondsToUTCTime
                                                            , utcTimeToPOSIXSeconds
                                                            )
import           Test.QuickCheck                            ( Arbitrary
                                                            , arbitrary
                                                            )
import           Test.QuickCheck.Instances                  ()
import           Text.Printf                                ( printf )
import           Web.HttpApiData                            ( FromHttpApiData
                                                            , ToHttpApiData
                                                            , parseQueryParam
                                                            , toQueryParam
                                                            )

newtype Timestamp = Timestamp UTCTime
  deriving (Eq, Generic, Ord, Read, Show)

instance FromHttpApiData Timestamp where
  parseQueryParam = first Text.pack . eitherDecode . LTextE.encodeUtf8 . LText.fromStrict

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
  . (P.read :: String -> Double)
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
