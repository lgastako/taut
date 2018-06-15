{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Taut.Types.MessageEvent
     ( MessageEvent
     , channelId
     , edited
     , eventTs
     , hidden
     , isStarred
     , make
     , payload
     , pinnedTo
     , reactionCount
     , reactions
     , reactionsSummary
     , subType
     , ts
     , type_
     , userId
     ) where

import qualified Prelude                as P
import           Taut.Prelude                       hiding ( intercalate )

import           Control.Lens                              ( (??)
                                                           , DefName( TopName )
                                                           , lensField
                                                           , lensRules
                                                           , makeLensesWith
                                                           )
import           Data.Aeson                                ( genericParseJSON
                                                           , genericToJSON
                                                           )
import           Data.Aeson.TH                             ( defaultOptions
                                                           , fieldLabelModifier
                                                           )
import           Data.Csv                                  ( ToNamedRecord( toNamedRecord ) )
import qualified Data.Csv               as Csv
import           Data.DeriveTH                             ( derive
                                                           , makeArbitrary
                                                           )
import           Data.String                               ( String )
import           Data.Text                                 ( intercalate )
import           Language.Haskell.TH                       ( mkName
                                                           , nameBase
                                                           )
import           Taut.Types.ChannelId                      ( ChannelId
                                                           , unChannelId
                                                           )
import           Taut.Types.EditInfo                       ( EditInfo )
import           Taut.Types.MessageType                    ( MessageType )
import           Taut.Types.Reaction                       ( Reaction )
import qualified Taut.Types.Reaction    as Reaction
import           Taut.Types.SubType                        ( SubType
                                                           , unSubType
                                                           )
import           Taut.Types.Timestamp                      ( Timestamp )
import           Taut.Types.UserId                         ( UserId )
import           Test.QuickCheck                           ( Arbitrary
                                                           , arbitrary
                                                           )

data MessageEvent a = MessageEvent
  { _channelId  :: ChannelId
  , _edited     :: Maybe EditInfo
  , _eventTs    :: Maybe Timestamp
  , _hidden     :: Maybe Bool
  , _isStarred  :: Maybe Bool
  , _payload    :: a
  , _pinnedTo   :: Maybe [ChannelId]
  , _reactions  :: Maybe [Reaction]
  , _subType    :: SubType
  , _ts         :: Timestamp
  , _type       :: MessageType
  , _userId     :: UserId
  } deriving (Data, Eq, Functor, Foldable, Generic, Ord, Read, Show, Traversable)

makeLensesWith ?? ''MessageEvent $ lensRules
  & lensField .~ (\_ _ n -> let name = nameBase n
                            in [ TopName . mkName $ (case name of
                                                        "_type"       -> "type_"
                                                        "_event_ts"   -> "eventTs"
                                                        "_is_starred" -> "isStarred"
                                                        "_pinned_to"  -> "pinnedTo"
                                                        other         -> drop 1 other) ])

fieldPairs :: [(String, String)]
fieldPairs = [ ("_channelId", "channel")
             , ("_userId",    "user")
             ]

-- reverseFieldPairs :: [(String, String)]
-- reverseFieldPairs = [(b, a) | (a, b) <- fieldPairs]

fromField :: String -> String
fromField field = fromMaybe (drop 1 field) $ P.lookup field fieldPairs

toField :: String -> String
toField field = fromMaybe (drop 1 field) $ P.lookup field fieldPairs -- reverseFieldPairs

instance ToJSON (MessageEvent Text) where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = toField }

instance FromJSON (MessageEvent Text) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fromField }

instance ToNamedRecord (MessageEvent Text) where
  toNamedRecord (MessageEvent chanId _ _ _ _ payload' _ _ subType' ts' type_' userId') =
    Csv.namedRecord [ ("channelId", Csv.toField (unChannelId chanId))
                    , ("userId",    Csv.toField userId')
                    , ("payload",   Csv.toField payload')
                    , ("type",      Csv.toField type_')
                    , ("subType",   Csv.toField (unSubType subType'))
                    , ("ts",        Csv.toField ts')
                    ]

make :: ChannelId
     -> Maybe EditInfo
     -> Maybe Timestamp
     -> Maybe Bool
     -> Maybe Bool
     -> a
     -> Maybe [ChannelId]
     -> Maybe [Reaction]
     -> SubType
     -> Timestamp
     -> MessageType
     -> UserId
     -> MessageEvent a
make = MessageEvent

reactionCount :: MessageEvent a -> Int
reactionCount = sum . map (^. Reaction.count) . fromMaybe [] . (^. reactions)

reactionsSummary :: MessageEvent a -> Text
reactionsSummary =
  intercalate ", " . map Reaction.summary . fromMaybe [] . (^. reactions)

derive makeArbitrary ''MessageEvent
