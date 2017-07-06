{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
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
       , reactions
       , subType
       , ts
       , type_
       , userId
       ) where

import           Prelude                             hiding ( null )

import           Control.Lens                               ( (&)
                                                            , (.~)
                                                            , (??)
                                                            , DefName( TopName )
                                                            , lensField
                                                            , lensRules
                                                            , makeLensesWith
                                                            )
import           Data.Aeson                                 ( FromJSON
                                                            , ToJSON
                                                            , genericParseJSON
                                                            , genericToJSON
                                                            , parseJSON
                                                            , toJSON
                                                            )
import           Data.Aeson.TH                              ( defaultOptions
                                                            , fieldLabelModifier
                                                            )
import           Data.Csv                                   ( ToRecord( toRecord ) )
import qualified Data.Csv               as Csv
import           Infinity                            hiding ( error )
import           Language.Haskell.TH                        ( mkName
                                                            , nameBase
                                                            )
import           Taut.Types.ChannelId                       ( ChannelId )
import qualified Taut.Types.ChannelId   as ChannelId
import           Taut.Types.EditInfo                        ( EditInfo )
import           Taut.Types.MessageType                     ( MessageType )
import           Taut.Types.Reaction                        ( Reaction )
import           Taut.Types.SubType                         ( SubType )
import qualified Taut.Types.SubType     as SubType
import           Taut.Types.Timestamp                       ( Timestamp )
import           Taut.Types.UserId                          ( UserId )

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
  } deriving (Functor, Foldable, Generic, Traversable)

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

reverseFieldPairs :: [(String, String)]
reverseFieldPairs = [(b, a) | (a, b) <- fieldPairs]

fromField :: String -> String
fromField field = fromMaybe (drop 1 field) $ lookup field fieldPairs

toField :: String -> String
toField field = fromMaybe ("_" ++ field) $ lookup field reverseFieldPairs

instance ToJSON (MessageEvent Text) where
  toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = toField }

instance FromJSON (MessageEvent Text) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = fromField }

deriving instance Eq   a => Eq   (MessageEvent a)
deriving instance Ord  a => Ord  (MessageEvent a)
deriving instance Read a => Read (MessageEvent a)
deriving instance Show a => Show (MessageEvent a)

instance ToRecord (MessageEvent Text) where
  toRecord (MessageEvent chanId _ _ _ _ payload _ _ subType ts type_ userId) =
    Csv.record [ Csv.toField (ChannelId.toText chanId)
               , Csv.toField payload
               , Csv.toField (SubType.toText subType)
               , Csv.toField ts
               , Csv.toField type_
               , Csv.toField userId
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
