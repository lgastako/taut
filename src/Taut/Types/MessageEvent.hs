{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
module Taut.Types.MessageEvent
       ( MessageEvent
       , channel
       , edited
       , empty
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
       , user
       ) where

import           Control.Lens                          ( (&)
                                                       , (.~)
                                                       , (??)
                                                       , DefName( TopName )
                                                       , lensField
                                                       , lensRules
                                                       , makeLensesWith
                                                       )
import           Language.Haskell.TH                   ( mkName
                                                       , nameBase
                                                       )
import           Taut.Types.ChannelId                  ( ChannelId )
import qualified Taut.Types.ChannelId   as ChannelId
import           Taut.Types.EditInfo                   ( EditInfo )
import           Taut.Types.MessageType                ( MessageType )
import qualified Taut.Types.MessageType as MessageType
import           Taut.Types.Reaction                   ( Reaction )
import           Taut.Types.SubType                    ( SubType )
import qualified Taut.Types.SubType     as SubType
import           Taut.Types.Timestamp                  ( Timestamp )
import qualified Taut.Types.Timestamp   as Timestamp
import           Taut.Types.UserId                     ( UserId )
import qualified Taut.Types.UserId      as UserId

data MessageEvent a = MessageEvent
  { _channel    :: ChannelId
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
  , _user       :: UserId
  } deriving (Functor, Foldable, Traversable)

deriving instance Eq   a => Eq   (MessageEvent a)
deriving instance Ord  a => Ord  (MessageEvent a)
deriving instance Read a => Read (MessageEvent a)
deriving instance Show a => Show (MessageEvent a)

makeLensesWith ?? ''MessageEvent $ lensRules
  & lensField .~ (\_ _ n -> let name = nameBase n
                            in [ TopName . mkName $ (case name of
                                                        "_type"       -> "type_"
                                                        "_event_ts"   -> "eventTs"
                                                        "_is_starred" -> "isStarred"
                                                        "_pinned_to"  -> "pinnedTo"
                                                        other -> drop 1 other) ])


empty :: MessageEvent ()
empty =
  make
    ChannelId.empty
    editInfoEmpty
    eventTsEmpty
    hiddenEmpty
    isStarredEmpty
    payloadEmpty
    pinnedEmpty
    reactionsEmpty
    SubType.empty
    Timestamp.empty
    MessageType.empty
    UserId.empty
  where
    editInfoEmpty  = Nothing
    eventTsEmpty   = Nothing
    hiddenEmpty    = Nothing
    isStarredEmpty = Nothing
    payloadEmpty   = ()
    pinnedEmpty    = Nothing
    reactionsEmpty = Nothing

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
