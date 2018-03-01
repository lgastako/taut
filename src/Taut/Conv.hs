{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Taut.Conv where

import           Control.Lens                       ( (^.)
                                                    , view
                                                    )
import           Data.Text                          ( Text
                                                    , pack
                                                    , unpack
                                                    )
import           Data.Time.Clock.POSIX              ( POSIXTime
                                                    , posixSecondsToUTCTime
                                                    )
import           Taut.Types.BotEvent                ( BotEvent( Message
                                                              , OtherEvent
                                                              )
                                                    )
import           Taut.Types.ChannelId               ( ChannelId )
import qualified Taut.Types.ChannelId  as ChannelId
import           Taut.Types.EditInfo                ( EditInfo )
import qualified Taut.Types.EditInfo   as EditInfo
import           Taut.Types.Timestamp               ( Timestamp
                                                    , fromUTCTime
                                                    )
import           Taut.Types.UserId                  ( UserId )
import qualified Taut.Types.UserId     as UserId
import qualified Web.Slack             as WS
import qualified Web.Slack.Api.Channel as WSAC

class Conv a b where
  conv :: a -> b

instance Conv WS.ChannelId ChannelId where
  conv = ChannelId.fromText . view WS.getId

instance Conv WS.Submitter UserId where
  conv = UserId.fromText . f
    where
      f (WS.BotComment  bid) = view WS.getId bid
      f (WS.UserComment uid) = view WS.getId uid
      f WS.System            = error "cannot conv WS.System"

instance Conv WS.SlackTimeStamp Timestamp where
  conv = conv . view WS.slackTime

instance Conv WS.Time Timestamp where
  conv = conv . view WS.getTime

instance Conv POSIXTime Timestamp where
  conv = fromUTCTime . posixSecondsToUTCTime

instance Conv WS.Edited EditInfo where
  conv wsEdited = EditInfo.from ( conv $ wsEdited ^. WS.editTimestap
                                , conv $ wsEdited ^. WS.editedUser
                                )

instance Conv WS.Subtype Text where
  conv (WS.SBotMessage {})     = "bot_message"
  conv (WS.SMessageChanged {}) = "message_changed"
  conv WS.SMeMessage           = "me_message"
  conv (WS.SChannelJoin {})    = "channel_join"
  conv (WS.SMessageDeleted {}) = "message_deleted"
  conv WS.SChannelLeave        = "channel_leave"
  conv (WS.SChannelTopic _)    = "channel_topic"
  conv (WS.SChannelPurpose _)  = "channel_purpose"
  conv (WS.SChannelName _ _)   = "channel_name"
  conv (WS.SChannelArchive _)  = "channel_archive"
  conv WS.SChannelUnarchive    = "channel_unarchive"
  conv (WS.SGroupJoin _)       = "group_join"
  conv WS.SGroupLeave          = "group_leave"
  conv (WS.SGroupTopic _)      = "group_topic"
  conv (WS.SGroupPurpose _)    = "group_purpose"
  conv (WS.SGroupName _ _)     = "group_name"
  conv (WS.SGroupArchive _)    = "group_archive"
  conv WS.SGroupUnarchive      = "group_unarchive"
  conv (WS.SFileShare _ _)     = "file_share"
  conv (WS.SFileComment _ _)   = "file_comment"
  conv (WS.SFileMention _)     = "file_mention"
  conv WS.SPinnedItem          = "pinned_item"
  conv WS.SUnpinnedItem        = "unpinned_item"

instance Conv WS.Event BotEvent where
  conv (WS.Message chan sub text slackTs subTypeMay editedMay)
    = Message cid uid text ts subTypeMay' editInfoMay
    where
      cid :: ChannelId
      cid = conv chan

      uid :: UserId
      uid = conv sub

      ts :: Timestamp
      ts = conv slackTs

      subTypeMay' :: Maybe Text
      subTypeMay' = conv <$> subTypeMay

      editInfoMay :: Maybe EditInfo
      editInfoMay = conv <$> editedMay
  conv _ = OtherEvent

instance Conv WS.UserId UserId where
  conv = UserId.fromText . view WS.getId

instance Conv UserId String where
  conv = unpack . UserId.toText

instance Conv WSAC.Channel ChannelId where
  conv = ChannelId.fromText . pack . WSAC.channelId
