module Taut.Types.UserDetail
       ( UserDetail
       ) where

import Data.Text               ( Text )
import Taut.Types.RealName     ( RealName )
import Taut.Types.TeamId       ( TeamId )
import Taut.Types.Timestamp    ( Timestamp )
import Taut.Types.User.Profile ( Profile )
import Taut.Types.UserId       ( UserId )
import Taut.Types.UserName     ( UserName )

data UserDetail = UserDetail
  { _userId            :: UserId
  , _teamId            :: TeamId
  , _name              :: UserName
  , _deleted           :: Bool
  , _color             :: Color
  , _realName          :: RealName
  , _tz                :: Maybe Text
  , _tzLabel           :: Text
  , _tzOffset          :: Int
  , _profile           :: Profile
  , _isAdmin           :: Bool
  , _isOwner           :: Bool
  , _isPrimaryOwner    :: Bool
  , _isRestricted      :: Bool
  , _isUltraRestricted :: Bool
  , _isBot             :: Bool
  , _updated           :: Timestamp
  } deriving (Eq, Ord, Read, Show)

type Color    = Text -- TODO

-- {
--     "id": "U5ESBUQV8",
--     "team_id": "T0LPTS12P",
--     "name": "alonzo",
--     "deleted": false,
--     "color": "d55aef",
--     "real_name": "",
--     "tz": null,
--     "tz_label": "Pacific Daylight Time",
--     "tz_offset": -25200,
--     "profile": {
--         "bot_id": "B5E6PQEQG",
--         "api_app_id": "",
--         "always_active": false,
--         "avatar_hash": "20767bcdef85",
--         "image_24": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_24.jpg",
--         "image_32": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_32.jpg",
--         "image_48": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_48.jpg",
--         "image_72": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_72.jpg",
--         "image_192": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_192.jpg",
--         "image_512": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_512.jpg",
--         "image_1024": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_1024.jpg",
--         "image_original": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_original.jpg",
--         "title": "I came to get down.",
--         "real_name": "",
--         "real_name_normalized": "",
--         "fields": null
--     },
--     "is_admin": false,
--     "is_owner": false,
--     "is_primary_owner": false,
--     "is_restricted": false,
--     "is_ultra_restricted": false,
--     "is_bot": true,
--     "updated": 1495061822
-- },
