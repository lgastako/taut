module Taut.Types.User.Profile
       ( Profile(Profile)
       ) where

import Taut.Types.ApiAppId   ( ApiAppId )
import Taut.Types.AvatarHash ( AvatarHash )
import Taut.Types.Title      ( Title )
import Taut.Types.UrlText    ( UrlText )
import Taut.Types.UserId     ( UserId )

data Profile = Profile
  { _botId         :: UserId
  , _apiAppId      :: ApiAppId
  , _alwaysActive  :: Bool
  , _avatarHash    :: AvatarHash
  , _image24       :: UrlText
  , _image32       :: UrlText
  , _image48       :: UrlText
  , _image72       :: UrlText
  , _image192      :: UrlText
  , _image512      :: UrlText
  , _image1024     :: UrlText
  , _imageOriginal :: UrlText
  , _title         :: Title
  -- , _fields        :: ??
  } deriving (Eq, Ord, Read, Show)

-- "profile": {
--     "bot_id": "B5E6PQEQG",
--     "api_app_id": "",
--     "always_active": false,
--     "avatar_hash": "20767bcdef85",
--     "image_24": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_24.jpg",
--     "image_32": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_32.jpg",
--     "image_48": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_48.jpg",
--     "image_72": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_72.jpg",
--     "image_192": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_192.jpg",
--     "image_512": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_512.jpg",
--     "image_1024": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_1024.jpg",
--     "image_original": "https:\/\/avatars.slack-edge.com\/2017-05-17\/184328807137_20767bcdef85ba519b70_original.jpg",
--     "title": "I came to get down.",
--     "real_name": "",
--     "real_name_normalized": "",
--     "fields": null
-- },
