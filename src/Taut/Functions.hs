module Taut.Functions
       ( byChannel
       , chronoByChan
       , chronological
       , userReplyDocs
       , userReplyDocsWith
       , userReplyWindows
       , replyWindows
       ) where

import           Control.Lens                    ( (^.) )
import           Control.Monad                   ( guard )
import           Data.List                       ( foldl'
                                                 , inits
                                                 , sortOn
                                                 )
import           Data.Map.Strict                 ( Map )
import qualified Data.Map.Strict         as Map
import           Data.Maybe                      ( mapMaybe )
import           Data.Text                       ( Text )
import qualified Data.Text               as Text
import           Taut.Types.ChannelId            ( ChannelId )
import           Taut.Types.MessageEvent         ( MessageEvent
                                                 , channelId
                                                 , payload
                                                 , ts
                                                 , userId
                                                 )
import           Taut.Types.UserId               ( UserId )

byChannel :: [MessageEvent a] -> Map ChannelId [MessageEvent a]
byChannel = foldr absorb Map.empty
  where
    absorb msg = Map.insertWith (++) (msg ^. channelId) [msg]

chronological :: [MessageEvent a] -> [MessageEvent a]
chronological = sortOn (^. ts)

chronoByChan :: [MessageEvent a] -> Map ChannelId [MessageEvent a]
chronoByChan = Map.map chronological . byChannel

replyWindows :: (Eq a, Ord a) =>
                Int -> [MessageEvent a] -> Map (MessageEvent a) [MessageEvent a]
replyWindows n msgs =
  Map.fromList             -- Map (MessageEvent a) [MessageEvent a]
  . mconcat                -- [(MessageEvent a, [MessageEvent a])]
  . Map.elems              -- [[(MessageEvent a, [MessageEvent a])]]
  . Map.map chanToWin      -- Map ChannelId [(MessageEvent a, [MessageEvent a])]
  . chronoByChan           -- Map ChannelId [MessageEvent a]
  $ msgs
  where
    chanToWin :: [MessageEvent a] -> [(MessageEvent a, [MessageEvent a])]
    chanToWin ms =
      mapMaybe makePair   -- [ ]
      . inits             -- [[MessageEvent a]]
      $ ms

    makePair :: [MessageEvent a] -> Maybe (MessageEvent a, [MessageEvent a])
    makePair ms = do
      guard $ length ms > 1
      guard $ length win == n
      return (target, win)
      where
        target       = last ms
        win          = takeWin . filter ((/= target ^. userId) . (^. userId)) . init $ ms
        takeWin xs   = foldl' (const . drop 1) xs (drop n xs)

userReplyWindows :: Map (MessageEvent a) [MessageEvent a] -> Map UserId (Map (MessageEvent a) [MessageEvent a])
userReplyWindows _replyWindows = undefined

userReplyDocs :: Map UserId (Map (MessageEvent Text) [MessageEvent Text]) -> Map UserId Text
userReplyDocs = userReplyDocsWith id

userReplyDocsWith :: (a -> Text) -> Map UserId (Map (MessageEvent a) [MessageEvent a]) -> Map UserId Text
userReplyDocsWith f = fmap (Text.intercalate "\n" . map (f . (^. payload)) . mconcat . Map.elems)
