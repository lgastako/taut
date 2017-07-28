module Taut.Functions
       ( byChannel
       , chronoByChan
       , chronological
       , replyWindow
       ) where

import           Control.Lens                   ( (^.) )
import           Data.List                      ( sortOn
                                                , tails
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict         as Map
import           Taut.Types.ChannelId           ( ChannelId )
import           Taut.Types.MessageEvent        ( MessageEvent
                                                , channelId
                                                , ts
                                                )

byChannel :: [MessageEvent a] -> Map ChannelId [MessageEvent a]
byChannel = foldr absorb Map.empty
  where
    absorb msg = Map.insertWith (++) (msg ^. channelId) [msg]

chronological :: [MessageEvent a] -> [MessageEvent a]
chronological = sortOn (^. ts)

chronoByChan :: [MessageEvent a] -> Map ChannelId [MessageEvent a]
chronoByChan = Map.map chronological . byChannel

-- replyWindow :: Int -> [MessageEvent a] -> Map UserId [MessageEvent a]
-- replyWindow n msgs = error "replyWindow not finished"
--   where
--     perMsgWindows     = mconcat . Map.elems . Map.map perMsgWindow . chronoByChan $ msgs
--     perMsgWindow msgs = filter ((== n) . length) . map (take n) . tails $ msgs

replyWindow :: Int -> [MessageEvent a] -> [[MessageEvent a]]
replyWindow n msgs = perMsgWindows
  where
    perMsgWindows = mconcat . Map.elems . Map.map perMsgWindow . chronoByChan $ msgs
    perMsgWindow  = filter ((== n) . length) . map (take n) . tails
