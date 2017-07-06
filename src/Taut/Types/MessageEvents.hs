module Taut.Types.MessageEvents
       ( onlyMessages
       , toCSV
       , withPayloads
       ) where

import           Prelude                            hiding ( null )

import           Control.Lens                              ( (^.) )
import qualified Data.Csv                as Csv
import           Infinity
import           Taut.Types.MessageEvent                   ( MessageEvent
                                                           , payload
                                                           , subType
                                                           )
import qualified Taut.Types.SubType      as SubType

-- TODO: rename this to something that communicates better. and also create
-- something called something like "justMessages" that filters out the non
-- SubType.empty messages.
withPayloads :: [MessageEvent (Maybe Text)] -> [MessageEvent Text]
withPayloads = foldr add []
  where
    add e acc =
      case e ^. payload of
        Just text -> m:acc
          where
            m = const text <$> e
        Nothing -> acc

onlyMessages :: [MessageEvent a] -> [MessageEvent a]
onlyMessages = filter (SubType.null . (^. subType))

toCSV :: [MessageEvent Text] -> LByteString
toCSV = Csv.encode
