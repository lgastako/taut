{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.Reaction
       ( Reaction
       , make
       , name
       , count
       , users
       ) where

import Control.Lens      ( makeLenses )
import Data.Aeson.TH     ( defaultOptions
                         , deriveJSON
                         )
import Infinity
import Taut.Types.UserId ( UserId )

data Reaction = Reaction
  { _name  :: ReactionName
  , _count :: Int
  , _users :: [UserId]
  } deriving (Eq, Ord, Read, Show)

type ReactionName = Text

makeLenses ''Reaction

make :: ReactionName -> Int -> [UserId] -> Reaction
make = Reaction

$(deriveJSON defaultOptions ''Reaction)
