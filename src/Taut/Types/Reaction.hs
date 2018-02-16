{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.Reaction
       ( Reaction
       , count
       , make
       , name
       , summary
       , users
       ) where

import Focus.Prelude

import Control.Lens      ( (^.)
                         , makeLenses
                         )
import Data.Aeson.TH     ( defaultOptions
                         , deriveJSON
                         )
import Data.DeriveTH     ( derive
                         , makeArbitrary
                         )
import Taut.Types.UserId ( UserId )
import Test.QuickCheck   ( Arbitrary
                         , arbitrary
                         )

data Reaction = Reaction
  { _name  :: ReactionName
  , _count :: Int
  , _users :: [UserId]
  } deriving (Eq, Generic, Ord, Read, Show)

type ReactionName = Text

makeLenses ''Reaction

make :: ReactionName -> Int -> [UserId] -> Reaction
make = Reaction

summary :: Reaction -> Text
summary r = r ^. name <> " (" <> show (r ^. count) <> ")"

$(deriveJSON defaultOptions ''Reaction)

derive makeArbitrary ''Reaction
