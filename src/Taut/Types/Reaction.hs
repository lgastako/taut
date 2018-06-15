{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Taut.Types.Reaction
     ( Reaction
     , ReactionName
     , count
     , make
     , name
     , summary
     , users
     ) where

import Taut.Prelude

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
  } deriving (Data, Eq, Generic, Ord, Read, Show)

type ReactionName = Text

makeLenses ''Reaction

make :: ReactionName -> Int -> [UserId] -> Reaction
make = Reaction

summary :: Reaction -> Text
summary r = r ^. name <> " (" <> show (r ^. count) <> ")"

-- TODO: options to remove the underscores in the serialized versions...  but
--       of course we either have to convert existing data, be backwards
--       compatible or verify that we're not already using a specific thing
--       before changing it
$(deriveJSON defaultOptions ''Reaction)

derive makeArbitrary ''Reaction
