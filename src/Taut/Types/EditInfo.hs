{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Taut.Types.EditInfo
     ( EditInfo
     , from
     , ts
     , user
     ) where

import Taut.Prelude         hiding ( from )

import Control.Lens                ( makeLenses )
import Data.Aeson.TH               ( defaultOptions
                                   , deriveJSON
                                   )
import Data.DeriveTH               ( derive
                                   , makeArbitrary
                                   )
import Taut.Types.Timestamp        ( Timestamp )
import Taut.Types.UserId           ( UserId )
import Test.QuickCheck             ( Arbitrary
                                   , arbitrary
                                   )

data EditInfo = EditInfo
  { _ts   :: Timestamp
  , _user :: UserId
  } deriving (Eq, Generic, Ord, Read, Show)

makeLenses ''EditInfo -- TODO: rename user to userId

from :: (Timestamp, UserId) -> EditInfo
from = uncurry EditInfo

-- TODO: options to remove the underscores in the serialized versions...  but
--       of course we either have to convert existing data, be backwards
--       compatible or verify that we're not already using a specific thing
--       before changing it
$(deriveJSON defaultOptions ''EditInfo)

derive makeArbitrary ''EditInfo
