{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Taut.Types.EditInfo
       ( EditInfo
       , from
       , ts
       , user
       ) where

import Focus.Prelude        hiding ( from )

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

$(deriveJSON defaultOptions ''EditInfo)

derive makeArbitrary ''EditInfo
