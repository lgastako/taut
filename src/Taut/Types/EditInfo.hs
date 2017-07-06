{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.EditInfo
       ( EditInfo
       , ts
       , user
       ) where

import Control.Lens         ( makeLenses )
import Data.Aeson.TH        ( defaultOptions
                            , deriveJSON
                            )
import Taut.Types.Timestamp ( Timestamp )
import Taut.Types.UserId    ( UserId )

data EditInfo = EditInfo
  { _ts   :: Timestamp
  , _user :: UserId
  } deriving (Eq, Ord, Read, Show)

makeLenses ''EditInfo -- TODO: rename user to userId

$(deriveJSON defaultOptions ''EditInfo)
