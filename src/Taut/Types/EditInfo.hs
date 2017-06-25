{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.EditInfo
       ( EditInfo
       , empty
       , ts
       , user
       ) where

import           Control.Lens                      ( makeLenses )
import           Taut.Types.Timestamp              ( Timestamp )
import qualified Taut.Types.Timestamp as Timestamp
import           Taut.Types.UserId                 ( UserId )
import qualified Taut.Types.UserId    as UserId

data EditInfo = EditInfo
  { _ts   :: Timestamp
  , _user :: UserId
  } deriving (Eq, Ord, Read, Show)

makeLenses ''EditInfo -- TODO: rename user to userId

instance Monoid EditInfo where
  mempty = empty
  (EditInfo tsA userA) `mappend` (EditInfo tsB userB) = EditInfo ts' user'
    where
      ts'   = tsA   `mappend` tsB
      user' = userA `mappend` userB

empty :: EditInfo
empty =
  EditInfo
    Timestamp.empty
    UserId.empty
