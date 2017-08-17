{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.SubType
       ( SubType
       , empty
       , make
       , null
       , toText
       ) where

import Prelude                   hiding ( null )

import Data.Aeson.TH                    ( defaultOptions
                                        , deriveJSON
                                        )
import Data.DeriveTH                    ( derive
                                        , makeArbitrary
                                        )
import Infinity
import Test.QuickCheck                  ( Arbitrary
                                        , arbitrary
                                        )
import Test.QuickCheck.Instances        ()

newtype SubType = SubType Text
  deriving (Eq, Ord, Read, Show)

make :: Text -> SubType
make = SubType

empty :: SubType
empty = make ""

null :: SubType -> Bool
null subType
  | subType == empty = True
  | otherwise        = False

toText :: SubType -> Text
toText (SubType s) = s

$(deriveJSON defaultOptions ''SubType)

derive makeArbitrary ''SubType
