{-# LANGUAGE TemplateHaskell #-}
module Taut.Types.SubType
       ( SubType
       , make
       , null
       , subText
       , toText
       ) where

import Prelude                   hiding ( null )

import Control.Lens                     ( Iso'
                                        , iso
                                        )
import Data.Aeson.TH                    ( defaultOptions
                                        , deriveJSON
                                        )
import Data.Default                     ( Default( def ) )
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

instance Default SubType where
  def = make ""

make :: Text -> SubType
make = SubType

null :: SubType -> Bool
null subType
  | subType == def = True
  | otherwise      = False

toText :: SubType -> Text
toText (SubType s) = s

subText :: Iso' SubType Text
subText = iso toText make

$(deriveJSON defaultOptions ''SubType)

derive makeArbitrary ''SubType
