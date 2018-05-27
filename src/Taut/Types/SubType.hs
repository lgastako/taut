{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Taut.Types.SubType
     ( SubType
     , make
     , null
     , subText
     , unSubType
     ) where

import Taut.Prelude              hiding ( null )

import Control.Lens                     ( Iso'
                                        , iso
                                        )
import Data.Default                     ( Default( def ) )
import Data.DeriveTH                    ( derive
                                        , makeArbitrary
                                        )
import Test.QuickCheck                  ( Arbitrary
                                        , arbitrary
                                        )
import Test.QuickCheck.Instances        ()

newtype SubType = SubType { unSubType :: Text }
  deriving (Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance Default SubType where
  def = make ""

make :: Text -> SubType
make = SubType

null :: SubType -> Bool
null subType
  | subType == def = True
  | otherwise      = False

subText :: Iso' SubType Text
subText = iso unSubType make

derive makeArbitrary ''SubType
