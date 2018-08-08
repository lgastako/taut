{-# LANGUAGE DeriveDataTypeable         #-}
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
     , toText
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
import Data.Serialize                   ( Serialize )
import Data.Serialize.Text              ()
import Test.QuickCheck                  ( Arbitrary
                                        , arbitrary
                                        )
import Test.QuickCheck.Instances        ()

newtype SubType = SubType { unSubType :: Text }
  deriving (Data, Eq, FromJSON, Generic, Ord, Read, Show, ToJSON)

instance Default SubType where
  def = make ""

instance Serialize SubType

make :: Text -> SubType
make = SubType

null :: SubType -> Bool
null subType
  | subType == def = True
  | otherwise      = False

subText :: Iso' SubType Text
subText = iso unSubType make

toText :: SubType -> Text
toText = unSubType

derive makeArbitrary ''SubType
