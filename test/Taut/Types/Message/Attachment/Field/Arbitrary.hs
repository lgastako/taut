{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Taut.Types.Message.Attachment.Field.Arbitrary () where

import Taut.Prelude

import Taut.Types.Message.Attachment.Field ( Field( Field ) )
import Test.QuickCheck
import Test.QuickCheck.Instances           ()

instance Arbitrary Field where
  arbitrary = Field
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink
