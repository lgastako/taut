{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Taut.Types.Message.Attachment.Action.Confirm.Arbitrary () where

import Taut.Prelude

import Taut.Types.Message.Attachment.Action.Confirm ( Confirm( Confirm ) )
import Test.QuickCheck
import Test.QuickCheck.Instances                    ()

instance Arbitrary Confirm where
  arbitrary = Confirm
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink
