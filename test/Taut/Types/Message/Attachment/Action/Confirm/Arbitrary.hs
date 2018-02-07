{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Taut.Types.Message.Attachment.Action.Confirm.Arbitrary () where

import Focus.Prelude

import Taut.Types.Message.Attachment.Action.Confirm ( Confirm( Confirm ) )
import Test.QuickCheck                              ( Arbitrary
                                                    , arbitrary
                                                    , genericShrink
                                                    , shrink
                                                    )
import Test.QuickCheck.Instances                    ()

instance Arbitrary Confirm where
  arbitrary = Confirm
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink
