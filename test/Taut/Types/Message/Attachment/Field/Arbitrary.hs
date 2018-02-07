{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Taut.Types.Message.Attachment.Field.Arbitrary () where

import Focus.Prelude

import Taut.Types.Message.Attachment.Field ( Field( Field ) )
import Test.QuickCheck                     ( Arbitrary
                                           , arbitrary
                                           , genericShrink
                                           , shrink
                                           )
import Test.QuickCheck.Instances           ()

instance Arbitrary Field where
  arbitrary = Field
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink
