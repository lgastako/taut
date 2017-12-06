{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Taut.Types.Message.Arbitrary () where

import Focus.Prelude
import Taut.Types.Message                      ( Message( Message )
                                               , Parse
                                               )
import Taut.Types.Message.Attachment.Arbitrary ()
import Test.QuickCheck                         ( Arbitrary
                                               , arbitrary
                                               , elements
                                               , genericShrink
                                               , shrink
                                               )

instance Arbitrary a => Arbitrary (Message a) where
  arbitrary = Message
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary Parse where
  arbitrary = elements [toEnum 0..]
  shrink = genericShrink
