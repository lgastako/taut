{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Taut.Types.Message.Attachment.Arbitrary () where

import Taut.Prelude

import Taut.Types.Message.Attachment                  ( Attachment( Attachment )
                                                      , Color( ColorCode
                                                             , Danger
                                                             , Good
                                                             , Warning
                                                             )
                                                      )
import Taut.Types.Message.Attachment.Action.Arbitrary ()
import Taut.Types.Message.Attachment.Field.Arbitrary  ()
import Test.QuickCheck

instance Arbitrary Color where
  arbitrary = do
    cc1 <- arbitrary
    cc2 <- arbitrary
    -- TODO: better
    elements [Good, Warning, Danger, ColorCode cc1, ColorCode cc2]
  shrink = genericShrink

instance Arbitrary Attachment where
  arbitrary = Attachment
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
