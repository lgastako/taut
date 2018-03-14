{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Taut.Types.ButtonPayload.Arbitrary () where

import Focus.Prelude

import Taut.Types.ButtonPayload                       ( ButtonPayload( ButtonPayload )
                                                      , Channel( Channel )
                                                      , Team( Team )
                                                      , TriggerId( TriggerId )
                                                      , User( User )
                                                      )
import Taut.Types.Message.Arbitrary                   ()
import Taut.Types.Message.Attachment.Action.Arbitrary ()
import Test.QuickCheck                                ( Arbitrary
                                                      , arbitrary
                                                      , genericShrink
                                                      , shrink
                                                      )

instance Arbitrary ButtonPayload where
  arbitrary = ButtonPayload
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
  shrink = genericShrink

instance Arbitrary Channel where
  arbitrary = Channel
    <$> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary Team where
  arbitrary = Team
    <$> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary User where
  arbitrary = User
    <$> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary TriggerId where
  arbitrary = TriggerId <$> arbitrary
  shrink    = genericShrink
