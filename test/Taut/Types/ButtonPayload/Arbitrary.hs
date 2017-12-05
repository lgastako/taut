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
                                                      )

instance Arbitrary a => Arbitrary (ButtonPayload a) where
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

instance Arbitrary Channel where
  arbitrary = Channel
    <$> arbitrary
    <*> arbitrary

instance Arbitrary Team where
  arbitrary = Team
    <$> arbitrary
    <*> arbitrary

instance Arbitrary User where
  arbitrary = User
    <$> arbitrary
    <*> arbitrary

instance Arbitrary TriggerId where
  arbitrary = TriggerId <$> arbitrary
