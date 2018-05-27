{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Taut.Types.Message.Attachment.Action.Arbitrary () where

import Taut.Prelude

import Taut.Types.Message.Attachment.Action                   ( Action( Action )
                                                              , ActionType( Button )
                                                              , ButtonStyle
                                                              )
import Taut.Types.Message.Attachment.Action.Confirm.Arbitrary ()
import Test.QuickCheck                                        ( Arbitrary
                                                              , arbitrary
                                                              , elements
                                                              , genericShrink
                                                              , shrink
                                                              )
import Test.QuickCheck.Instances                              ()

instance Arbitrary Action where
  arbitrary = Action
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary ActionType where
  arbitrary = return $ Button
  shrink = genericShrink

instance Arbitrary ButtonStyle where
  arbitrary = elements [toEnum 0..]
  shrink = genericShrink
