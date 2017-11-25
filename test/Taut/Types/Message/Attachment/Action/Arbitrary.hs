{-# OPTIONS_GHC -fno-warn-orphans #-}
module Taut.Types.Message.Attachment.Action.Arbitrary () where

import Taut.Types.Message.Attachment.Action                   ( Action( Action )
                                                              , ActionType( Button )
                                                              , ButtonStyle
                                                              )
import Taut.Types.Message.Attachment.Action.Confirm.Arbitrary ()
import Test.QuickCheck                                        ( Arbitrary
                                                              , arbitrary
                                                              , elements
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

instance Arbitrary ActionType where
  arbitrary = return $ Button

instance Arbitrary ButtonStyle where
  arbitrary = elements [toEnum 0..]
