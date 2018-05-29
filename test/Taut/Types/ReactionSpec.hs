{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Taut.Types.ReactionSpec ( spec ) where

import Taut.Prelude

import Data.Aeson                    ( decode
                                     , encode
                                     )
import Taut.Types.Reaction           ( Reaction
                                     , make
                                     )
import Taut.Types.UserId   as UserId
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Reaction" $

  context "serialization" $ do

    it "should roundtrip" $ property $
      \r -> (decode.encode) r == Just (r :: Reaction)

    it "this specific example thus" $ do
      let uid1 = UserId.fromText "U123"
          r    = make "fixme" 3 [uid1]
      encode r `shouldBe` "{\"_name\":\"fixme\",\"_count\":3,\"_users\":[\"U123\"]}"
