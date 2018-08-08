{-# LANGUAGE NoImplicitPrelude #-}

module Taut.Prelude ( module Exports ) where

import Taut.Prelude.Extras as Exports

import Control.Lens        as Exports ( (.~)
                                      , (^.)
                                      , makeLenses
                                      , view
                                      )
import Data.Aeson          as Exports ( FromJSON
                                      , ToJSON
                                      , parseJSON
                                      , toJSON
                                      )
import Data.Data           as Exports ( Data )
import Protolude           as Exports

