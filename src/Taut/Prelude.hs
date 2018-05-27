{-# LANGUAGE NoImplicitPrelude #-}

module Taut.Prelude ( module Exports ) where

import Focus.Prelude as Exports

import Control.Lens  as Exports ( (^.)
                                , view
                                )
import Data.Aeson    as Exports ( FromJSON
                                , ToJSON
                                , parseJSON
                                , toJSON
                                )

