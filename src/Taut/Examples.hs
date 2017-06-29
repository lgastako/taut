module Taut.Examples
       ( exampleMsg1
       ) where

import           Control.Lens                            ( (&)
                                                         , (.~)
                                                         )
import           Infinity
import           Taut
import           Taut.Types.MessageEvent                 ( payload )
import qualified Taut.Types.MessageEvent as MessageEvent

exampleMsg1 :: MessageEvent Text
exampleMsg1 = MessageEvent.empty & payload .~ "Example message #1."
