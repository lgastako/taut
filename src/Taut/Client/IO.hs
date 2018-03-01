module Taut.Client.IO
     ( sendMessage
     ) where

import Data.Text              ( Text )
import Taut.Types.UserId      ( UserId )
import Taut.Types.AccessToken ( AccessToken )

sendMessage :: AccessToken token => token -> UserId -> Text -> IO ()
sendMessage = error "Taut.Client.IO.sendMessage not impl"
