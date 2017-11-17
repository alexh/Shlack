{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

-- Types shared across Client and Server.
module Model where

import Network.Socket
import System.IO

-- Type of messages.
data Message = JoinRoom Int
             | TextData String
             | LogIn String
             | Disconnect