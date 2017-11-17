{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

-- Types shared across Client and Server.
module Model where

import Network.Socket
import System.IO

-- Type of usernames.
type UserName = String

-- Type of channels. Identified by their name, which must be unique.
type Channel = String

-- Type of messages.
data Message = JoinChannel Channel
             | TextData String
             | LogIn UserName
             | Disconnect

-- Type of slash commands (e.g. /whisper)
data Command = Whisper UserName
             | ListChannels
             | Ignore UserName
             | Help