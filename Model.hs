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

-- Type of messages. Represents any client/server interaction.
data Message = TextData String
             | LogIn UserName
             | Cmd Command
    deriving (Show, Eq)

-- Type of slash commands (e.g. /whisper).
-- Slash commands are the main mechanism for non-chat interaction with the server.
data Command = JoinChannel Channel
             | Whisper UserName String
             | Ignore UserName
             | ListChannels
             | Help
             | Disconnect
 deriving (Show, Eq)