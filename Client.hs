{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module Client where

import Network.Socket
import System.IO

import Model

-- Parses user input into a Message.
parseInput :: String -> Message
parseInput = undefined

-- Serializes messages into a friendly intermediate format to send to server.
serializeMessage :: Message -> String
serializeMessage = undefined

-- Serializes commands into a friendly intermediate format to send to server.
serializeCommand :: Command -> String
serializeCommand = undefined

-- | IP address of the local host
local :: HostName
local = undefined

-- | Start the client given an IP address and a port. The port should
-- be a string number > 1024
client :: HostName -> ServiceName -> IO Handle
client = undefined

-- Main entry point for client.
-- main :: IO ()
-- main = undefined
    -- Open socket to Server
    -- Loop:
        --  Read from socket
        --  Read from stdin
        --  Parse stdin
        --  Serialize message
        --- Send message over socket