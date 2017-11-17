{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module Client where

import Network.Socket
import System.IO
import Test.HUnit

import Model

parseInput :: String -> Message
parseInput = undefined

serializeMessage :: Message -> String
serializeMessage = undefined

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

testSerializeMessage :: Test
testSerializeMessage = TestList [
    serializeMessage (JoinRoom 1) ~?= "Join,1",
    serializeMessage (JoinRoom 232) ~?= "Join,232"]
