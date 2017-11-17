{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module Client where

import Network.Socket
import System.IO
import Model

parseInput :: String -> Message
parseInput = undefined

serializeMessage :: Message -> String
serializeMessage = undefined

clientMain = undefined
-- Open socket to Server
-- Loop:
--  Read from socket
--  Read from stdin
--  Parse stdin
--  Serialize message
--- Send message over socket

-- Main entry point for client.
-- main :: IO ()
-- main = undefined