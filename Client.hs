{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module Main where

import Network.Socket
import System.IO

-- | IP address of the local host
local :: HostName
local = undefined

-- | Start the client given an IP address and a port. The port should
-- be a string number > 1024
client :: HostName -> ServiceName -> IO Handle
client = undefined

-- Main entry point for client.
main :: IO ()
main = undefined