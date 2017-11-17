{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module Server where

import qualified Data.Map as M
import Network.Socket
import System.IO
import Model

-- State of the server. Polymorphic in the type of the socket.
data ServerState s = ServerState {
  connectedUsers :: M.Map s Int
}

-- Parses String recieved from Socket into a Message
parseMessage :: String -> Message
parseMessage = undefined

-- Main entry point for server.
-- main :: IO ()
-- main = undefined