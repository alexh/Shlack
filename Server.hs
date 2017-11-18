{-# LANGUAGE
  TypeFamilies, FlexibleContexts, MultiParamTypeClasses
#-}

module Server where

import qualified Data.Map as M
import Network.Socket
import System.IO
import Model

-- State of the server. Polymorphic in the type of the socket for testing.
data ServerState s = ServerState {
  connectedUsers :: M.Map s UserName,
  channels :: M.Map Channel [UserName],
  ignoredUsers :: M.Map UserName [UserName]
}

-- Monadic actions for interacting with sockets. Polymorphic in type of socket.
class Monad m => MonadSocket m s where
  readFrom :: s -> m Message
  sendTo :: s -> Message -> m ()

-- Concrete MonadSocket instance for actual server.
instance MonadSocket IO Socket where
    readFrom = undefined
    sendTo = undefined

-- Parses String received from Client into a Message.
-- String has a friendly intermediate format.
parseMessage :: String -> Message
parseMessage = undefined

-- Evaluate a message sent by this client and update the state.
-- TODO this function may have side effects? Like printing out to user?
evaluateMessage :: UserName -> Message -> ServerState s -> ServerState s
evaluateMessage = undefined

-- Evaluate a command sent by this client and update the state.
-- TODO this function also must print to user?
evaluateCommand :: UserName -> Command -> ServerState s -> ServerState s
evaluateCommand = undefined

-- Send a message to an entire channel.
sendToChannel :: MonadSocket m Socket => String -> Channel -> m ()
sendToChannel = undefined

-- Main entry point for server.
-- main :: IO ()
-- main = undefined
-- Open sockets with Clients.
    -- Loop:
        --  Read from socket
        --  Parse data from intermediate format into message
        --  Evaluate the message, updating server state if necessary.
            -- also broadcast any messages that were sent to server