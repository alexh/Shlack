{-# LANGUAGE
  TypeFamilies, FlexibleContexts, MultiParamTypeClasses, GADTs
#-}

module Server where

import qualified Data.Map as M
import Network
import System.IO
import Model

-- A socket type for either real usage or testing.
data Socket s where
    NetSocket :: Server.Socket Network.Socket
    AbsSocket :: Server.Socket AbstractSocket

-- And here is the abstract socket type for testing.
data AbstractSocket =
  AbstractSocket { getAbstractSocket :: Int,
                   getSocketData :: String }

-- State of the server. Boundedly polymorphic in the type of the socket for testing.
data ServerState s = ServerState {
  connectedUsers :: M.Map (Server.Socket s) UserName,
  channels :: M.Map Channel [UserName],
  ignoredUsers :: M.Map UserName [UserName]
}

-- Monadic actions for interacting with sockets. Boundedly polymorphic in type of socket.
class Monad m => MonadSocket m s where
  readFrom :: Server.Socket s -> m Message
  sendTo :: Server.Socket s -> Message -> m ()

-- Concrete MonadSocket instance for actual server.
instance MonadSocket IO (Server.Socket s) where
    readFrom = undefined
    sendTo = undefined

-- Parses String received from Client into a Message.
-- String has a friendly intermediate format.
parseMessage :: String -> Message
parseMessage = undefined

-- Evaluate a message sent by this client and update the state.
-- Has a side effect of writing out to clients the data associated with the message.
evaluateMessage :: MonadSocket m (Server.Socket s) => UserName -> Message -> ServerState s -> m (ServerState s)
evaluateMessage = undefined

-- Evaluate a command sent by this client and update the state.
-- Has a side effect of writing out to clients the data associated with the command.
evaluateCommand :: MonadSocket m (Server.Socket s) => UserName -> Command -> ServerState s -> m (ServerState s)
evaluateCommand = undefined

-- Send a message to an entire channel.
sendToChannel :: MonadSocket m (Server.Socket s) => String -> Channel -> m ()
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