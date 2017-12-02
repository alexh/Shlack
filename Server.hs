{-# LANGUAGE
  TypeFamilies, FlexibleContexts, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables
#-}

module Server where

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C
import Control.Concurrent
import Control.Concurrent.Chan
import Network hiding (send, sendTo, recv, recvFrom)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.Proxy
import System.IO
import Model

-- A socket type for either real usage or testing.
data Socket s where
    NetSocket :: Network.Socket -> Server.Socket Network.Socket
    AbsSocket :: AbstractSocket -> Server.Socket AbstractSocket

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
instance MonadSocket IO Network.Socket where
    readFrom s' = 
      let s = getNetSocket s' in
      do
        byteData <- recv s 1024
        return (parseMessage (C.unpack byteData))
    sendTo = undefined

getNetSocket :: Server.Socket s -> Network.Socket
getNetSocket (NetSocket s) = s
getNetSocket _ = undefined -- todo?

-- Parses String received from Client into a Message.
-- String has a friendly intermediate format.
parseMessage :: String -> Message
parseMessage = TextData -- TODO, implement this

-- Evaluate a message sent by this client and update the state.
-- Has a side effect of writing out to clients the data associated with the message.
-- TODO: Add a constraint like MonadState (ServerState Network.Socket) m here
evaluateMessage :: MonadSocket m (Server.Socket s) => UserName -> Message -> ServerState s -> m (ServerState s)
evaluateMessage = undefined

-- Evaluate a command sent by this client and update the state.
-- Has a side effect of writing out to clients the data associated with the command.
-- TODO: Add a constraint like MonadState (ServerState Network.Socket) m here
evaluateCommand :: MonadSocket m (Server.Socket s) => UserName -> Command -> ServerState s -> m (ServerState s)
evaluateCommand = undefined

-- Send a message to an entire channel.
-- TODO: Add a constraint like MonadState (ServerState Network.Socket) m here
sendToChannel :: MonadSocket m (Server.Socket s) => Proxy s -> String -> Channel -> m ()
sendToChannel = undefined

mainLoop :: Network.Socket -> IO ()
mainLoop s = do
  msg <- readFrom (NetSocket s)
  putStrLn ("ack: " ++ (show msg))
  mainLoop s

-- Main entry point for server.
main :: IO ()
main = do
  s <- socket AF_INET Stream defaultProtocol
  setSocketOption s ReuseAddr 1
  bind s (SockAddrInet 4040 iNADDR_ANY)
  listen s 2
  (s', _) <- Network.Socket.accept s
  mainLoop s'

-- Open sockets with Clients.
    -- Loop:
        --  Read from socket
        --  Parse data from intermediate format into message
        --  Evaluate the message, updating server state if necessary.
            -- also broadcast any messages that were sent to server