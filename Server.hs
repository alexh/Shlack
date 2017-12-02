{-# LANGUAGE
  TypeFamilies, FlexibleContexts, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables
#-}

module Server where

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as C
import Control.Concurrent
import Network hiding (sendTo, recvFrom)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Monad.State.Class
import Data.Proxy
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
  socketToUser :: M.Map (Server.Socket s) UserName,
  userToSocket :: M.Map UserName (Server.Socket s),
  channelToUser :: M.Map Channel [UserName],
  userToChannel :: M.Map UserName Channel,
  ignoredUsers :: M.Map UserName [UserName]
}

-- Monadic actions for interacting with sockets. Boundedly polymorphic in type of socket.
class Monad m => MonadSocket m s where
  readFrom :: Server.Socket s -> m Message
  sendTo :: Server.Socket s -> String -> m ()

-- Concrete MonadSocket instance for actual server.
instance MonadSocket IO Network.Socket where
    readFrom s' = 
      let s = getNetSocket s' in
      do
        byteData <- recv s 1024
        return (parseMessage (C.unpack byteData))
    sendTo s' str =
      let s = getNetSocket s' in
      do
        sendAll s (C.pack $ str)

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
evaluateMessage uname msg st =
  case msg of
      TextData str -> 
        let channel = M.lookup uname (userToChannel st) in
        case channel of
          Just c -> do
            sendToChannel Proxy str c st
            return st
          Nothing -> do
            return st
      Login inputName ->
        let channel = M.lookup "defaultChannel" (channelToUser st) in
        case channel of
          Just c -> do
            return (st { userToChannel = (M.insert inputName "defaultChannel" (userToChannel st)),
             channelToUser = (M.insert "defaultChannel" (uname : c) (channelToUser st)) })
          Nothing -> do
            return st
      Logout -> undefined
      Cmd c -> undefined -- TODO, after checkpoint 2?

-- Evaluate a command sent by this client and update the state.
-- Has a side effect of writing out to clients the data associated with the command.
-- TODO: Add a constraint like MonadState (ServerState Network.Socket) m here
evaluateCommand :: MonadSocket m (Server.Socket s) => UserName -> Command -> ServerState s -> m (ServerState s)
evaluateCommand = undefined

-- Send a message to an entire channel.
-- TODO: Add a constraint like MonadState (ServerState Network.Socket) m here
sendToChannel :: MonadSocket m (Server.Socket s) => Proxy s -> String -> Channel -> ServerState s -> m ()
sendToChannel = undefined

-- Repeatedly read from the socket.
readLoop :: ServerState Network.Socket -> Network.Socket -> IO ()
readLoop st s = do
    let s' = NetSocket s
    msg <- readFrom s'
    putStrLn ("ack: " ++ (show msg))
    readLoop st s

-- Repeatedly accept connections and fork a new thread to read from them.
mainLoop :: ServerState Network.Socket -> Network.Socket -> IO ()
mainLoop st s = do
  (s', _) <- Network.Socket.accept s
  _ <- forkIO (readLoop st s')
  mainLoop st s

-- Main entry point for server.
main :: IO ()
main = do
  sckt <- socket AF_INET Stream defaultProtocol
  setSocketOption sckt ReuseAddr 1
  bind sckt (SockAddrInet 4040 iNADDR_ANY)
  listen sckt 3
  let st = ServerState {socketToUser = M.empty, 
                        userToSocket = M.empty, 
                        channelToUser = M.empty, 
                        userToChannel = M.empty, 
                        ignoredUsers = M.empty}
  mainLoop st sckt

-- Open sockets with Clients.
    -- Loop:
        --  Read from socket
        --  Parse data from intermediate format into message
        --  Evaluate the message, updating server state if necessary.
            -- also broadcast any messages that were sent to server