{-# LANGUAGE
  TypeFamilies, FlexibleContexts, MultiParamTypeClasses, GADTs, FlexibleInstances, ScopedTypeVariables
#-}

module Server where

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as C
import Control.Concurrent
import Network hiding (sendTo, recvFrom)
import Network.Socket as NS hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Monad.State.Class
import Control.Concurrent.MVar
import Data.Proxy
import Data.List.Split
import Model

-- A socket type for either real usage or testing.
data Socket s where
    NetSocket :: NS.Socket -> Server.Socket NS.Socket
    AbsSocket :: AbstractSocket -> Server.Socket AbstractSocket

-- An Eq instance for real sockets.
instance Eq (Server.Socket Network.Socket) where
  (NetSocket s1) == (NetSocket s2) = s1 == s2

-- And here is the abstract socket type for testing.
data AbstractSocket =
  AbstractSocket { getAbstractSocket :: Int,
                   getSocketData :: String }

-- State of the server. Boundedly polymorphic in the type of the socket for testing.
data ServerState s = ServerState {
  socketToUser :: [(Server.Socket s, UserName)], -- no Ord instance for our sockets
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
instance MonadSocket IO NS.Socket where
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
parseMessage str =
  let parts = splitOn "," str in
  case parts of
    p1 : p2 : [] -> case p1 of
      "Message" -> TextData p2
      "Login" -> Login p2
      _ -> TextData "parse error"
    _ -> TextData "parse error"


-- Evaluate a message sent by this client and update the state.
-- Has a side effect of writing out to clients the data associated with the message.
-- TODO: Add a constraint like MonadState (ServerState Network.Socket) m here
evaluateMessage :: MonadSocket m s => Server.Socket s -> UserName -> Message -> ServerState s -> m (ServerState s)
evaluateMessage sckt uname msg st =
  case msg of
      TextData str -> 
        let channel = M.lookup uname (userToChannel st) in
        case channel of
          Just c -> do
            sendToChannel Proxy uname str c st
            return st
          Nothing -> do
            return st
      Login inputName ->
        let channel = M.lookup "defaultChannel" (channelToUser st) in
        case channel of
          Just c -> do
            return (st { userToChannel = M.insert inputName "defaultChannel" (userToChannel st),
                         channelToUser = M.insert "defaultChannel" (inputName : c) (channelToUser st),
                         userToSocket = M.insert inputName sckt (userToSocket st),
                         socketToUser = (sckt, inputName) : socketToUser st })
          Nothing -> do
            return (st { userToChannel = M.insert inputName "defaultChannel" (userToChannel st),
                         channelToUser = M.insert "defaultChannel" [inputName] (channelToUser st),
                         userToSocket = M.insert inputName sckt (userToSocket st),
                         socketToUser = (sckt, inputName) : socketToUser st })
      Logout ->
        let channel = M.lookup uname (userToChannel st) in
        let sckt = M.lookup uname (userToSocket st) in
        case (sckt, channel) of
          (Just s, Just c) -> do
            undefined
          _ -> do
            return st
      Cmd c -> undefined -- TODO, after checkpoint 2?

-- Evaluate a command sent by this client and update the state.
-- Has a side effect of writing out to clients the data associated with the command.
-- TODO: Add a constraint like MonadState (ServerState Network.Socket) m here
evaluateCommand :: MonadSocket m (Server.Socket s) => UserName -> Command -> ServerState s -> m (ServerState s)
evaluateCommand = undefined



-- Send a message to an entire channel.
-- The sender of the message is given as a parameter.
-- TODO: Add a constraint like MonadState (ServerState Network.Socket) m here
sendToChannel :: MonadSocket m s => Proxy s -> UserName -> String -> Channel -> ServerState s -> m ()
sendToChannel _ uname str c st = do
  let recvs = M.lookup c (channelToUser st)
  case recvs of
    Just receivers -> do {
      let sendHelper receiver = do {
        let receiverSocket = M.lookup receiver (userToSocket st) in
        case receiverSocket of
          Just rSckt ->
            if uname == receiver 
              then return () 
              else Server.sendTo rSckt (uname ++ ": " ++ str ++ "\n")
          Nothing -> return ()
        } in
      mapM_ sendHelper receivers
    }
    Nothing -> return ()

-- Repeatedly read from the socket.
readLoop :: MonadSocket IO NS.Socket => (MVar (ServerState NS.Socket)) -> NS.Socket -> IO ()
readLoop st s = do
    let s' = NetSocket s
    msg <- readFrom s'
    putStrLn ("ack: " ++ show msg)
    ste <- takeMVar st
    let uname = lookup s' (socketToUser ste)
    case uname of
      Just name -> do
        newState <- evaluateMessage s' name msg ste
        putMVar st newState
        readLoop st s
      Nothing -> do
        newState <- evaluateMessage s' "" msg ste
        putMVar st newState
        readLoop st s

-- Repeatedly accept connections and fork a new thread to read from them.
mainLoop :: MonadSocket IO NS.Socket => (MVar (ServerState Network.Socket)) -> Network.Socket -> IO ()
mainLoop st s = do
  -- Related TODO, make the ServerState be a monadic state so our lives are easier
  (s', _) <- NS.accept s
  _ <- forkIO (readLoop st s')
  mainLoop st s

-- Main entry point for server.
main :: MonadSocket IO NS.Socket => IO ()
main = do
  sckt <- socket AF_INET Stream defaultProtocol
  setSocketOption sckt ReuseAddr 1
  bind sckt (SockAddrInet 4040 iNADDR_ANY)
  listen sckt 3
  let st = ServerState {socketToUser = [], 
                        userToSocket = M.empty, 
                        channelToUser = M.empty, 
                        userToChannel = M.empty, 
                        ignoredUsers = M.empty}
  mst <- newMVar st
  mainLoop mst sckt

-- Open sockets with Clients.
    -- Loop:
        --  Read from socket
        --  Parse data from intermediate format into message
        --  Evaluate the message, updating server state if necessary.
            -- also broadcast any messages that were sent to server