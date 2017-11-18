{-# LANGUAGE
  TypeFamilies, FlexibleContexts, MultiParamTypeClasses
#-}

module ServerTesting where

import qualified Data.Map as M
import Control.Monad.State as S
import Test.HUnit
import Model
import Server

-- Abstract socket type for testing.
data AbstractSocket =
  AbstractSocket { getAbstractSocket :: Int,
                   getSocketData :: String }

-- Inject new data into the socket for testing.
injectSocketData :: AbstractSocket -> String -> AbstractSocket
injectSocketData = undefined

instance MonadSocket Maybe AbstractSocket where
  readFrom s = Just (TextData (getSocketData s))
  sendTo s m = Just () -- succeed silently

serverIter :: (MonadSocket m AbstractSocket, MonadState (ServerState AbstractSocket) m)
           => AbstractSocket
           -> m ()
serverIter sock = do
  msg <- readFrom sock
  case msg of
    TextData s ->
      -- Send the message to all sockets with connected users in this channel.
      return ()
    Login u ->
      -- Add the user to the chat platform. User should start in a default "general" channel.
      return ()
    Cmd c ->
      -- Note that the sender of messages will be identified by the socket they are connected to.
      case c of
        JoinChannel ch ->
          -- Remove the user from their current channel and put them in the new channel.
          return ()
        Whisper name str ->
          -- Send this message privately to the destination user.
          return ()
        Ignore name ->
          -- Mark that all messages sent by this user should not be seen by the sender.
          return ()
        ListChannels ->
          -- Output all channels to sender.
          return ()
        Help ->
          -- Output helpful information to the sender about how to use the platform.
          return ()
        Disconnect ->
          -- Remove the user from their channel and close the client.
          return ()

-- Mini test suite for the parseMessage function.
testParseMessage :: Test
testParseMessage = TestList [
    parseMessage "Join,channel1" ~?= Cmd (JoinChannel "channel1"),
    parseMessage "Message,pizza" ~?= TextData "pizza",
    parseMessage "Login,alex" ~?= Login "alex",
    parseMessage "Disconnect" ~?= Cmd Disconnect,
    parseMessage "ListChannels" ~?= Cmd ListChannels,
    parseMessage "Help" ~?= Cmd Help,
    parseMessage "Ignore,alex" ~?= Cmd (Ignore "alex"),
    parseMessage "Whisper,alex,hey dad" ~?= Cmd (Whisper "alex" "hey dad")]
    