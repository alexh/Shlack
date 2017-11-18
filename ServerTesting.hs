{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module ServerTesting where

import qualified Data.Map as M
import Control.Monad.State as S
import Test.HUnit
import Model
import Server

-- Abstract socket type for testing.
newtype AbstractSocket =
  AbstractSocket { getAbstractSocket :: Int }

-- Monadic actions for testing sockets.
class MonadSocket m where
  readFrom :: AbstractSocket -> m Message
  sendTo :: AbstractSocket -> Message -> m ()

serverIter :: (MonadSocket m, MonadState (ServerState AbstractSocket) m)
           => AbstractSocket
           -> m ()
serverIter sock = do
  msg <- readFrom sock
  case msg of
    TextData s ->
      -- TODO document
      return ()
    Login u ->
      -- TODO document
      return ()
    Cmd c ->
      -- TODO document
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