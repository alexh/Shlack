{-# LANGUAGE
  TypeFamilies, FlexibleContexts, MultiParamTypeClasses
#-}

module ServerTesting where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Test.HUnit
import Model
import Server

-- Inject new data into the socket for testing.
injectSocketData :: AbstractSocket -> String -> AbstractSocket
injectSocketData = undefined

-- The monad socket implementation for abstract sockets.
instance MonadSocket Maybe AbstractSocket where
  readFrom (AbsSocket s) = Just (TextData (getSocketData s))
  sendTo (AbsSocket s) m = Just () -- succeed silently

-- Example of a test suite for server actions.
-- TODO, how do you use do notation in a unit test?
  -- Define a runTestingServer to run our monadic action (the stateful socket one)
  -- Use runStateT
testServer :: Test
testServer = TestList []

-- Mini test suite for the parseMessage function.
testParseMessage :: Test
testParseMessage = TestList [
    parseMessage (L.intercalate delim ["Join", "channel1"]) ~?= Cmd (JoinChannel "channel1"),
    parseMessage (L.intercalate delim ["Message", "pizza"]) ~?= TextData "pizza",
    parseMessage (L.intercalate delim ["Login", "alex"]) ~?= Login "alex",
    parseMessage "Logout" ~?= Logout,
    parseMessage "ListChannels" ~?= Cmd ListChannels,
    parseMessage "Help" ~?= Cmd Help,
    parseMessage "ListUsers" ~?= Cmd ListUsers,
    parseMessage (L.intercalate delim ["Whisper", "alex", "hey dad"]) ~?= Cmd (Whisper "alex" "hey dad")]

-- Entry point for testing.
main :: IO ()
main = do
  _ <- runTestTT (TestList [
          testParseMessage])
  return ()
    