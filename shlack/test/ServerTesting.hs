{-# LANGUAGE
  TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances
#-}

module ServerTesting where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Test.HUnit
import Model
import Server

-- Inject new data into the socket for testing.
injectSocketData :: AbstractSocket -> String -> AbstractSocket
injectSocketData sck str = sck { getSocketData = str }

-- The action of user Alice logging in.
aliceLogin :: AbstractSocket 
aliceLogin = AbstractSocket { getAbstractSocket = 1,
                              getSocketData = "Login" ++ delim ++ "Alice" }

-- The action of user Bob logging in.
bobLogin :: AbstractSocket 
bobLogin = AbstractSocket { getAbstractSocket = 2,
                            getSocketData = "Login" ++ delim ++ "Bob" }

-- An empty server state. General channel is the default channel.
emptyState :: ServerState AbstractSocket
emptyState = ServerState {socketToUser = [], 
                          userToSocket = M.empty, 
                          channelToUser = M.insert "General" [] M.empty, 
                          userToChannel = M.empty}

-- A state with just Alice logged in.
aliceState :: ServerState AbstractSocket
aliceState = 
  let s = AbsSocket aliceLogin in
  ServerState {socketToUser = [(s, "Alice")],
                userToSocket = M.insert "Alice" s M.empty,
                channelToUser = M.insert "General" ["Alice"] M.empty,
                userToChannel = M.insert "Alice" "General" M.empty}

-- A state with both Alice and Bob logged in.
aliceBobState :: ServerState AbstractSocket
aliceBobState = 
  let s1 = AbsSocket aliceLogin
      s2 = AbsSocket bobLogin in
  ServerState {socketToUser = [(s2, "Bob"), (s1, "Alice")],
                              userToSocket = 
                                M.insert "Bob" s2 (M.insert "Alice" s1 M.empty),
                              channelToUser = 
                                M.insert "General" ["Bob", "Alice"] 
                                (M.insert "General" ["Alice"] M.empty),
                              userToChannel = 
                                M.insert "Bob" "General"
                                (M.insert "Alice" "General" M.empty)}

-- The monad socket implementation for abstract sockets.
instance MonadSocket IO AbstractSocket where
  readFrom (AbsSocket s) = return (parseMessage (getSocketData s))
  sendTo (AbsSocket s) m = return () -- succeed silently

-- An equality instance for ServerStates for testing.
instance Eq (ServerState AbstractSocket) where
  s1 == s2 = socketToUser s1 == socketToUser s2
    && userToSocket s1 == userToSocket s2
    && channelToUser s1 == channelToUser s2
    && userToChannel s1 == userToChannel s2

-- And a show instance (surprisingly demanded by HUnit)
instance Show (ServerState AbstractSocket) where
  show s = show (socketToUser s)
    ++ ", " ++ show (userToSocket s)
    ++ ", " ++ show (channelToUser s)
    ++ ", " ++ show (userToChannel s)

-- The test suite for server state logic.
testOneLogin :: Test
testOneLogin = TestCase (do
  let s = AbsSocket aliceLogin
  msg <- readFrom s
  newState <- evaluateMessage s "" msg emptyState
  assertEqual "one login test" aliceState newState)

testTwoLogins :: Test
testTwoLogins = TestCase (do
  let s1 = AbsSocket aliceLogin
  let s2 = AbsSocket bobLogin
  msg1 <- readFrom s1
  msg2 <- readFrom s2
  newState1 <- evaluateMessage s1 "" msg1 emptyState
  newState2 <- evaluateMessage s2 "" msg2 newState1
  assertEqual "two logins test" aliceBobState newState2)

testOneLogout :: Test
testOneLogout = TestCase (do
  let logoutSock = AbsSocket (injectSocketData bobLogin "Logout")
  msg <- readFrom logoutSock
  newState <- evaluateMessage logoutSock "Bob" msg aliceBobState
  assertEqual "one logout test" aliceState newState)

testTwoLogouts :: Test
testTwoLogouts = TestCase (do
  let logoutSock1 = AbsSocket (injectSocketData aliceLogin "Logout")
  let logoutSock2 = AbsSocket (injectSocketData bobLogin "Logout")
  msg1 <- readFrom logoutSock1
  msg2 <- readFrom logoutSock2
  newState1 <- evaluateMessage logoutSock1 "Alice" msg1 aliceBobState
  newState2 <- evaluateMessage logoutSock2 "Bob" msg2 newState1
  assertEqual "two logouts (all users) test" emptyState newState2)

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
serverTestingMain :: IO ()
serverTestingMain = do
  _ <- runTestTT (TestList [
          testParseMessage, testOneLogin, testTwoLogins,
          testOneLogout, testTwoLogouts])
  return ()