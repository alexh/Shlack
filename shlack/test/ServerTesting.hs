{-# LANGUAGE
  TypeFamilies, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances
#-}

module ServerTesting where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Lazy
import Test.HUnit
import Model
import Server

-- The state type for testing.
-- The state is a list of sockets, where each socket stores the most recent
-- data that can be read from it or was written to it.
type MockState = StateT [AbstractSocket] IO

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

-- A state with Alice and Bob in distinct channels.
aliceBobChannels :: ServerState AbstractSocket
aliceBobChannels =
  let s1 = AbsSocket aliceLogin
      s2 = AbsSocket bobLogin in
  ServerState {socketToUser = [(s2, "Bob"), (s1, "Alice")],
                userToSocket = 
                  M.insert "Bob" s2 (M.insert "Alice" s1 M.empty),
                channelToUser = 
                  M.insert "random" ["Bob"] 
                  (M.insert "General" ["Alice"] M.empty),
                userToChannel = 
                  M.insert "Bob" "random"
                  (M.insert "Alice" "General" M.empty)}

-- Inject new data into the socket for testing.
injectSocketData :: AbstractSocket -> String -> AbstractSocket
injectSocketData sck str = sck { getSocketData = str }

-- Insert the given abstract socket into the list, or update the existing
-- abstract socket in the list if there is one.
updateSockets :: AbstractSocket -> [AbstractSocket] -> [AbstractSocket]
updateSockets s l = 
  let l' = map (\x -> if getAbstractSocket x == getAbstractSocket s then s else x) l in
  if l == l' then s : l' else l'

-- Get the string data out of the input socket in the list, or "" if there
-- is no data.
getSocket :: AbstractSocket -> [AbstractSocket] -> String
getSocket sckt = foldr (\x acc -> if getAbstractSocket sckt == getAbstractSocket x then
  getSocketData x else acc) ""

-- The monad socket implementation for abstract sockets.
instance MonadSocket MockState AbstractSocket where
  readFrom (AbsSocket s) = return (parseMessage (getSocketData s))
  sendTo (AbsSocket s) str = do
    lst <- get
    put (updateSockets (injectSocketData s str) lst)

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
  msg <- evalStateT (do readFrom s) [aliceLogin]
  newState <- evalStateT (do evaluateMessage s "" msg emptyState) [aliceLogin]
  assertEqual "one login test" aliceState newState)

testTwoLogins :: Test
testTwoLogins = TestCase (do
  let s1 = AbsSocket aliceLogin
  let s2 = AbsSocket bobLogin
  msg1 <- evalStateT (do readFrom s1) [aliceLogin]
  msg2 <- evalStateT (do readFrom s2) [bobLogin, aliceLogin]
  newState1 <- evalStateT 
    (do evaluateMessage s1 "" msg1 emptyState) [aliceLogin]
  newState2 <- evalStateT 
    (do evaluateMessage s2 "" msg2 newState1) [bobLogin, aliceLogin]
  assertEqual "two logins test" aliceBobState newState2)

testOneLogout :: Test
testOneLogout = TestCase (do
  let logoutSock = AbsSocket (injectSocketData bobLogin "Logout")
  msg <- evalStateT (do readFrom logoutSock) [bobLogin, aliceLogin]
  newState <- evalStateT
    (do evaluateMessage logoutSock "Bob" msg aliceBobState) [bobLogin, aliceLogin]
  assertEqual "one logout test" aliceState newState)

testTwoLogouts :: Test
testTwoLogouts = TestCase (do
  let logoutSock1 = AbsSocket (injectSocketData aliceLogin "Logout")
  let logoutSock2 = AbsSocket (injectSocketData bobLogin "Logout")
  msg1 <- evalStateT (do readFrom logoutSock1) [bobLogin, aliceLogin]
  msg2 <- evalStateT (do readFrom logoutSock2) [bobLogin]
  newState1 <- evalStateT
    (do evaluateMessage logoutSock1 "Alice" msg1 aliceBobState) [bobLogin, aliceLogin]
  newState2 <- evalStateT
    (do evaluateMessage logoutSock2 "Bob" msg2 newState1) [bobLogin]
  assertEqual "two logouts (all users) test" emptyState newState2)

testChangeChannel :: Test
testChangeChannel = TestCase (do
  let changeSock = AbsSocket (injectSocketData bobLogin ("Join" ++ delim ++ "random"))
  msg <- evalStateT (do readFrom changeSock) [bobLogin, aliceLogin]
  newState <- evalStateT
    (do evaluateMessage changeSock "Bob" msg aliceBobState) [bobLogin, aliceLogin]
  assertEqual "bob changes channels" aliceBobChannels newState)

testReturnToChannel :: Test
testReturnToChannel = TestCase (do
  let changeSock = AbsSocket (injectSocketData bobLogin ("Join" ++ delim ++ "General"))
  msg <- evalStateT (do readFrom changeSock) [bobLogin, aliceLogin]
  newState <- evalStateT
    (do evaluateMessage changeSock "Bob" msg aliceBobChannels) [bobLogin, aliceLogin]
  let expectedState = 
        aliceBobState { 
          channelToUser = M.insert "random" [] (channelToUser aliceBobState) }
  assertEqual "bob returns to general" expectedState newState)

testChannelSend :: Test
testChannelSend = TestCase (do
  let s = AbsSocket aliceLogin
  let msg = TextData "hi bob"
  mockState <- execStateT 
    (do evaluateMessage s "Alice" msg aliceBobState) [aliceLogin, bobLogin]
  let str = getSocket bobLogin mockState
  assertEqual "send to channel" ("P" ++ delim ++ "Alice: hi bob\n") str)

testChannelMessageIsolation :: Test
testChannelMessageIsolation = TestCase (do
  let s = AbsSocket aliceLogin
  let msg = TextData "hi bob"
  mockState <- execStateT
    (do evaluateMessage s "Alice" msg aliceBobChannels) [aliceLogin, bobLogin]
  let str = getSocket bobLogin mockState
  -- if no msg was sent then the data to be read is still the login data
  assertEqual "no send across channel" ("Login" ++ delim ++ "Bob") str)

testWhisperAcrossChannels :: Test
testWhisperAcrossChannels = TestCase (do
  let s = AbsSocket bobLogin
  let msg = Cmd $ Whisper "Alice" "hi alice"
  mockState <- execStateT
    (do evaluateMessage s "Bob" msg aliceBobChannels) [aliceLogin, bobLogin]
  let str = getSocket aliceLogin mockState
  assertEqual "whisper across channels" ("W" ++ delim ++ "Bob: [random] hi alice\n") str)

testWhisperInChannel :: Test
testWhisperInChannel = TestCase (do
  let s = AbsSocket bobLogin
  let msg = Cmd $ Whisper "Alice" "hi alice"
  mockState <- execStateT
    (do evaluateMessage s "Bob" msg aliceBobState) [aliceLogin, bobLogin]
  let str = getSocket aliceLogin mockState
  assertEqual "whisper in same channel" ("W" ++ delim ++ "Bob: [General] hi alice\n") str)

testListUsers :: Test
testListUsers = TestCase (do
  let s = AbsSocket aliceLogin
  let msg = Cmd ListUsers
  mockState <- execStateT
    (do evaluateMessage s "Alice" msg aliceBobState) [aliceLogin, bobLogin]
  let str = getSocket aliceLogin mockState
  let expected = "S" ++ delim ++ "[Server]: Alice, Bob\n"
  assertEqual "list users" expected str)

testListChannels :: Test
testListChannels = TestCase (do
  let s = AbsSocket aliceLogin
  let msg = Cmd ListChannels
  mockState <- execStateT
    (do evaluateMessage s "Alice" msg aliceBobChannels) [aliceLogin, bobLogin]
  let str = getSocket aliceLogin mockState
  let expected = "S" ++ delim ++ "[Server]: General, random\n"
  assertEqual "list channels" expected str)

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
          testOneLogout, testTwoLogouts, testChangeChannel,
          testReturnToChannel, testChannelSend, testChannelMessageIsolation,
          testWhisperAcrossChannels, testWhisperInChannel,
          testListUsers, testListChannels ])
  return ()