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
injectSocketData = undefined

emptyState :: ServerState AbstractSocket
emptyState = ServerState {socketToUser = [], 
                          userToSocket = M.empty, 
                          channelToUser = M.empty, 
                          userToChannel = M.empty}

aliceLogin :: AbstractSocket 
aliceLogin = AbstractSocket { getAbstractSocket = 1,
                              getSocketData = "Login" ++ delim ++ "Alice" }

bobLogin :: AbstractSocket 
bobLogin = AbstractSocket { getAbstractSocket = 2,
                            getSocketData = "Login" ++ delim ++ "Bob" }

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

-- Example of a test suite for server actions.
-- TODO, how do you use do notation in a unit test?
  -- Define a runTestingServer to run our monadic action (the stateful socket one)
  -- Use runStateT
testOneLogin :: Test
testOneLogin = TestCase (do
  let s = AbsSocket aliceLogin
  msg <- readFrom s
  newState <- evaluateMessage s "" msg emptyState
  let expectedState = ServerState {socketToUser = [(s, "Alice")],
                              userToSocket = M.insert "Alice" s M.empty,
                              channelToUser = M.insert "General" ["Alice"] M.empty,
                              userToChannel = M.insert "Alice" "General" M.empty}
  assertEqual "one login test" expectedState newState)

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
          testParseMessage, testOneLogin])
  return ()