{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module ClientTesting where

import Test.HUnit
import Model
import Client

-- Mini test suite for the parseInput function.
testParseInput :: Test
testParseInput = TestList [
    parseInput "This is a message" ~?=
        Just (TextData "This is a message"),
    parseInput "/join" ~?= Nothing,
    parseInput "/join channel1" ~?= Just (Cmd $ JoinChannel "channel1"),
    parseInput "/listchannels" ~?= Just (Cmd ListChannels),
    parseInput "/whisper john hey john" ~?=
        Just (Cmd (Whisper "john" "hey john")),
    parseInput "/whisper" ~?= Nothing,
    parseInput "/ignore john" ~?= Just (Cmd (Ignore "john")),
    parseInput "/help" ~?= Just (Cmd Help),
    parseInput "/help garbage extra text" ~?= Nothing]

-- Mini test suite for the serializeMessage function.
testSerializeMessage :: Test
testSerializeMessage = TestList [
    serializeMessage (Cmd $ JoinChannel "channel1") ~?= "Join,channel1",
    serializeMessage (TextData "pizza") ~?= "Message,pizza",
    serializeMessage (Login "alex") ~?= "Login,alex",
    serializeMessage (Cmd Disconnect) ~?= "Disconnect",
    serializeMessage (Cmd ListChannels) ~?= "ListChannels",
    serializeMessage (Cmd Help) ~?= "Help",
    serializeMessage (Cmd $ Ignore "alex") ~?= "Ignore,alex",
    serializeMessage (Cmd $ Whisper "alex" "hey dad") ~?= "Whisper,alex,hey dad"]

-- Entry point for testing.
main :: IO ()
main = do
  _ <- runTestTT (TestList [
          testParseInput, testSerializeMessage ])
  return ()