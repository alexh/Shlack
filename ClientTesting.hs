{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module ClientTesting where

import Test.HUnit
import Model
import Client

testParseInput :: Test
testParseInput = TestList [
    parseInput "This is a message\n" ~?=
        Just (TextData "This is a message"),
    parseInput "/join\n" ~?= Nothing,
    parseInput "/join channel1\n" ~?= Just (Cmd $ JoinChannel "channel1"),
    parseInput "/listchannels\n" ~?= Just (Cmd ListChannels),
    parseInput "/whisper john hey john\n" ~?=
        Just (Cmd (Whisper "john" "hey john")),
    parseInput "/whisper\n" ~?= Nothing,
    parseInput "/ignore john\n" ~?= Just (Cmd (Ignore "john")),
    parseInput "/help\n" ~?= Just (Cmd Help),
    parseInput "/help garbage extra text\n" ~?= Nothing]

testSerializeMessage :: Test
testSerializeMessage = TestList [
    serializeMessage (Cmd $ JoinChannel "channel1") ~?= "Join,channel1",
    serializeMessage (TextData "pizza") ~?= "Message,pizza",
    serializeMessage (LogIn "alex") ~?= "Login,alex",
    serializeMessage (Cmd Disconnect) ~?= "Disconnect",
    serializeMessage (Cmd ListChannels) ~?= "ListChannels",
    serializeMessage (Cmd Help) ~?= "Help",
    serializeMessage (Cmd $ Ignore "alex") ~?= "Ignore,alex",
    serializeMessage (Cmd $ Whisper "alex" "hey dad") ~?= "Whisper,alex,hey dad"]