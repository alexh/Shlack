import Test.HUnit
import Client

testParseInput :: Test
testParseInput = TestList [
    ]

testSerializeMessage :: Test
testSerializeMessage = TestList [
    serializeMessage (JoinRoom 1) ~?= "Join,1",
    serializeMessage (JoinRoom 232) ~?= "Join,232"]