{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module Client where

import Network
import Control.Concurrent
import Data.List.Split
import System.IO

import Model

-- Parses user input into a Message.
parseInput :: String -> Maybe Message
-- parseInput str = return $ Just $ TextData str
parseInput str =
    if length str == 0 then Nothing else
    if str == "logout" then Just $ Logout else
    let msg = str in
    let parts = splitOn " " msg in
    case parts of
        "/whisper" : p2 : rest -> Just $ Cmd $ Whisper p2 (unwords rest)
        p : [] -> case p of
            '/' : cmd -> case cmd of
                "listchannels" -> Just $ Cmd $ ListChannels
                "help" -> Just $ Cmd $ Help
                _ -> Nothing
            text -> Just $ TextData text
        p : ps ->
            case p of
                '/' : cmd -> case cmd of
                    "join" -> Just $ Cmd $ JoinChannel (concat ps)
                    "ignore" -> Just $ Cmd $ Ignore(concat ps)
                    _ -> Nothing
                _ -> Just $ TextData msg
        _ -> Nothing

    -- Use applicative parsing similar to HW06

-- Serializes messages into a friendly intermediate format to send to server.
serializeMessage :: Message -> String
serializeMessage msg = case msg of
    TextData str -> "Message," ++ str
    Login user -> "Login," ++ user
    Logout -> "Logout"
    Cmd cmd -> serializeCommand cmd

-- Serializes commands into a friendly intermediate format to send to server.
serializeCommand :: Command -> String
serializeCommand cmd = case cmd of 
    JoinChannel channel -> "Join," ++ channel
    Whisper user msg -> "Whisper," ++ user ++ "," ++ msg
    Ignore user -> "Ignore," ++ user
    ListChannels -> "ListChannels"
    Help -> "Help"

-- | IP address of the local host
local :: HostName
local = "127.0.0.1"

-- | Start the client given an IP address and a port. The port should
-- be a string number > 1024
client :: HostName -> PortID -> IO Handle
client = connectTo

clientLoop :: Handle -> IO ()
clientLoop sock = do
    input <- getLine
    maybeMsg <- return $ parseInput input
    case maybeMsg of
        Just msg -> 
            let serialMsg = serializeMessage msg in
            do
                hPutStr sock (serialMsg ++ "\n")
                hFlush sock
                clientLoop sock
        Nothing -> return ()


parseIP :: String -> String
parseIP ip = case ip of
    "" -> "192.168.1.83" 
    s -> s

readLoop :: Handle -> IO ()
readLoop sock = do
    line <- hGetLine sock
    putStrLn line
    readLoop sock

-- Main entry point for client.
main :: IO ()
main = do
    putStrLn "Enter the ip to connect to - newline for default"
    ip <- getLine
    -- sock <- client local (PortNumber 8080)
    putStrLn ("====== Connecting to: " ++ (parseIP ip) ++ " ======")
    sock <- client (parseIP ip) (PortNumber 4040)
    hSetBuffering sock LineBuffering
    putStrLn "Enter username"
    username <- getLine
    hPutStr sock (serializeMessage (Login username))
    hFlush sock
    putStrLn ("====== Logged in as: " ++ username ++ " ======")
    _ <- forkIO (readLoop sock)
    clientLoop sock

    -- Open socket to Server
    -- Loop:
        -- TODO make this double-threaded (using MVars)
        --  Read from socket
        --  Read from stdin
        --  Parse stdin
        --  Serialize message
        --- Send message over socket
