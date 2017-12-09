{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}
{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns #-}

module Main where

import Network
import Control.Concurrent
import Data.List.Split
import System.IO
import System.Console.ANSI

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
    TextData str -> "Message" ++ delim ++ str
    Login user -> "Login" ++ delim ++ user
    Logout -> "Logout"
    Cmd cmd -> serializeCommand cmd

-- Serializes commands into a friendly intermediate format to send to server.
serializeCommand :: Command -> String
serializeCommand cmd = case cmd of 
    JoinChannel channel -> "Join" ++ delim ++ channel
    Whisper user msg -> "Whisper" ++ delim ++ user ++ delim ++ msg
    Ignore user -> "Ignore" ++ delim ++ user
    ListChannels -> "ListChannels"
    Help -> "Help"

-- | IP address of the local host
local :: HostName
local = "127.0.0.1"

-- | Start the client given an IP address and a port. The port should
-- be a string number > 1024
client :: HostName -> PortID -> IO Handle
client = connectTo

actOnMessage msg sock user chnl = 
    case msg of
        Cmd (JoinChannel c) -> do
            chnlName <- takeMVar chnl
            putMVar chnl c
            writeDivider (Just c)
            clientLoop sock user chnl
        _ -> do
            chnlName <- takeMVar chnl
            putMVar chnl chnlName
            writeDivider (Just chnlName)
            clientLoop sock user chnl

updateState maybeMsg chnl = do
    case maybeMsg of
        Just (Cmd (JoinChannel chnlName)) ->
            putMVar chnl chnlName
        _ -> do
        chnlName <- takeMVar chnl
        putMVar chnl chnlName


clientLoop :: Handle -> String -> MVar String -> IO ()
clientLoop sock user chnl = do
    input <- getLine
    if input == "" then do
        scrollPageDown 1
        hFlush stdout
        clientLoop sock user chnl else do
    cursorUp 2
    hFlush stdout
    clearFromCursorToLineEnd
    hFlush stdout
    setSGR [SetColor Foreground Dull Cyan]
    putStr (user ++ ": " ++ input)
    hFlush stdout
    setSGR [SetColor Foreground Dull White]
    cursorDown 1
    hFlush stdout
    setCursorColumn 0
    hFlush stdout
    maybeMsg <- return $ parseInput input
    hFlush stdout
    case maybeMsg of
        Just msg ->
            let serialMsg = serializeMessage msg in
            do
                hPutStr sock serialMsg
                hFlush sock
                actOnMessage msg sock user chnl
        Nothing -> clientLoop sock user chnl


parseIP :: String -> String
parseIP ip = case ip of
    -- "" -> "192.168.1.190"
    "" -> "192.168.1.83" 
    s -> s

writeDivider :: Maybe String -> IO ()
writeDivider channel = 
    do
    setSGR [SetConsoleIntensity BoldIntensity]
    hFlush stdout
    setSGR [SetColor Foreground Dull Yellow]
    case channel of
        Nothing ->
            putStrLn "-----------------------------------"
        Just chnl ->
            let len = dividerLength - 2 - (length chnl) in
            putStrLn ("[" ++  chnl ++ "]" ++ (replicate len '-') )
    hFlush stdout
    setSGR [SetColor Foreground Dull White]
    hFlush stdout
    setSGR [SetConsoleIntensity NormalIntensity]
    hFlush stdout

readLoop :: Handle -> MVar String -> IO ()
readLoop sock chnl = do
    line <- hGetLine sock
    -- threadDelay 1000000
    setSGR [SetColor Foreground Dull Blue]
    cursorUp 1
    hFlush stdout
    -- threadDelay 1000000
    setCursorColumn 0
    hFlush stdout
    clearFromCursorToLineEnd
    hFlush stdout
    -- threadDelay 3000000
    -- threadDelay 1000000
    putStrLn line
    hFlush stdout
    chnlName <- takeMVar chnl
    putMVar chnl chnlName
    writeDivider (Just chnlName)
    readLoop sock chnl

printServerNotification :: String -> IO ()
printServerNotification str = do
    setSGR [SetColor Foreground Dull Green]
    hFlush stdout
    putStrLn ("[Server]: " ++ str)
    setSGR [SetColor Foreground Dull White]
    hFlush stdout

printPrompt :: String -> IO ()
printPrompt str = do
    setSGR [SetColor Foreground Dull Yellow]
    putStrLn str
    setSGR [SetColor Foreground Dull White]
    hFlush stdout

printWelcomeMessage :: String -> IO ()
printWelcomeMessage user = do
    printServerNotification ("hey " ++ user ++ " welcome to Shlλck!")
    putStrLn "          ,▄▄,                 ▄▄        ▄▄  ▄▄▄               ▄▄"
    putStrLn "    ,▄█▓▓▓▓▓▓▓▓▓▄              ▓▓        ▓▓    ▓▓              ▓▓ "
    putStrLn "   ▓▌,  ▀▓▓▓▓▓▓▓▓▌     ▄▄▓▓▄▄  ▓▓▄▄▓▓▄   ▓▓    ▐▓▓     ,▄▓▓▓▄▄ ▓▓   ▄▓▄"
    putStrLn "  |▓▓▓▓▓   ▓▓▓▓▓▓▓▄   ▐▓▓   ▀  ▓▓▀   ▓▓  ▓▓    ▓▓▓▌   ▓▓▓▀  ▀  ▓▓▄▄▓▓▀"
    putStrLn "  ▐▓▓▓▓▓▌ ▄  ▀▓▓▓▓▓    ▀█▓▓▓▄  ▓▓    ▓▓  ▓▓   ▓▓▀▀▓▄  ▓▓       ▓▓▓▓▓▌"
    putStrLn "   ▓▓▓▓▓▌ |▓▄   ▓▓▓    ▄  ▓▓▓  ▓▓    ▓▓  ▓▓  ▓▓▌  ▓▓▄ ▀▓▓▄__▄, ▓▓▌ ▀▓▓▄"
    putStrLn "    ▓▓▓▓▌ ▐▓▓▓▓▓▓▓`    ▀▀▀▀▀▀  ▀▀    ▀▀  ▀▀  ▀▀    ▀▀▀ '▀▀▀▀▀  ▀▀    ▀▀`"
    putStrLn "     ▀▓▓▓▓▓▓▓▓▀▀ "
    putStrLn ""
    writeDivider Nothing
-- Main entry point for client.
main :: IO ()
main = do
    printPrompt "Enter server IP"
    ip <- getLine
    printServerNotification ("connecting to: " ++ (parseIP ip))
    hFlush stdout
    sock <- client (parseIP ip) (PortNumber 4040)
    hSetBuffering sock LineBuffering
    printPrompt "Enter username"
    username <- getLine
    printWelcomeMessage username
    hPutStr sock (serializeMessage (Login username))
    hFlush sock
    chnlState <- newMVar defaultChannel
    _ <- forkIO (readLoop sock chnlState)
    writeDivider (Just defaultChannel)
    clientLoop sock username chnlState

    -- Open socket to Server
    -- Loop:
        -- TODO make this double-threaded (using MVars)
        --  Read from socket
        --  Read from stdin
        --  Parse stdin
        --  Serialize message
        --- Send message over socket
