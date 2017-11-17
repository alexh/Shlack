{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module Main where

import Network.Socket
import System.IO
import Model

-- Parses String recieved from Socket into a Message
parseMessage :: String -> Message
parseMessage = undefined




-- Main entry point for server.
main :: IO ()
main = undefined