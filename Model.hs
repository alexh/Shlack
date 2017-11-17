{-# LANGUAGE
  TypeFamilies, FlexibleContexts
#-}

module Model where

import Network.Socket
import System.IO


data Message = JoinRoom Int
             | TextData String
             | LogIn String
             | Disconnect

-- Main entry point for client.
-- main :: IO ()
-- main = undefined