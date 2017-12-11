module Main where

import ServerTesting
import ClientTesting

main :: IO ()
main = do
    putStrLn "Client tests"
    ClientTesting.clientTestingMain
    putStrLn "Server tests"
    ServerTesting.serverTestingMain
