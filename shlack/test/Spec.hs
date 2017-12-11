import ServerTesting
import ClientTesting

main :: IO ()
main = do
    putStrLn "Client tests"
    ClientTesting.main
    putStrLn "Server tests"
    ServerTesting.main
