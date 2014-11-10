module Main where

import ServerConnection
import Control.Monad.Reader

acceptClient :: Network ()
acceptClient = do
    sendData "Hello there!"
    liftIO $ putStrLn "Connection made..."

main :: IO ()
main = server 8081 acceptClient
