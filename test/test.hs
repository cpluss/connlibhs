module Main where

import ServerConnection
import Control.Monad.Reader

acceptClient :: Network ()
acceptClient = recvData >>= sendData >> acceptClient

main :: IO ()
main = server 8080 acceptClient
