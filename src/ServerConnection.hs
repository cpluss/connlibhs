module ServerConnection (server, 
                         Network, 
                         ConnectionLoop, 
                         ConnectionData, 
                         connHandle,
                         sendData,
                         recvData) 
    where

import Network.Socket
import System.IO
import Control.Monad.Reader
import Control.Exception
import Control.Concurrent
import Control.Monad

-- | Contains all connectiondata that should be available in order to perform
--   network operations.
newtype ConnectionData = ConnectionData { connHandle :: Handle }

-- | Contains all server data
newtype Server = Server { sock :: Socket }

-- | The function type of the function that will handle clients (defined by user)
type ConnectionLoop = Network ()

-- | Network type declaration for a socket reader with support for
--   IO.
type Network = ReaderT ConnectionData IO

-- | Server, starts a server using the information provided.
server :: PortNumber -> ConnectionLoop -> IO ()
server port handler = do
    -- Open a socket on the specified port
    srv <- openSocket port
    -- Listen on the port for incoming connections
    listenSocket srv
    -- Jump to the server main loop
    serverLoop srv handler


-- Open a new socket on this specific port
openSocket :: PortNumber -> IO Server
openSocket port = do
    -- Create a new socket 
    s <- socket AF_INET Stream 0
    -- Used when debugging, in order to restart often
    setSocketOption s ReuseAddr 1
    -- Bind the socket to the assigned port
    bindSocket s $ SockAddrInet port iNADDR_ANY
    return $ Server s

-- Listen on a specific socket for incoming connections
listenSocket :: Server -> IO ()
listenSocket = flip listen 25 . sock

-- The main server loop
serverLoop :: Server -> ConnectionLoop -> IO ()
serverLoop srv handler = do
    -- Accept an incoming connection
    conn <- accept $ sock srv
    -- Fork it into a new connection thread
    forkIO $ trampoline conn handler
    -- next
    serverLoop srv handler

-- Trampoline to the user defined connection handler
trampoline (s, _) handler = do
    -- Get a handle to the new connection
    h <- socketToHandle s ReadWriteMode
    hSetBuffering h NoBuffering

    -- Give control to the user
    runReaderT handler $ ConnectionData h

    -- Clean this mess up
    hClose h

-- | Send data to the connection
sendData :: String -> Network ()
sendData s = asks connHandle >>= liftIO . flip hPutStrLn s

-- | Receive data from the connection
recvData :: Network String
recvData = asks connHandle >>= liftIO . liftM init . hGetLine
