module Main where

import qualified Connection as C
import qualified Node as N
import qualified Constants
import qualified Message as M

import qualified Data.HashMap.Strict as HM

import Network.Socket
import System.Environment
import System.IO
import Control.Monad
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  args <- getArgs
  let port = head args
      bootstrap = if length args == 1 then Nothing else Just (args!!1)
  withSocketsDo $ bracket (startServer port) sClose (initialize port bootstrap)

startServer :: String -> IO Socket
startServer port = do
  (serveraddr:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                Nothing (Just port)
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bindSocket sock (addrAddress serveraddr) >> return sock

-- fork off another thread to prune the sockets tvar

reader :: Socket -> TVar (HM.HashMap String Socket) -> TQueue M.Message -> IO ()
reader socket socketsBox q = forever $ do
  message <- recv socket Constants.socketReadSize
  -- atomically add to sockets
  let deserialized = M.deserialize message
  atomically $ writeTQueue q deserialized

writer :: Socket -> TVar (HM.HashMap String Socket) -> TQueue M.Message -> IO ()
writer socket socketsBox q = forever $ do
  message <- atomically $ readTQueue q
  -- sockets <- atomically $ readTVar socketsBox
  let serialized = M.serialize message
  void $ send socket serialized

initialize :: String -> Maybe String -> Socket -> IO ()
initialize myPort maybeBootstrap mySocket = do
  initSockets <- timeToBootstrap maybeBootstrap
  sockets <- atomically $ newTVar HM.empty
  readQ <- atomically $ newTQueue
  writeQ <- atomically $ newTQueue
  writing <- forkIO $ writer mySocket sockets writeQ
  reading <- forkIO $ reader mySocket sockets readQ
  return ()

timeToBootstrap :: Maybe String -> IO [Socket]
timeToBootstrap Nothing = return []
timeToBootstrap (Just bootstrap) = do
  s <- C.getSocket bootstrap
  return [s]
