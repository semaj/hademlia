module Real where
import qualified Connection as C
import qualified Constants
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import qualified Data.HashMap.Strict as HM
import qualified Message as M
import           Network.Socket

start :: String -> Maybe String -> IO ()
start port bootstrap =
  withSocketsDo $ bracket (C.startServer port) sClose (initialize port bootstrap)

  -- fork off another thread to prune the sockets tvar

reader :: Socket -> TVar (HM.HashMap String Socket) -> TQueue M.Message -> IO ()
reader sock socketsBox q = forever $ do
  message <- recv sock Constants.socketReadSize
  -- atomically add to sockets
  putStrLn message
  let deserialized = M.deserialize message
  atomically $ writeTQueue q deserialized

writer :: Socket -> TVar (HM.HashMap String Socket) -> TQueue M.Message -> IO ()
writer sock socketsBox q = forever $ do
  message <- atomically $ readTQueue q
  sockets <- atomically $ readTVar socketsBox
  let serialized = M.serialize message
  void $ mapM (\ s -> send s serialized) sockets

initialize :: String -> Maybe String -> Socket -> IO ()
initialize myPort maybeBootstrap mySocket = do
  initSockets <- timeToBootstrap maybeBootstrap
  sockets <- atomically $ newTVar HM.empty
  readQ <- atomically $ newTQueue
  writeQ <- atomically $ newTQueue
  writing <- forkIO $ writer mySocket sockets writeQ
  reading <- forkIO $ reader mySocket sockets readQ
  forever (threadDelay (10^8))

timeToBootstrap :: Maybe String -> IO [Socket]
timeToBootstrap Nothing = return []
timeToBootstrap (Just bootstrap) = do
  s <- C.getSocket bootstrap
  send s "X"
  return [s]
