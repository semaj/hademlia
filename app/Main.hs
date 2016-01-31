module Main where

import qualified Connection as C

import Network.Socket
import System.Environment
import System.IO
import Control.Monad
import Control.Exception

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

initialize :: String -> Maybe String -> Socket -> IO ()
initialize myPort maybeBootstrap mySocket = do
  initSockets <- timeToBootstrap my

iAmTheFirst :: String -> Socket -> IO ()
iAmTheFirst myPort mySocket = forever $ do
  s <- recv mySocket 1024
  putStrLn s

timeToBootstrap :: Socket -> Maybe String -> IO [Socket]
timeToBootstrap _ Nothing = return []
timeToBootstrap mySocket (Just bootstrap) = return [C.getSocket bootstrap]
