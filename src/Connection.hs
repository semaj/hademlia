module Connection where
import Network.Socket

startServer :: String -> IO Socket
startServer port = do
  (serveraddr:_) <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                                Nothing (Just port)
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bindSocket sock (addrAddress serveraddr) >> return sock

getSocket :: String -> IO Socket
getSocket port = do
  (serveraddr:_) <- getAddrInfo Nothing (Just "0.0.0.0") (Just port)
  s <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect s (addrAddress serveraddr) >> return s
