module Connection where
import Network.Socket

getSocket :: String -> IO Socket
getSocket port = do
  (serveraddr:_) <- getAddrInfo Nothing (Just "0.0.0.0") (Just port)
  s <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect s (addrAddress serveraddr) >> return s
