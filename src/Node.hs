module Node where
import Network.Socket

data Node = Node {
  myPort :: String,
  sockets :: [Socket]
  }
