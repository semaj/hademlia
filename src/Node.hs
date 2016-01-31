module Node where
import Network.Socket

data Node = Node {
  myPort :: String,
  sockets :: [Socket]
  }

initNode :: String -> [Socket] -> Node
initNode port sockets = Node port sockets
