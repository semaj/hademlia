module Node where
import Network.Socket

type ID = String

data Node = Node {
  myPort :: String,
  sockets :: [Socket]
  }

initNode :: String -> [Socket] -> Node
initNode port sockets = Node port sockets
