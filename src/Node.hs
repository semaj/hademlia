module Node where
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import           Network.Socket
import           RoutingData

data Node = Node { port :: B.ByteString
                 , id :: ID
                 , tree :: Tree
                 , store :: HM.HashMap B.ByteString B.ByteString
                 , sockets :: [Socket]
                 }
